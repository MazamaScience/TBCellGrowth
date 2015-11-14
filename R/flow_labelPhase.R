#' @export
#' @title Identify and Label Phase Microscopy Colonies
#' @param image an image matrix to search for cell colonies
#' @param artifactMask a mask of non biological features to ignore. See \link{flow_createArtifactMask}.
#' @param ignoredRegions a vector of row numbers to ignore. Blobs which have centroids
#' in this range are removed.
#' @param minBlobSize minimum size for a colony blob to be retained (pixels)
#' @description Searches an image for dark cell colonies and uses EBImage::bwlabel
#' to assign squential numeric labels to the pixels of each identified colony.
#' @return A \code{matrix} of integer labeled blobs.

flow_labelPhase <- function(image, artifactMask, ignoredRegions, minBlobSize=100) {
  
  # TODO:  Set all hardcoded values here at the beginning of the function
  # TODO:  so that we can easily modify or promote to an argument if needed.
  
  # Normalize to 0:1 by setting bright regions to 1
  image[image > 1] <- 1
  
  # Save the normalized image for later use in equalizing
  imageMask <- image
  
  # Set artifacts to medium gray 
  imageMask[artifactMask > 0] <- quantile(image, 0.55)
  ###imageMask[artifactMask > 0] <- quantile(image, seq(0,1,0.05), na.rm=T)[12]
  
  # Edge detect and fill in holes (closing performs dilate/erode)
  imageEdit <- filter_sobel(imageMask, FALSE, 2)
  imageEdit <- EBImage::closingGreyScale(imageEdit, EBImage::makeBrush(7))
  
  # Mask out everything below 50%
  imageEdit <- imageEdit > 0.5
  
  # imageEdit <- fillHull(imageEdit)
  
  imageEdit[EBImage::equalize(imageMask) > 0.8] <- 0
  
  imageEdit <- EBImage::dilateGreyScale(imageEdit, EBImage::makeBrush(3))
  
  imageEdit[EBImage::dilateGreyScale(artifactMask, EBImage::makeBrush(3)) > 0] <- 0
  
  # Remove blobs (colonies) smaller than minBlobSize
  imageEdit <- removeBlobs(imageEdit, minBlobSize)
  
  # Sequentially assign each blob a numeric identifier
  imageEdit <- EBImage::bwlabel(imageEdit)
  
  # Calculate location information associated with each colony
  # (x, y, xmin, xmax, ymin, ymax, size, id, index)
  centroids <- getCentroids(imageEdit)
  
  # TODO:  Place removeIgnored logic here in the main function and check why we have an error
  # TODO:  for xy10 in this .sbatch script on the SLURM cluster:
  
#   --inputDir="/nearline/sherman-ngs/KM_Temp_Imaging/Microfluidics/BSL3/CellAsic, RvC & pLC372 Phage Delivery of GFP, 11-3-15" \
#   --dataDir="Experimental Images" \
#   --backgroundIndex=1 \
#   --outputDir="/nearline/sherman-ngs/JC_Results/Nov03_phaseOnly" \
#   --chambers="xy01,xy04,xy07,xy10" \
#   --channels="c1" \
#   --channelNames="phase1" \
#   --startFrame=1 \
#   --distanceScale=0.21 \
#   --timestep=2 \
#   --verbose
  
  toRemove <- removeIgnored(centroids, ignoredRegions)
  
  imageEdit[!(imageEdit %in% toRemove)] <- 0
  
  imageEdit[EBImage::equalize(imageMask) > 0.775] <- 0
  
  # Again remove blobs smaller than a new threshold
  imageEdit <- removeBlobs(imageEdit, 175, label=FALSE)
  
  return(imageEdit)
  
}

removeIgnored <- function(df, ignoredRegions) {
  remove <- apply(ignoredRegions, 1, function(ig) (df$x > ig[[1]]) & (df$x < ig[[2]]) & (df$y > ig[[3]]) & (df$y < ig[[4]]))
  remove <- apply(remove, 1, function(x) sum(x) < 1)
  return(df[remove,]$index)
} 

