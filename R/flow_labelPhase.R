#' @export
#' @title Identify and Label Phase Microscopy Groups
#' @param image an image matrix to search for cell colonies
#' @param artifactMask a mask of non biological features to ignore. See \link{flow_createArtifactMask}.
#' @param ignoredRegions a vector of row numbers to ignore. Blobs which have centroids
#' in this range are removed.
#' @description Searches an image for dark cell colonies and incrementally labels each colony.
#' @return A \code{matrix} of integer labeled blobs.

flow_labelPhase <- function(image, artifactMask, ignoredRegions) {
  
  image[image > 1] <- 1
  
  imageMask <- image
  
  imageMask[artifactMask > 0] <- quantile(image, seq(0,1,0.05), na.rm=T)[[12]]
  
  imageEdit <- filter_sobel(imageMask, FALSE, 2)
  
  imageEdit <- EBImage::closingGreyScale(imageEdit, EBImage::makeBrush(7))
  
  imageEdit <- imageEdit > 0.5
  
  # imageEdit <- fillHull(imageEdit)
  
  imageEdit[EBImage::equalize(imageMask) > 0.8] <- 0
  
  imageEdit <- EBImage::dilateGreyScale(imageEdit, EBImage::makeBrush(3))
  
  imageEdit[EBImage::dilateGreyScale(artifactMask, EBImage::makeBrush(3)) > 0] <- 0
  
  imageEdit <- removeBlobs(imageEdit, 100)
  
  imageEdit <- EBImage::bwlabel(imageEdit)
  
  centroids <- getCentroids(imageEdit)
  toRemove <- removeIgnored(centroids, ignoredRegions)
  imageEdit[!(imageEdit %in% toRemove)] <- 0
  
  imageEdit[EBImage::equalize(imageMask) > 0.775] <- 0
  
  imageEdit <- removeBlobs(imageEdit, 175, label=FALSE)
  
  return(imageEdit)
  
}

removeIgnored <- function(df, ignoredRegions) {
  remove <- apply(ignoredRegions, 1, function(ig) (df$x > ig[[1]]) & (df$x < ig[[2]]) & (df$y > ig[[3]]) & (df$y < ig[[4]]))
  remove <- apply(remove, 1, function(x) sum(x) < 1)
  return(df[remove,]$index)
} 

