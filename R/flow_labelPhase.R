#' @export
#' @title Identify and Label Phase Microscopy Groups
#' @param image an image matrix to search for cell colonies
#' @param artifactMask a mask of non biological features to ignore. See \link{flow_createArtifactMask}.
#' @param ignore a vector of row numbers to ignore. Blobs which have centroids
#' in this range are removed.
#' @description Searches an image for dark cell colonies and incrementally labels each colony.
#' @return A \code{matrix} of integer labeled blobs.

flow_labelPhase <- function(image, artifactMask, ignore) {
  
  ptm <- proc.time()
  print("Searching new image...")
  
  imageMask <- image
  imageMask[imageMask > 1] <- 1
  
  imageMask[artifactMask > 0] <- quantile(image)[[4]]
  
  imageEdit <- filter_sobel(imageMask)
  
  imageEdit <- imageEdit > 0.5
  
  imageEdit[EBImage::equalize(imageMask) > 0.5] <- 0
  
  imageEdit <- EBImage::closingGreyScale(imageEdit, EBImage::makeBrush(7))
  
  imageEdit <- EBImage::dilateGreyScale(imageEdit, EBImage::makeBrush(5))
  
  imageEdit <- EBImage::fillHull(imageEdit)
  
  imageEdit[EBImage::equalize(imageMask) > 0.4] <- 0
  
#   imageEdit <- dilateGreyScale(imageEdit, EBImage::makeBrush(5))
  
  imageEdit <- removeBlobs(imageEdit, 50)
  
  imageEdit <- EBImage::bwlabel(imageEdit)
  
  centroids <- getCentroids(imageEdit)
  toRemove <- removeIgnored(centroids, ignore)
  imageEdit[!(imageEdit %in% toRemove)] <- 0

  imageEdit <- EBImage::bwlabel(imageEdit)
  
  imageEdit[EBImage::equalize(imageMask) > 0.3] <- 0

#   imageEdit <- EBImage::fillHull(imageEdit)
  
  return(imageEdit)
  
}

removeIgnored <- function(df, ignore) {
  remove <- apply(ignore, 1, function(ig) (df$x > ig[[1]]) & (df$x < ig[[2]]) & (df$y > ig[[3]]) & (df$y < ig[[4]]))
  remove <- apply(remove, 1, function(x) sum(x) < 1)
  return(df[remove,]$index)
} 

