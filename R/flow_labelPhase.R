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
  
  imageMask[artifactMask] <- quantile(image)[[4]]
  
  imageEdit <- sobelFilter(imageMask)
  
  imageEdit <- imageEdit > 0.5
  
  imageEdit[imageMask > 0.5] <- 0
  
  imageEdit <- closingGreyScale(imageEdit, EBImage::makeBrush(9))
  
  imageEdit <- EBImage::fillHull(imageEdit)
  
  imageEdit <- dilateGreyScale(imageEdit, EBImage::makeBrush(7))
  
  imageEdit[imageMask > 0.4] <- 0
  
  imageEdit <- dilateGreyScale(imageEdit, EBImage::makeBrush(5))
  
  imageEdit <- removeBlobs(imageEdit, 150)
  
  imageEdit <- EBImage::bwlabel(imageEdit)
  
  for (i in 1:max(imageEdit)) {
    ind <- which(i == imageEdit, arr.ind=TRUE)
    xx <- mean(ind[,1])
    yy <- mean(ind[,2])
    for (j in 1:dim(ignore)[1]) {
      ig <- ignore[j,]
      if(xx >= ig[[1]] & xx <= ig[[2]] & yy >= ig[[3]] & yy <= ig[[4]]) {
        imageEdit[imageEdit == i] <- 0
      }
    }
  }
  
  imageEdit <- EBImage::bwlabel(imageEdit)
  
  imageEdit[imageMask > 0.35] <- 0
  
  return(imageEdit)
  
}
