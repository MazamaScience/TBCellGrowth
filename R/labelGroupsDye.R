#' @export
#' @title Identify and Label Florescent Dye
#' @param image an image matrix to search for dye
#' @param artifactMask a mask of non biological features to ignore. See \link{createArtifactMask}.
#' @description Searches an image for dark cell colonies and incrementally labels each blob.
#' @return A \code{matrix} of integer labeled blobs.

labelGroupsDye <- function(image, artifactMask) {

  print("Searching new image...")
  ptm <- proc.time()
  
  # Threshold
  imageEdit <- image > 0.3
  
  # Dilate groups slightly too large for more inclusive labeling
  imageEdit <- EBImage::dilateGreyScale(imageEdit, makeBrush(9, shape="disc"))
  imageEdit <- EBImage::erodeGreyScale(imageEdit, makeBrush(5, shape="disc"))
  
  # Mask out artifacts
  imageEdit[artifactMask>0] <- 0
  
  # Label blobs
  imageEdit <- EBImage::bwlabel(imageEdit)
  
  # Shrink blobs to a more accurage size
  imageEdit[image < 0.3] <- 0
  
  return(imageEdit)
   
}
