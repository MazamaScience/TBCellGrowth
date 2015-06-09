#' @export
#' @title Identify and label Green Dye Groups
#' @param image an image matrix to search for green dye
#' @param artifactMask a mask of non biological features to ignore. See \link{createArtifactMask}.
#' @description Searches an image for dark cell colonies and incrementally labels each colony.
#' @return A \code{matrix} of integer labeled blobs.

labelGroupsGreen <- function(image, artifactMask) {

  print("Searching new image...")
  ptm <- proc.time()
  
  # Threshold
  imageEdit <- image > 0.3
  
  # Dilate groups slightly too large for more inclusive labeling
  imageEdit <- dilateGreyScale(imageEdit, makeBrush(9, shape="disc"))
  imageEdit <- erodeGreyScale(imageEdit, makeBrush(5, shape="disc"))
  
  # Mask out artifacts
  imageEdit[artifactMask>0] <- 0
  
  # Label blobs
  imageEdit <- bwlabel(imageEdit)
  
  # Shrink blobs to a more accurage size
  imageEdit[image < 0.3] <- 0
  
  return(imageEdit)
   
}
