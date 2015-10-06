#' @export
#' @title Identify and Label Florescent Dye
#' @param image an image matrix to search for dye
#' @param phase.labeled a list of labeled phase images from
#' \link{flow_labelPhase}
#' @param artifactMask a mask of non biological features to ignore. See \link{flow_createArtifactMask}.
#' @description Searches an image for dark cell colonies and incrementally labels each blob.
#' @return A \code{matrix} of integer labeled blobs.

flow_labelDye <- function(image, phase.labeled, artifactMask) {
  
  test = filter_sobel(image)
  
  imageEdit <- image

  cat(".")
  
  imageEdit <- image
  imageEdit[phase.labeled<1] <- 0
  
  # Threshold
  imageEdit <- imageEdit > 0.9
  
  # Dilate groups slightly too large for more inclusive labeling
  imageEdit <- EBImage::dilateGreyScale(imageEdit, EBImage::makeBrush(7, shape="disc"))
  imageEdit <- EBImage::erodeGreyScale(imageEdit, EBImage::makeBrush(5, shape="disc"))
  
  # Now we have dye labels
  imageEdit <- EBImage::bwlabel(imageEdit)
  
  phase.labeled[imageEdit < 1] <- 0
  
  return(phase.labeled)
   
}
