#' @export
#' @title Identify and Label Florescent Dye
#' @param image an image matrix to search for dye
#' @param phase.labeled a labeled phase image from \link{flow_labelPhase}
#' @param labelingThreshold threshold value for detection [0-1]
#' @description Searches an image for dark cell colonies and incrementally labels each blob.
#' @return A \code{matrix} of integer labeled blobs.

flow_labelDye <- function(image, phase.labeled, labelingThreshold=0.9) {
  
  # Mask out portions of the dye image that are not associated with a phase.labeled blob
  image[phase.labeled < 1] <- 0
  
  # Threshold
  image <- image > labelingThreshold
  
  # Dilate groups slightly too large for more inclusive labeling
  image <- EBImage::dilateGreyScale(image, EBImage::makeBrush(7, shape="disc"))
  image <- EBImage::erodeGreyScale(image, EBImage::makeBrush(5, shape="disc"))
  
  # Create dye labels
  dye.labeled <- EBImage::bwlabel(image)
  
  # Use the labeled dye colonies to create a mask for the phase.labeled image
  phase.labeled[dye.labeled < 1] <- 0

  return(phase.labeled)
   
}
