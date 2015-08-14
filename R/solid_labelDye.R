#' @export
#' @title Identify and Label Florescent Dye
#' @param image an image matrix to search for dye
#' @param phase.labeled a list of labeled phase images from
#' \link{flow_labelPhase}
#' @description Searches an image for dark cell colonies and incrementally labels each blob.
#' @return A \code{matrix} of integer labeled blobs.

solid_labelDye <- function(image, phase.labeled) {
  image <- filter_blur(image,9)
  image <- filter_sobel(image)
  image[phase.labeled < 1] <- 0
  image <- EBImage::dilateGreyScale(image, kern=EBImage::makeBrush(13,'disc'))
  image <- EBImage::erodeGreyScale(image, kern=EBImage::makeBrush(15,'disc'))
  image <- image > 0.5
  phase.labeled[!image] <- 0
  return(phase.labeled)
}


