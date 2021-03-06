#' @export
#' @title Blur an Image
#' @param image an image matrix
#' @param radius number of pixels to blur. Must be an odd number.
#' @description Blurs an image using the EBImage makeBrush and filter2
#' functions.
#' @return An image of the same dimensions.

filter_blur <- function(image, radius=3) {
  save <- image
  image[is.na(image)] <- 0
  flo <- EBImage::makeBrush(radius, shape='disc', step=FALSE)^2
  flo <- flo/sum(flo)
  image <- EBImage::filter2(image,flo)
  image[is.na(image)] <- 0
  image[is.na(save)] <- NA
  return(image)
}