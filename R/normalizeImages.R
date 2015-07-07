#' @export
#' @title Normalize Image Values
#' @param image the image matrix to modify.
#' @param medianNew a new median value to set scale the image values against
#' @description Normalize a given image matrix based on median. The new median
#'  value should be between 0 and 1, and will typically be between 0.2 and 0.6.
#' @return an image of the same dimensions.

normalizeImages <- function(image, medianNew) {
  
  # Set darkest value to 0
  image <- image - min(image)
  # Scale image values so median matched new median
  image <- image * (medianNew / median(image))
  return(image)
  
}