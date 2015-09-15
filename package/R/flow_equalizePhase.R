#' @export
#' @title Normalize Image Values
#' @param image the image matrix to modify.
#' @param medianNew a new median value to set scale the image values against
#' @description Normalize a given image matrix based on median. The new median
#'  value should be between 0 and 1, and will typically be between 0.2 and 0.6.
#' @return an image of the same dimensions.

flow_equalizePhase <- function(image, medianNew) {
  
  cat("\n")
  
  cat("a=")
  cat(round(min(image),3))
  
  # Set darkest value to 0
  image <- image - min(image)
  
  cat(", b=")
  cat(round(medianNew/median(image),3))
  
  # Scale image values so median matched new median
  image <- image * (medianNew / median(image))
  
  return(image)
  
}