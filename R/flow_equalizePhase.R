#' @export
#' @title Normalize Image Values
#' @param image the image matrix to modify.
#' @param medianNew a new median value to set scale the image values against
#' @description Normalize a given image matrix based on median. The new median
#'  value should be between 0 and 1, and will typically be between 0.2 and 0.6.
#' @return an image of the same dimensions.

flow_equalizePhase <- function(image, medianNew) {
  
  # Set darkest value to 0
  imageMin <- min(image)
  image <- image - imageMin
  
  # Scale image values so median matches new median
  scaling <- medianNew / median(image)
  image <- image * scaling
  
  if (getRunOptions('verbose')) {
    cat(paste0('\timageMin = ',round(imageMin,3),', scaling = ',round(scaling,3),'\n'))
  }
  
  return(image)
  
}