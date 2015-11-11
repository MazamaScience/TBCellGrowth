#' @export
#' @title Normalize Dye Image Values
#' @param image the image matrix to modify.
#' @description Normalize a given dye image. TODO More info here
#' @return an image of the same dimensions.
#' 
#TODO how do we know if a frame isn't useful? Too dark?
solid_equalizeDye <- function(image) {
  
  dyeMedian <- getRunOptions('dyeMedian')
  
  # Get rid of noise by blurring
  image <- filter_blur(image,7)
  
  # Center and rescale the range of values
  image <- image * (dyeMedian / median(image,na.rm=TRUE))
  image <- ((image-0.5)*6)^4 # Ad hoc scaling factor
  
  return(image)
  
}


