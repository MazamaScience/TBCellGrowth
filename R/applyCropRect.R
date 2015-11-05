#' @export
#' @title Crop an Image
#' @param image an image matrix
#' @param width twice the individual width border
#' @param height twice the individual height border
#' @description Removes border pixels from an image.
#' @return Cropped image.

applyCropRect <- function(image, width, height) {
  
  dimx <- dim(image)[[1]]
  dimy <- dim(image)[[2]]
  
  x1 <- floor(dimx/2) - floor(width/2)
  x2 <- floor(dimx/2) + floor(width/2)
  y1 <- floor(dimy/2) - floor(height/2)
  y2 <- floor(dimy/2) + floor(height/2)
     
  return(image[x1:x2,y1:y2])
  
}