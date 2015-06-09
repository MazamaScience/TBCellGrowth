#' @export
#' @title Overlay Dye Colors on an Image
#' @param phase a preprocessed phase image
#' @param green a labeled green dye image
#' @param red a labeled red dye image
#' @description Overlays green and red labeled dye images on a phase image.
#' @return a color Image object

overlayDyes <- function(phase, green, red) {
  
  # Stack three layers of phase to make a 3d matrix
  stack <- simplify2array(list(phase,phase,phase))
  
  # With colormode=Color, the 3 matrix layers are interpreted as RBG channels
  image <- EBImage::Image(stack, colormode="Color") 
  
  # Remove stack from memory
  rm(stack)
  
  # Add red dye to red channel
  image[,,1] <- phase + (red > 0 )
  
  # Add green dye to green and blue channel (for a nice green)
  image[,,2] <- phase + (green > 0 ) / 2
  image[,,3] <- phase + (green > 0 ) / 5
  
  return(image)
  
}