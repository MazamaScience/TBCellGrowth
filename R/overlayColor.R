#' @export
#' @title Adds a Colored Overlay to an Image
#' @param color a color string, i.e. "green" or "phase"
#' @param bg a background image
#' @param label a binary image label
#' @description Adds a colored overlay to a background image. When passed "phase" 
#' for color uses grey by default
#' @return a color Image object

overlayColor <- function(color, bg, label) {
  
  colors <- list(green = c(0,0.5,0.1),
                 red = c(0.5,0,0),
                 orange = c(0.5,0.25,0),
                 phase = c(0.7,0.7,0.7))
  
  color <- colors[color][[1]]
  
  # Stack three layers of phase to make a 3d matrix
  stack <- simplify2array(list(bg,bg,bg))
  
  # With colormode=Color, the 3 matrix layers are interpreted as RBG channels
  image <- EBImage::Image(stack, colormode="Color") 
  
  # red, green, blue channels
  image[,,1] <- bg + (label) * color[[1]]
  image[,,2] <- bg + (label) * color[[2]]
  image[,,3] <- bg + (label) * color[[3]]
  
  return(image)
  
}