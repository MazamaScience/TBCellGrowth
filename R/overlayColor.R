#' @export
#' @title Adds a Colored Overlay to an Image
#' @param color a color string, i.e. "green" or "phase"
#' @param bg a background image
#' @param label a binary image label
#' @param mergeWith an equally sized color image to overlay the new color on. Uses background
#' image as default
#' @description Adds a colored overlay to a background image. When passed "phase" 
#' for color uses grey by default
#' @return a color Image object

overlayColor <- function(color, bg, label, mergeWith=NULL) {
  
  colors <- list(green = c(0,0.5,0.1),
                 red = c(0.5,0,0),
                 orange = c(0.5,0.25,0),
                 phase = c(0.7,0.7,0.7))
  
  color <- colors[color][[1]]

  # Stack three layers of phase to make a 3d matrix
  if (!is.null(mergeWith)) {
    if (class(mergeWith)=="Image") {
      image <- mergeWith
    } else {
      stack <- simplify2array(list(mergeWith,mergeWith,mergeWith))
      image <- EBImage::Image(stack, colormode="Color") 
    }
  } else {
    stack <- simplify2array(list(bg,bg,bg))
    image <- EBImage::Image(stack, colormode="Color") 
  }
  
  # Some labels are in 0 to n  instead of TRUE/FALSE.
  # They need to be in TF
  if (typeof(label) == "double") label <- label>0
  
  # red, green, blue channels
  image[,,1] <- (image[,,1] * !label) + (bg + color[[1]]) * label
  image[,,2] <- (image[,,2] * !label) + (bg + color[[2]]) * label
  image[,,3] <- (image[,,3] * !label) + (bg + color[[3]]) * label

#   image[,,1] <- bg + label * color[[1]]
#   image[,,2] <- bg + label * color[[2]]
#   image[,,3] <- bg + label * color[[3]]
  
  return(image)
  
}