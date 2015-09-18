#' @export
#' @title Add a Scale Bar to an Image
#' @param image an image matrix
#' @param distanceScale the image scale in micrometers
#' @param thresholds the length of the scale bar in sequential order. The last
#' number in the vector will be used unless the image is small enough for one
#' of the other values.
#' @description Adds a bar to the bottom right of an image.
#' The bar is labeled according to the scale that is passed in.
#' @return a \code{matrix} image.

overlayScaleBar <- function(image, distanceScale, thresholds=c(10,50,100)) {
  
#   distanceScale <- distanceScale * barLength
#   
#   # Get dimensions
#   dimx <- dim(image)[[1]]
#   dimy <- dim(image)[[2]]
#   
#   plotf <- function() {
#     text(dimx-(barLength/2)-10, dimy-20, paste(distanceScale,"µm"), cex=barLength/85)
#     lines(c(dimx-barLength-10,dimx-10), rep(dimy-10,2), lwd=3)
#   }
#   
#   labels <- plotToOverlay(plotf, dimx, dimy)
#   
#   image[labels > 0.4] <- 1
#   
#   return(image)
# 
  
  # Get dimensions
  dimx <- dim(image)[[1]]
  dimy <- dim(image)[[2]]
  
  dimxum <- distanceScale * dimx
  
  distance <- thresholds[dimxum > thresholds + 10]
  distance <- distance[length(distance)]
  if (length(distance) < 1) distance <- 5
  
  barLength <- distance / distanceScale
  
  plotf <- function() {
    text(dimx-(barLength/2)-10, dimy-20, paste(distance,"\U00B5m"), cex=barLength/100) # \U00B5 = µ
    lines(c(dimx-barLength-10,dimx-10), rep(dimy-10,2), lwd=3)
  }
  
  labels <- plotToOverlay(plotf, dimx, dimy)
  
  image[labels > 0.4] <- 1
  
  return(image)
  
  
}