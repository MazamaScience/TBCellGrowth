#' @export
#' @title Add a Scale Bar to an Image
#' @param image an image matrix
#' @param distanceScale the image scale in µm
#' @param barLength how many pixels wide the scale bar should be
#' @description Adds a bar to the bottom right of an image.
#' The bar is labeled according to the scale that is passed in.
#' @return a \code{matrix} image.

overlayScaleBar <- function(image, distanceScale, barLength=100) {
  
  distanceScale <- distanceScale * barLength
  
  # Get dimensions
  dimx <- dim(image)[[1]]
  dimy <- dim(image)[[2]]
  
  plotf <- function() {
    text(dimx-60, dimy-30, paste(distanceScale,"µm"), cex=1.2)
    lines(c(dimx-barlength-10,dimx-10), rep(dimy-20,2), lwd=3)
  }
  
  image[labels > 0.4] <- 1
  
  labels <- plotToOverlay(plotf, dimx, dimy)
  
}