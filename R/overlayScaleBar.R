# Add a scale bar to an image, set at 100 px
overlayScaleBar <- function(image, distanceScale) {
  
  distanceScale <- distanceScale * 100
  
  # Get dimensions
  dimx <- dim(image)[[1]]
  dimy <- dim(image)[[2]]
  
  plotf <- function() {
    text(dimx-60, dimy-30, paste(distanceScale,"µm"), cex=1.2)
    lines(c(dimx-110,dimx-10), rep(dimy-20,2), lwd=3)
  }
  
  image[labels > 0.4] <- 1
  
  labels <- plotToOverlay(plotf, dimx, dimy)
  
}