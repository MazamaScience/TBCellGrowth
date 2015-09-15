#' @export
#' @title Adds a timestamp to an image.
#' @param image the image to modify.
#' @param timeString a string with the time at the given image frame.
#' @description Adds a timestamp to the upper right corner of an image.
#' @return an image of equal dimensions.

overlayTimestamp <- function(image, timeString) {
  
  # Get dimensions
  dimx <- dim(image)[[1]]
  dimy <- dim(image)[[2]]
  
  # Initialize plotting function. Incrementally adds text labels
  # near the grid lines
  plotf <- function() {
    text(dimx - 100,15,timeString,cex=1.2)
  }
  
  # Call plotToOverlay which returns an equal sized image with the lines
  # and text as a mask
  labels <- plotToOverlay(plotf, dimx, dimy)
  
  # Mask the image with white
  image[labels > 0] <- 1
  
  return(image)
  
}