#' @export
#' @title Adds a grid to an image.
#' @param image the image to modify.
#' @param spacing how many pixels between each grid line.
#' @description Adds a white grid to an image with 1px lines. Grid labels are on the left and top.
#' @return an image of equal dimensions.

overlayGrid <- function(image, spacing=200) {
  
  # Get dimensions
  dimx <- dim(image)[[1]]
  dimy <- dim(image)[[2]]
  
  # At what indices should the lines be drawn
  xLines <- seq(1,dimy,spacing)
  yLines <- seq(1,dimx,spacing)
  
  # Draw the lines
  image[c(yLines),] <- 1
  image[,c(xLines)] <- 1
  
  # Initialize plotting function. Incrementally adds text labels
  # near the grid lines
  plotf <- function() {
    for (x in xLines) {
      text(20,x+15,(x-1),cex=1.2)
    }
    
    for (y in yLines) {
      text(y+20,20,(y-1),cex=1.2)
    }
  }
  
  # Call plotToOverlay which returns an equal sized image with the lines
  # and text as a mask
  labels <- plotToOverlay(plotf, dimx, dimy)
  
  # Mask the image with white
  image[labels > 0] <- 1
  
  return(image)
  
}