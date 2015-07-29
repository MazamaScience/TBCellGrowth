#' @export
#' @title Adds Basic Statistics to an Image
#' @param image an image matrix
#' @param id the blob id the image is focused on
#' @param size the object size in um
#' @param time the timestep at this frame
#' @description Overylay a series of statistics to an image
#' from cropImageByID. TODO add more details when they are
#' avaiable
#' @return a \code{matrix} image.

overlayVitalStats <- function(image, id, size, time) {
  
  # Get dimensions
  dimx <- dim(image)[[1]]
  dimy <- dim(image)[[2]]
  
#   growth <- growth[!is.na(growth)]
#   slope <- lm(1:length(growth) ~ growth)
  
  # Initialize plotting function. Incrementally adds text labels
  # near the grid lines
  plotf <- function() {
    text(5,8,paste0(time,"hr"),cex=1,adj=c(0,NA))
    text(5,20,paste0(id,"id"),cex=1,adj=c(0,NA))
    text(5,32,paste0(size,"px"),cex=1,adj=c(0,NA))
  }
  
  labels <- plotToOverlay(plotf, dimx, dimy)
  
  # Mask the image with white
  image[labels > 0.4] <- 1
  
  return(image)
  
}