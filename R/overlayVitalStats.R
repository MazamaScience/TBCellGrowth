#' @export
#' @title Adds Basic Statistics to an Image
#' @param image an image matrix
#' @param id the blob id the image is focused on
#' @param time the timestep at this frame
#' @param size the object size in um
#' @param distanceScale the distance conversion in um/pixel
#' @description Overylay a series of statistics to an image
#' from cropImageByID. TODO add more details when they are
#' avaiable
#' @return a \code{matrix} image.

overlayVitalStats <- function(image, id, time, size, distanceScale) {
  
  # Get dimensions
  dimx <- dim(image)[[1]]
  dimy <- dim(image)[[2]]
  
  # Convert size
  size <- round((distanceScale * sqrt(size))^2, 1)
  
#   growth <- growth[!is.na(growth)]
#   slope <- lm(1:length(growth) ~ growth)
  
  # Initialize plotting function. Incrementally adds text labels
  # near the grid lines
  plotf <- function() {
    text(5,8,paste0(time,"hr"),cex=1,adj=c(0,NA))
    text(5,20,paste0(id,"id"),cex=1,adj=c(0,NA))
    text(5,32,paste0(size,"µm^2"),cex=1,adj=c(0,NA))
  }
  
  labels <- plotToOverlay(plotf, dimx, dimy)
  
  # Mask the image with white
  image[labels > 0.4] <- 1
  
  return(image)
  
}