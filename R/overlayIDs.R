#' @export
#' @title Adds colony ID text labels to a given image.
#' @param image the image to modify.
#' @param centroids a \code{dataframe} corresponding to this image.
#' See \link{generateBobTimeseries}.
#' @param labelbg if TRUE adds a transparent background to make the labels
#' stand out more.
#' @description Uses the centroids data for a given image to label blob IDs directly
#' above blobs.
#' @return an image of equal dimensions.

overlayIDs <- function(image, centroids, labelbg=FALSE) {
  
  dimx <- dim(image)[[1]]
  dimy <- dim(image)[[2]]
  
  labelf = function() {
    text(centroids$x, centroids$ymin-8, centroids$id, cex=1)
  }
  
  rectf = function() {
    rect(centroids$x-40, centroids$ymin-16, centroids$x+40, centroids$ymin, col='black')
  }
  
  if (labelbg) {
    rectmat <- plotToOverlay(rectf, dimx, dimy)
    image[rectmat > 0.5] <- image[rectmat > 0.5] * 0.6
  }
  
  labmat <- plotToOverlay(labelf, dimx, dimy)
  
  image[labmat > 0.3] <- 1
  
  return(image)
  
}

