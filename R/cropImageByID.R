#' @export
#' @title Crops an Image Around a Given Blob ID
#' @param id the blob id to crop image around
#' @param centroids a centroids dataframe as generated by
#' \link{generateBlobTimeseries}
#' @param phase a list of phase image matrices, one per timestep
#' @param labeled a binary image label
#' @description Given a blob ID string, look through all centroids for that
#' image and calculate a reasonable rectangle and then crop images around that ID.
#' @return a list with elements bg, the cropped background, and label, the cropped
#' label

cropImageByID <- function(id, centroids, phase, labeled) {
  
  dimx <- dim(phase[[1]])[1]
  dimy <- dim(phase[[1]])[2]
  
  # Get centroid data just for this id
  cId <- lapply(centroids, function(x) x[x$id == id,])

  buffer <- 75
  
  # Find the boundaries of the blob
  # Add a buffer
  x1 <- min(unlist(lapply(cId, function(x) x$xmin))) - buffer
  x2 <- max(unlist(lapply(cId, function(x) x$xmax))) + buffer
  y1 <- min(unlist(lapply(cId, function(x) x$ymin))) - buffer
  y2 <- max(unlist(lapply(cId, function(x) x$ymax))) + buffer
  
  # Make sure the images aren't out of bounds
  x1 <- max(x1, 1)
  x2 <- min(x2, dimx)
  y1 <- max(y1, 1)
  y2 <- min(y2, dimy)
  
  bgRet    <- vector("list",length(phase))
  labelSingle <- vector("list",length(phase))
  labelFull <- vector("list",length(phase))
  
  for (i in 1:length(phase)) {
    
    # Crop the area
    bgRet[[i]] <- phase[[i]][x1:x2,y1:y2]
    
    # All blobs
    labelFull[[i]] <- labeled[[i]][x1:x2,y1:y2]
    
    # Get labeled subsection
    labelbg <- labeled[[i]][x1:x2,y1:y2]
    isIndex <- labelbg == cId[[i]]$index
    if (length(isIndex) < 1) isIndex <- labelbg < -1
    labelSingle[[i]] <- isIndex
  
  }
    
  return(list(bg=bgRet, labelSingle=labelSingle, labelFull=labelFull))
  
}