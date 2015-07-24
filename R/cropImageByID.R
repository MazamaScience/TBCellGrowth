#' @export
#' @title Crops an Image Around a Given Blob ID
#' @param id the blob id to crop image around
#' @param output an output object with timeseries and centroids from
#' \link{generateBlobTimeseries}
#' @param phase a list of phase image matrices
#' @param labeled a binary image label
#' @description Given a blob ID string, crops an image around that id.
#' @return a list with elements bg, the cropped background, and label, the cropped
#' label

cropImageByID <- function(id, output, phase, labeled) {
  
  # Extract timeseries and centroids for reference
  timeseries <- output$timeseries
  centroids <- output$centroids
  
  # Ignore the first index here, it's empty (bg frame)
#   centroids[[1]] <- NULL
  
  dimx <- dim(phase[[1]])[[1]]
  dimy <- dim(phase[[1]])[[2]]
  
  # Get time series just for the id
  series <- timeseries[id]
  
  # Get centroid data just for this id
  cId <- lapply(centroids, function(x) x[x$id == id,])
  
  # Find the boundaries of the blob
  # Add a 15 pixel buffer
  x1 <- min(unlist(lapply(cId, function(x) x$xmin))) - 15
  x2 <- max(unlist(lapply(cId, function(x) x$xmax))) + 15
  y1 <- min(unlist(lapply(cId, function(x) x$ymin))) - 15
  y2 <- max(unlist(lapply(cId, function(x) x$ymax))) + 15
  
  # Make sure the images aren't out of bounds
  x1 <- max(x1, 1)
  x2 <- min(x2, dimx)
  y1 <- max(y1, 1)
  y2 <- min(y2, dimy)
  
  bgRet    <- vector("list",length(phase))
  labelRet <- vector("list",length(phase))
  
  for (ii in 1:length(phase)) {
    
    # Crop the area
    bgRet[[ii]] <- phase[[ii]][x1:x2,y1:y2]
    
    # Get labeled subsection
    labelbg <- labeled[[ii]][x1:x2,y1:y2]
    isIndex <- labelbg == cId[[ii]]$index
    if (length(isIndex) < 1) isIndex <- labelbg < -1
    labelRet[[ii]] <- isIndex
  
  }
  
  print(lapply(labelRet, sum))
    
  return(list(bg=bgRet, label=labelRet))
  
}