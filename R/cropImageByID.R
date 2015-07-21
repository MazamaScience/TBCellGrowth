#' @export
#' @title Crops an Image Around a Given Blob ID
#' @param id the blob id to crop image around
#' @param output an output object with timeseries and centroids from
#' \link{generateBlobTimeseries}
#' @param bg a phase image matrix
#' @param label a binary image label
#' @description Given a blob ID string, crops an image around that id.
#' @return a list with elements bg, the cropped background, and label, the cropped
#' label

cropImageByID <- function(id, output, bg, label) {
  
  # Extract timeseries and centroids for reference
  timeseries <- output$timeseries
  centroids <- output$centroids
  
  # Ignore the first index here, it's empty (bg frame)
  centroids[[1]] <- NULL
  
  dimx <- dim(bg[[1]])[[1]]
  dimy <- dim(bg[[1]])[[2]]
  
  # The width and height of the cropped image
  width <- 100
  height <- 100
  
  # Get time series just for the id
  series <- timeseries[id]
  
  # Get centroid data just for this id
  cId <- lapply(centroids, function(x) x[x$id == id,])
  
  # Find the mean center of this blob
  x1 <- min(unlist(lapply(cId, function(x) x$x)))
  x2 <- max(unlist(lapply(cId, function(x) x$x)))
  y1 <- min(unlist(lapply(cId, function(x) x$y)))
  y2 <- max(unlist(lapply(cId, function(x) x$y)))
  
  bgRet <- vector("list",length(bg)-1)
  labelRet <- vector("list",length(bg)-1)
  
  for (ii in 2:length(bg)) {
    
    # Crop the area
    bgRet[[ii-1]] <- bg[[ii]][x1:x2,y1:y2]
    
    # Get labeled subsection
    labelbg <- label[[ii]][x1:x2,y1:y2]
    isIndex <- labelbg == cId[[ii-1]]$index
    if (length(isIndex) < 1)  isIndex <- labelbg < -1
    labelRet[[ii-1]] <- isIndex
    
  
  }
    
  return(list(bg=bgRet, label=labelRet))
  
}