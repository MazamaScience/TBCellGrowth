#' @export
#' @title Find Overlap Between Dye and Phase Blobs
#' @param imageList a list of labeled dye images
#' @param timeseriesList a list with timeseries and centroids from \link{generateBlobTimeseries}
#' @description For every labeled blob in every dye image, count the number of pixels
#' where the blob overlaps with a blob generated from the phase images.
#' @return A \code{matrix} of integer labeled blobs.

findDyeOverlap <- function(imageList, timeseriesList) {
  
  # NOTE:  The timeseries list contains two elements:
  # NOTE:  1) timeseries -- dataframe with colony id as column name and timestep as row
  # NOTE:  2) centroids -- a list of dataframes, one per timestep
  # NOTE:  
  # NOTE:  Each centroids dataframe has a row for each colony present at that timestep
  # NOTE:  and the following columns: 
  # NOTE:  * x -- x pixel coordinate
  # NOTE:  * y -- y pixel coordinate
  # NOTE:  * xmin, xmax, ymin, ymax -- pixel coordinates
  # NOTE:  * size -- total pixel count for this colony
  # NOTE:  * id -- human name for this colony
  # NOTE:  * index -- numeric identifier for this colony  
  
  # Initialize a dyeOverlap dataframe of the same dimensions as the timeseries dataframe.
  dyeOverlapDF <- timeseriesList$timeseries
  dyeOverlapDF[,] <- 0
  
  # Loop through each timestep
  for (i in 1:nrow(dyeOverlapDF)) {
    
    # Each timestep has its own centroids dataframe
    centroidsDF <- timeseriesList$centroids[[i]]
    
    # NOTE:  Dye colonies already have correct indices assigned during flow_labelDye
    # NOTE:  Here we unravel the matrix of blob indices and remove zeroes.
    blobIndices <- as.numeric(imageList[[i]])
    blobIndices <- blobIndices[blobIndices > 0]
    
    # For each colony, find the number of dye image pixels with a matching index
    for (id in colnames(dyeOverlapDF)) {
      # NOTE:  Not every colony is found in every timestep's centroidsDF
      row <- which(centroidsDF$id == id)
      if (length(row) > 0) {
        index <- centroidsDF[row,'index']
        dyeOverlapDF[[id]][[i]] <- sum(blobIndices==index)
      }
    }
    
  }
  
  return(dyeOverlapDF)
  
}