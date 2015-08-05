#' @export
#' @title Find the Amount of Overlap Between Dye and Phase Blobs
#' @param dye a list of labeled dye images.
#' @param phase.labeled a list of labeled phase images.
#' @param output an output object with timeseries and centroids from
#' \link{generateBlobTimeseries}
#' @description Searches an image for dark cell colonies and incrementally labels each blob.
#' @return A \code{matrix} of integer labeled blobs.

findDyeOverlap <- function(dye, phase.labeled, output) {
  
  # Copy output timeseries and set values to 0
  tsCopy <- output$timeseries
  tsCopy[,] <- 0
  
  # Loop through each timestep (row) of the timeseries
  for (ii in 1:dim(tsCopy)[[1]]) {
    
    cat(".")
    
    centroids <- output$centroids[[ii]]
    
    # Dyes already have correct indices
    dd <- as.numeric(dye[[ii]])
    indices <- 1:length(dd)
    mask <- dd > 0
    dd <- dd[mask]
    indices <- indices[mask]
    
    for (id in colnames(tsCopy)) {
      row <- which(centroids$id == id)
      if (length(row) > 0) {
        index <- centroids[row,]$index
        tsCopy[[id]][[ii]] <- sum(dd==index)
      }
    }
    
  }
  
  return(tsCopy)
  
}