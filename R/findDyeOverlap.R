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
  for (i in 1:dim(tsCopy)[[1]]) {
    
    d <- dye[[i+1]]
    p <- phase.labeled[[i+1]]
    c <- output$centroids[[i+1]]
    
    if (max(d) < 1) break
    
    for (j in 1:max(d)) {
      
      if (sum(d==j)) {
        overlap <- p[d==j]
        id <- as.character(c[c$index == j,]$id)
        print(i)
        if (id %in% names(tsCopy)) tsCopy[[id]][[i]] <- sum(overlap > 0)
      }
      
    }
    
  }
  
  return(tsCopy)
  
}