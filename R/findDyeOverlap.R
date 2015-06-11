#' @export
#' @title Find the Amount of Overlap Between Dye and Phase Blobs
#' @param dye a list of labeled dye images.
#' @param phase a list of labeled phase images.
#' @param output
#' @description Searches an image for dark cell colonies and incrementally labels each blob.
#' @return A \code{matrix} of integer labeled blobs.

findDyeOverlap <- function(dye, phase, output) {
  
  # Copy output timeseries and set values to 0
  tsCopy <- output$timeseries
  tsCopy[,] <- 0
  
  # Loop through each timestep (row) of the timeseries
  for (i in 1:dim(tsCopy)[[1]]) {
    
    d <- dye[[i+1]]
    p <- phase[[i+1]]
    c <- output$centroids[[i+1]]
    
    for (j in 1:max(d)) {
      # overlap mask: which coordinates in phase are dyed?
      overlap <- p[ d == j ]
      # If at least 20% overlap
      if (sum(overlap>0) > length(overlap)/5) {
        overlap <- overlap[overlap>0]
        unq <- unique(overlap)
        index <- unq[which.max(tabulate(match(overlap, unq)))]
        id <- as.character(c[c$index == index,]$id)
        
        if (id %in% names(tsCopy)) tsCopy[[id]][[i]] <- sum(overlap > 0)
        
      }
    }
  }
  
  return(tsCopy)
  
}