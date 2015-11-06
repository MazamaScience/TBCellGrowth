#' @export
#' @title Print Memory Usage
#' @description Prints out the size of each object currently in memory.
#' @return No return.

printMemoryUsage <- function() {

  # Get names from the global environment
  objectNames <- ls(globalenv())
  
  # Create an object to store the sizes
  objectSizes <- rep(0, length(objectNames))
  names(objectSizes) <- objectNames

  # Because we are using get(), this has to be in a loop
  for (name in objectNames) {
    objectSizes[name] <- as.numeric(object.size(get(name)))
  }

  # Sort and print to stdout
  objectSizes <- round(sort(objectSizes/1e6),1)
  cat('\t----------------------------------------\n')
  cat('\t-         Current Memory Usage         -\n')
  cat('\t----------------------------------------\n')
  for (name in objectNames) {
    cat(paste0('\t',sprintf("%8.1f",objectSizes[name]),' MB : ',name,'\n'))
  }
  cat('\t----------------------------------------\n')
  
}

