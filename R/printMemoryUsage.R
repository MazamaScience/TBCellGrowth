#' @export
#' @title Print Memory Usage
#' @description Prints out the size of each object currently in memory.
#' @return No return.

printMemoryUsage <- function() {

  # Create an object to store the sizes
  DELETEME <- 0
  DELETEME <- rep(0,length(ls()))
  names(DELETEME) <- ls()

  objNames <- ls()

  # Because we are using get(), this has to be in a loop
  for (i in 1:length(DELETEME)) {
    DELETEME[i] <- as.numeric(object.size(get(objNames[i])))
  }

  # Sort and print to stdout
  DELETEME <- round(sort(DELETEME/1e6),1)
  for (i in 1:length(DELETEME)) {
    cat(paste0('\t',sprintf("%8.1f",DELETEME[i]),' MB : ',names(DELETEME)[i],'\n'))
  }

}

