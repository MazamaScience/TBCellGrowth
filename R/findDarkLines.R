#' Find horizontal dark lines, typically the space between plate levels.
#' @export
#' @param m an image matrix to search for dark lines.
#' @return A \code{dataframe} of start and end indices for dark lines.

findDarkLines <- function(m) {
  
  # Iterate through the imagine in 15 pixel rows and find the mean of each row
  meanValue <- c()
  for(y in seq(1,(dim(m)[[2]]-5),1)) {
    sample <- m[ ,y:(y+5)]
    meanValue <- c(meanValue, mean(sample))
  }
  
  # Dark areas
  dStart <- which(diff(meanValue < 0.35) > 0) - 10
  dEnd <- which(diff(meanValue < 0.35) < 0) + 20
  dRange <- cbind(dStart, dEnd)
  
  return(dRange)
  
}