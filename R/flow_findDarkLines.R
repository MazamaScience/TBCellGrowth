#' @export
#' @title Find Horizontal Dark Lines
#' @param image matrix of image data
#' @description In Fluidomics apparatus, the space between plate levels often
#' generates dark lines in phase contrast microscopy.
#' @return A \code{matrix} of start and end indices for dark lines.

flow_findDarkLines <- function(image) {
  
  # Iterate through the image matrix in 15 pixel rows and find the mean of each row
  meanValue <- c()
  for(j in seq(1,(dim(image)[[2]]-5),1)) {
    sample <- image[ ,j:(j+5)]
    meanValue <- c(meanValue, mean(sample))
  }
  
  # Dark areas
  dStart <- c(which(diff(meanValue < 0.35) > 0), which(diff(meanValue > 0.9) > 0)) - 10
  dEnd <-   c(which(diff(meanValue < 0.35) < 0), which(diff(meanValue > 0.9) < 0)) + 20
  dRange <- cbind(dStart, dEnd)
  
  return(dRange)
  
}