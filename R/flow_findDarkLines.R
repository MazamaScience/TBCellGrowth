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
  y1 <- c(which(diff(meanValue < 0.32) > 0), which(diff(meanValue > 0.9) > 0)) - 20
  y2 <- c(which(diff(meanValue < 0.32) < 0), which(diff(meanValue > 0.9) < 0)) + 20
  dRange <- cbind(1, dim(image)[1], cbind(y1,y2))
  
  colnames(dRange) <- c("x1", "x2", "y1", "y2")
  
#   rows <- unlist(apply(dRange, 1, function(x) x[[1]]:x[[2]]))
  
  return(dRange)
  
}