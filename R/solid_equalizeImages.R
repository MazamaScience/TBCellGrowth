#' @export
#' @title Normalize Image Values
#' @param image the image matrix to modify.
#' @description Normalize a given solid image. This function attempts to 
#' replicate the auto levels function in Photoshop. It shifts the peak of
#' the histogram to 0 and scales the values so the histogram is between 
#' 0 and ~0.5
#' @return an image of the same dimensions.

solid_equalizeImages <- function(image) {
  
  image <- filter_blur(image)
  
  # Make histogram of values
  valueHist <- hist(image, breaks=40, plot=FALSE)
  
  # Which index of histogram is highest
  index <- which.max(valueHist$counts) - 1
  
  # What value corresponds to that
  minVal <- valueHist$breaks[index]
  
  # Shift and stretch the image so the new minimum is this 
  # minVal and the max is 1
  image <- image - minVal
  image[image < 0] <- NA
  image <- image / max(image, na.rm=TRUE)
  
  # Make a new histogram
  valueHist <- hist(image, breaks=40, plot=FALSE)
  
  # Find the area after the histogram peak to expand
  index <- which(valueHist$counts/max(valueHist$counts) < 0.1 & c(FALSE,diff(valueHist$counts) < 0))[[1]]
  midVal <- valueHist$breaks[index]
  image <- image / (midVal * 2)
  image[image > 1] <- 1
  
  return(image)
  
}