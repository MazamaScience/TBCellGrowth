#' @export
#' @title Normalize Dye Image Values
#' @param image the image matrix to modify.
#' @param artifactMask a mask of non biological features to ignore. See \link{flow_createArtifactMask}.
#' @description Normalize a given dye image. TODO:  More info here
#' @return An image of the same dimensions.

flow_equalizeDye <- function(image, artifactMask) {
  
  dyeMedian <- getRunOptions('dyeMedian')

  if (getRunOptions('verbose')) cat('\tBlurring dye image ...\n')
  
  # Get rid of noise by blurring
  image <- filter_blur(image,11)
  profilePoint('blur','seconds to blur dye image')
  
  # Apply artifact mask
  image[artifactMask] <- NA
  
  # Center and rescale the range of values
  image <- image * (dyeMedian / median(image,na.rm=TRUE))
  image <- ((image-0.5)*6)^4 # Ad hoc scaling factor
  
  # Set artifacts to the median value
  image[artifactMask] <- median(image, na.rm=TRUE)
  
  profilePoint('equalize','seconds to equalize dye image')
  
  return(image)
  
}

