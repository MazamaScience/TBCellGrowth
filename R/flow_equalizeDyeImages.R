#' @export
#' @title Normalize Dye Image Values
#' @param image the image matrix to modify.
#' @param artifactMask a mask of non biological features to ignore. See \link{flow_createArtifactMask}.
#' @description Normalize a given dye image. TODO More info here
#' @return an image of the same dimensions.

flow_equalizeDyeImages <- function(image, artifactMask) {
  
  # For development shorten this so it's easier to play
  im <- image
  rm(image)
  
#   im <- xy$green[[2]]
  
  im[artifactMask] <- NA
  
  im <- im - mean(im, na.rm=TRUE)
  im[im < 0] <- NA
  
  im[im > 0.1] <- 0.1
  
  h <- hist(im, breaks=seq(0,0.1, by=0.001), plot=FALSE)
  
  keybreak <- h$mids[h$counts < (max(h$counts)/200)][1]
  
  im <- im / keybreak
  
  return(im)
  
}

