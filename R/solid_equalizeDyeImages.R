#' @export
#' @title Normalize Dye Image Values
#' @param image the image matrix to modify.
#' @param artifactMask a mask of non biological features to ignore. See \link{flow_createArtifactMask}.
#' @description Normalize a given dye image. TODO More info here
#' @return an image of the same dimensions.
#TODO how do we know if a frame isn't useful? Too dark?
solid_equalizeDyeImages <- function(image) {
  
  # For development shorten this so it's easier to play
  im <- image
  rm(image)
  
#   im <- xy$green[[2]]
  
  im <- im - mean(im, na.rm=TRUE)
  im[im < 0] <- NA
  
  h <- hist(im, breaks=50)
  h$mids <- h$mids[h$counts > 100]
  h$counts <- h$counts[h$counts > 100]
  
  keybreak <- h$mids[h$counts < (max(h$counts)/200)][1]
  
  im <- im / keybreak

  return(im)
  
}


