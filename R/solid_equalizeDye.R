#' @export
#' @title Normalize Dye Image Values
#' @param image the image matrix to modify.
#' @description Normalize a given dye image. TODO More info here
#' @return an image of the same dimensions.
#' 
#TODO how do we know if a frame isn't useful? Too dark?
solid_equalizeDye <- function(image) {
  
  cat("\n")
  
  # For development shorten this so it's easier to play
  im <- image
  rm(image)
  
#   im <- xy$green[[2]]
  
  cat("a=")
  cat(round(mean(im,na.rm=TRUE),3))
  
  im <- im - mean(im, na.rm=TRUE)
  im[im < 0] <- NA
  
  h <- hist(im, breaks=50)
  h$mids <- h$mids[h$counts > 100]
  h$counts <- h$counts[h$counts > 100]
  
  keybreak <- h$mids[h$counts < (max(h$counts)/200)][1]
  
  cat(", b=")
  cat(round(1/keybreak, 3))
  
  im <- im / keybreak

  return(im)
  
}


