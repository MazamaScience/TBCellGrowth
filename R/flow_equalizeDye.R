#' @export
#' @title Normalize Dye Image Values
#' @param image the image matrix to modify.
#' @param artifactMask a mask of non biological features to ignore. See \link{flow_createArtifactMask}.
#' @description Normalize a given dye image. TODO More info here
#' @return an image of the same dimensions.
# 
# flow_equalizeDye <- function(image, artifactMask) {
#   
#   cat("\n")
#   
#   # For development shorten this so it's easier to play
#   im <- image
#   rm(image)
#   
# #   im <- xy$green[[2]]
#   
#   im[artifactMask] <- NA
#   
#   cat("a=")
#   cat(round(mean(im, na.rm=TRUE),3))
#   
#   im <- im - mean(im, na.rm=TRUE)
#   im[im < 0] <- NA
#   
#   im[im > 0.1] <- 0.1
#   
#   h <- hist(im, breaks=seq(0,0.1, by=0.001), plot=FALSE)
#   
#   keybreak <- h$mids[h$counts < (max(h$counts)/200)][1]
#   
#   cat(", b=")
#   cat(round(1/keybreak,3))
#   
#   im <- im / keybreak
# 
#   return(im)
#   
# }

### TEST
flow_equalizeDye <- function(images, artifactMask) {
  
  # Find median of each image
  medians <- unlist(lapply(images, median))
  
  # Set each median to be the 0.1 and set the median to 0
  ims <- mapply(function(im,m) return(im*(0.1/m) - 0.1), images, medians, SIMPLIFY=FALSE)
  
  # Mask out artifacts
  ims <- lapply(ims, function(im) {
    im[artifactMask] <- mean(im)
    return(im)
  })
  
  # Brighten images based on SD
  ims <- lapply(ims, function(im) {
    return(im > ( mean(im) + 3*sd(im) ))
  })
  
  # Remove noise
  ims <- lapply(ims, function(im) {
    return(removeBlobs(im, 30))
  })
  
  return(ims)
  
}
