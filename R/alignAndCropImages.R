#' @export
#' @title Crop and Align A Series of Images
#' @param phase a list of phase images
#' @param dyes a list of lists dye images of equal length to phase
#' @param alignmentSample region of background image to align with (in order:
#' starting x position, starting y position, square width)
#' @param cropBoundaries how many pixels to crop from each side (in order:
#' bottom, left, top, right
#' @param rotation degrees to rotate images
#' @description Given a list of phase images and a list of zero or
#' more lists of dye images, aligns these images to the first image
#' of phase (assumed to be a background). Will also crop and rotate
#' the images if those are specified.
#' @return a \code{list} of two lists, \code{phase} and \code{dyes},
#' which will be the same lengths as the input.

alignAndCropImages <- function(phase, dyes, alignmentSample=c(200,200,100), cropBoundaries=c(0,0,0,0), rotation=0) {
  
  # Extract information from alignmentSample
  xx <- alignmentSample[[1]]
  yy <- alignmentSample[[2]]
  width <- alignmentSample[[3]]
  
  # Cropping helper function
  my_crop <- function(x, offset.x, offset.y) { 
    return(x[(cropBoundaries[[2]] + offset.x):(width + offset.x - cropBoundaries[[4]]),
             (cropBoundaries[[3]] + offset.y):(height + offset.y - cropBoundaries[[1]])])
  }
  
  # Iterates through dyes and applies cropping to each set
  my_dyeCrop <- function(dye, i, ...) {
    dye[[i]] <- my_crop(dye[[i]], ...)
    return(dye)
  }
  
  bgSample <- phase[[1]][xx:(xx+width), yy:(yy+width)]
  
  
  for (i in 2:length(phase)) {
    
    searchBounds <- 50
    
    print(paste0("Aligning image ", i-1, " of ", length(phase)-1))
    
    # Take a sample 50px larger on each side than the background sample
    phaseSample <- phase[[i]][(xx-searchBounds):(xx+width+searchBounds), 
                              (yy-searchBounds):(yy+width+searchBounds)]
    
    # Initialize matrix for recording differences
    diffs <- matrix(NA,nrow=searchBounds*2,ncol=searchBounds*2)
    
    # Search the subset space for the least different region
    for (x in 1:100) {
      for (y in 1:100) {
        sample <- subset[x:(x+width),y:(y+width)]
        diffs[x,y] <- mean(abs(phaseSample-bgSample))
      }
    }
    
    # Find the x and y offsets
    offset.x <- which(diffs == min(diffs),arr.ind=T)[[1]] - searchBounds
    offset.y <- which(diffs == min(diffs),arr.ind=T)[[2]] - searchBounds
    
    # Image dimensions
    width <- dim(image)[[1]]
    height <- dim(image)[[2]]
    
    # Crop phase
    phase[[i]] <- crop(image, offset.x, offset.y)
    
    # Crop dyes
    dyes <- lapply(dyes, dyeIter, i, offset.x, offset.y)
    
  }
  
}

