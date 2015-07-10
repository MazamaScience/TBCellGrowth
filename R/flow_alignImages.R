#' @export
#' @title Align A Series of Flow Images
#' @param phase a list of phase images
#' @param dyes a list of lists dye images of equal length to phase
#' @param alignmentTargets a list of x y coordinates to use for alignment
#' comparisons
#' @param targetWidth the radius of alignment targets
#' @param searchSpace how far to search for best alignment. A smaller
#' value here yeilds faster searches.
#' @description Given a list of phase images and a list of zero or
#' more lists of dye images, aligns these images to the first image
#' of phase (assumed to be a background). Will also crop and rotate
#' the images if those are specified.
#' @return a \code{list} of two lists, \code{phase} and \code{dyes},
#' which will be the same lengths as the input.

flow_alignImages <- function(phase, dyes, alignmentTargets, targetWidth=25, 
                               searchSpace=25) {
  
  dyeNames <- names(dyes)
  
#   # Rotate images
#   phase <- lapply(phase, EBImage::rotate, rotation)
#   dyes <- lapply(dyes, function(dye) lapply(dye, EBImage::rotate, rotation))
#   
  # Cropping helper function
  my_crop <- function(x, offset.x, offset.y, width, height) { 
    return(x[(cropBoundaries[[2]] + offset.x):(width + offset.x - cropBoundaries[[4]]),
             (cropBoundaries[[3]] + offset.y):(height + offset.y - cropBoundaries[[1]])])
  }
  
  # Iterates through dyes and applies cropping to each set
  my_dyeCrop <- function(dye, i, ...) {
    dye[[i]] <- my_crop(dye[[i]], ...)
    return(dye)
  }
  
  # Sample the background image with alignment targets
  bgSamples <- lapply(alignmentTargets, function(x) phase[[1]][(x[[1]]-targetWidth):(x[[1]]+targetWidth),
                                                  (x[[2]]-targetWidth):(x[[2]]+targetWidth)])
  

  # Vectors detailing how much to shift images
  offset.x <- numeric(length(phase))
  offset.y <- numeric(length(phase))  

  for (i in 2:length(phase)) {
    
    image <- phase[[i]]
    
    phaseSamples <- lapply(alignmentTargets, function(x) image[(x[[1]]-targetWidth-searchSpace):(x[[1]]+targetWidth+searchSpace),
                                                       (x[[2]]-targetWidth-searchSpace):(x[[2]]+targetWidth+searchSpace)])
    
    sampleDiffs <- matrix(NA,nrow=1 + searchSpace*2,ncol=1 + searchSpace*2)
    
    for (ii in 1:(searchSpace*2)) {
      for (jj in 1:(searchSpace*2)) { 
        
        x1 <- ii
        x2 <- ii + searchSpace*2
        y1 <- jj
        y2 <- jj + searchSpace*2
        phaseSubset <- lapply(phaseSamples, function(x) x[x1:x2,y1:y2])
        
        # Find the total difference between samples
        diffs <- unlist(mapply(function(x,x1) sum(abs(x-x1)), bgSamples, phaseSubset, SIMPLIFY=FALSE))
        
        sampleDiffs[ii,jj] <- sum(diffs)
        
      }
    }
    
    bestFit <- which(sampleDiffs == min(sampleDiffs, na.rm=T),arr.ind=T)
    
    offset.x[[i]] <- bestFit[[1]] - searchSpace - 1
    offset.y[[i]] <- bestFit[[2]] - searchSpace - 1
    
  }

  # Crop images based on offset
  cropY <- max(abs(offset.y))
  cropX <- max(abs(offset.x))
  
  for (i in 1:length(phase)) {
    
    print(i)
    
    im <- phase[[i]]
    
    dimx <- dim(im)[[1]]
    dimy <- dim(im)[[2]]
    
    
    
    phase[[i]] <- im[(cropX + offset.x[[i]]):(dimx - cropX + offset.x[[i]]),
             (cropY + offset.y[[i]]):(dimy - cropY + offset.y[[i]])]
    
    for (dye in names(dyes)) {
      im <- dyes[[dye]][[i]]
      dyes[[dye]][[i]] <- im[(cropX - offset.x[[i]]):(dimx - cropX - offset.x[[i]]),
                             (cropY - offset.y[[i]]):(dimy - cropY - offset.y[[i]])]
    }
    
  }
    
# 
#     # Crop phase
#     phase[[i]] <- my_crop(image, offset.x, offset.y, dim(image)[[1]], dim(image)[[2]])
#     # Crop Dyes
#     lapply(dyes, my_dyeCrop, i, offset.x, offset.y, dim(image)[[1]], dim(image)[[2]])
#     
  
#   
#   # Crop background image
#   phase[[1]] <- my_crop(phase[[1]], 0, 0, dim(phase[[1]])[[1]], dim(phase[[1]])[[2]])
#   
#   # Crop dye backgrounds
#   dyes <- lapply(dyes, my_dyeCrop, 1, 0, 0, dim(phase[[1]])[[1]], dim(phase[[1]])[[2]])
#   names(dyes) <- dyeNames
#   
  
  return(list(phase=phase, dyes=dyes))
  
}

