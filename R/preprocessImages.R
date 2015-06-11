#' @export
#' @title Align, Normalize Brightness, and Crop a List of Images
#' @param phase a list of phase microscopy images
#' @param green a list of dye images
#' @param rotation how many degrees to rotate each image
#' @param sample which region of the phase to use for alignment in the format \code{c(x, y, size)}
#' @param crop amount to crop from each side in format \code{c(bottom, left, top, right)}
#' @note The \code{phase} that are passed are actually just the contents of the \code{@@.Data} slot
#' of an \code{EBImage::Image} object. We use the term *image* to refer to the matrix of image data
#' that has been pulled out of each \code{EBImage::Image} object.
#' @return A \code{list} of \code{matrices} ready for feature extraction.

preprocessImages <- function(phase, dyes=list(), rotation=0, sample=c(150,450,125), crop=c(150,150,150,150)) {
  
  # Cropping function
  cropf <- function(x, offset.x, offset.y) { 
    return(x[(crop[[2]] + offset.x):(width + offset.x - crop[[4]]),
             (crop[[3]] + offset.y):(height + offset.y - crop[[1]])])
  }
  
  # For a dye object, crop the current parent index
  # pass additional arguments to cropf
  dyeIter <- function(x, i, ...) {
    x[[i]] <- cropf(x[[i]], ...)
    return(x)
  }

  normalizePhase <- function(m) {      
    # brighten
    m <- m / max(m)
    # apply rotation
    m <- EBImage::rotate(m, rotation)
    return(m)
  }
  
  normalizeDye <- function(m) {
    # brighten
    m <- m * 40
    # apply rotation
    m <- EBImage::rotate(m, rotation)
    return(m)
  }
  
  # Sampe values
  x1 <- sample[1]
  y1 <- sample[2]
  wh <- sample[3]
  
  # Brighten and rotate phase
  phase <- lapply(phase, normalizePhase)
  
  # Brighten and rotate dyes
  dyes=lapply(dyes, function(x) lapply(x, normalizeDye))
#   if (!is.na(green)) green <- lapply(green, normalizeDye)
#   if (!is.na(red)) red <- lapply(red, normalizeDye)
  
  # Get background region. Assumes the first image is the background
  bgSample <- phase[[1]][x1:(x1+wh), y1:(y1+wh)]
  
  # For non-background phase, align to background image
  for (i in 2:length(phase)) {
    
    # Debug
    print(paste0("Aligning image ", i))
    
    image <- phase[[i]]
    
    # Take a sample 50px larger on each side than the background sample
    subset <- image[(x1-50):(x1+wh+50), (y1-50):(y1+wh+50)]
    
    # Initialize matrix for recording differences
    diffs <- matrix(NA,nrow=100,ncol=100)
    
    # Search the subset space for the least different region
    for (x in 1:100) {
      for (y in 1:100) {
        sample <- subset[x:(x+wh),y:(y+wh)]
        diffs[x,y] <- mean(abs(sample-bgSample))
      }
    }
    
    # Find the x and y offsets
    offset.x <- which(diffs == min(diffs),arr.ind=T)[[1]] - 50
    offset.y <- which(diffs == min(diffs),arr.ind=T)[[2]] - 50
    
    # Image dimensions
    width <- dim(image)[[1]]
    height <- dim(image)[[2]]
    
    # Crop phase
    phase[[i]] <- cropf(image, offset.x, offset.y)
    
    # Crop dyes
    dyes <- lapply(dyes, dyeIter, i, offset.x, offset.y)
    
  }
  
  phase[[1]] <- cropf(phase[[1]], 0, 0)

  dyes <- lapply(dyes, dyeIter, 1, 0, 0)
  
  return(list(phase=phase, dyes=dyes))
  
}