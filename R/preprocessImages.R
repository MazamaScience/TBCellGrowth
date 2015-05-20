#' Align, normalize brightness, and crop a list of image matrices.
#' @export
#' @param frames a list of matrices.
#' @param rotation how many degrees to rotate each image.
#' @param sample which region of the frames to use for alignment.
#' @param crop amount to crop from each side in format \code{c(bottom, left, top,
#' right)}.
#' @return A \code{list} of \code{matrices} ready for feature extraction.

preprocessFrames <- function(frames, rotation=0, sample=c(150,450,125), crop=c(150,150,150,150)) {

  normalizeValue <- function(m) {      
    # brighten
    m <- m / max(m)
    # apply rotation
    m <- rotate(m, rotation)
    return(m)
  }
  
  # Sampe values
  x1 <- sample[[1]]
  y1 <- sample[[2]]
  wh <- sample[[3]]
  
  # Adjust image brightness
  frames <- lapply(frames, normalizeValue)
  
  # Get background region. Assumes the first image is the background
  bgSample <- frames[[1]][x1:(x1+wh), y1:(y1+wh)]
  
  # For non-background frames, align to background frame
  for (i in 2:length(frames)) {
    
    # Debug
    print(paste0("Aligning frame ", i))
    
    image <- frames[[i]]@.Data
    
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
    
    # Crop image, adjust by offsets
    frames[[i]] <- image[(crop[[2]] + offset.x):(width + offset.x - crop[[4]]),
                         (crop[[3]] + offset.y):(height + offset.y - crop[[1]])]
    
  }
  
  # Crop background
  background <- frames[[1]]
  background <- background[crop[[2]]:(dim(background)[[1]]-crop[[4]]),
                           crop[[3]]:(dim(background)[[2]]-crop[[1]])]
  frames[[1]] <- background
  
  return(frames)
  
}