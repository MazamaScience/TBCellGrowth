#' @export
#' @title Align, Normalize Brightness, and Crop a List of Images
#' @param imageList a list of images
#' @param rotation how many degrees to rotate each image
#' @param sample which region of the imageList to use for alignment in the format \code{c(x, y, size)}
#' @param crop amount to crop from each side in format \code{c(bottom, left, top, right)}
#' @note The \code{imageList} that are passed are actually just the contents of the \code{@@.Data} slot
#' of an \code{EBImage::Image} object. We use the term *image* to refer to the matrix of image data
#' that has been pulled out of each \code{EBImage::Image} object.
#' @return A \code{list} of \code{matrices} ready for feature extraction.

preprocessImages <- function(imageList, rotation=0, sample=c(150,450,125), crop=c(150,150,150,150)) {

  normalizeValue <- function(m) {      
    # brighten
    m <- m / max(m)
    # apply rotation
    m <- rotate(m, rotation)
    return(m)
  }
  
  # Sampe values
  x1 <- sample[1]
  y1 <- sample[2]
  wh <- sample[3]
  
  # Adjust image brightness
  imageList <- lapply(imageList, normalizeValue)
  
  # Get background region. Assumes the first image is the background
  bgSample <- imageList[[1]][x1:(x1+wh), y1:(y1+wh)]
  
  # For non-background imageList, align to background image
  for (i in 2:length(imageList)) {
    
    # Debug
    print(paste0("Aligning image ", i))
    
    image <- imageList[[i]]
    
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
    imageList[[i]] <- image[(crop[[2]] + offset.x):(width + offset.x - crop[[4]]),
                         (crop[[3]] + offset.y):(height + offset.y - crop[[1]])]
    
  }
  
  # Crop background
  background <- imageList[[1]]
  background <- background[crop[[2]]:(dim(background)[[1]]-crop[[4]]),
                           crop[[3]]:(dim(background)[[2]]-crop[[1]])]
  imageList[[1]] <- background
  
  return(imageList)
  
}