#' @export
#' @title Rotate an Image
#' @param images a list of lists of images with channel names as keys.
#' @param searchSpace a vector of rotation degrees to check.
#' @description Uses the EBImage rotate function to rotate a series
#' of dye and phase images. An algorithm determines which degree value
#' in the searchSpace will be applied. The image will also be cropped 
#' to remove the excess black space introduced by the rotate
#' @return an image of equal or smaller dimensions to the input.

flow_rotateImages <- function(images,
                              searchSpace=seq(-2,2,by=0.2)) {
  
  ptm <- proc.time()
  cat("\nRotating images")
  
  # Always assume phase[[1]] is background image
  background <- images$phase[[1]]
  
  # Find background edges
  edges <- filter_sobel(background) > 0.5
  edges <- EBImage::fillHull(edges)
  
  # TODO:  Can searchBorders be esimated from the angle and size of the image matrix?
  searchBorders <- 200
  
  # Checks the variation in image rows when rotated
  checkRotations <- function(r, im) {
    rotated <- EBImage::rotate(im, r)
    testArea <- rotated[searchBorders:(dim(edges)[[1]]-searchBorders),
                        searchBorders:(dim(edges)[[2]]-searchBorders)]
    apply(testArea, 1, mean)
  }
  
  # What happens when you rotate the images
  rotationResults <- lapply(searchSpace, checkRotations, edges)
  
  # Find the best rotation
  bestRotation <- searchSpace[which.max(unlist(lapply(rotationResults, sd)))]
  
  background[background==0] <- -5 # Ensures we don't select anything that's already black
  # Rotate a sample so we know how much to crop
  rSample <- EBImage::rotate(background, bestRotation) 
  # Label empty areas from rotation
  rSample <- EBImage::bwlabel(rSample==0)
  # Initialize x and y offset
  xOffset <- 0
  yOffset <- 0
  # Loop through leftover area
  for (i in 1:max(rSample)){
    # Get x and y size of this area
    sample <- data.frame(which(rSample==i, arr.ind=TRUE))
    x <- diff(range(sample$row))
    y <- diff(range(sample$col))
    # Determine which kind of offset this is and update it
    if (x < y) xOffset <- max(xOffset, x)
    if (x > y) yOffset <- max(yOffset, y)
  }
  
  rotateAndCrop <- function(x) {
    cat(".")
    rotated <- EBImage::rotate(x, bestRotation)
    return(rotated[(xOffset):(dim(x)[[1]]-xOffset), (yOffset):(dim(x)[[2]]-yOffset)])
  }
  
  images <- lapply(images, function(dye) lapply(dye, rotateAndCrop))
  
  cat(paste0("\nImages rotated in ", formatTime(ptm)))
  
  return(images)
  
}

