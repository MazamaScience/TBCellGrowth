#' @export
#' @title Rotate an Image
#' @param imageList list of images orgainzed as [[channel]][[timestep]]
#' @param searchSpace a vector of rotation degrees to check.
#' @description Uses the EBImage rotate function to rotate a list of images.
#' The 'flow' images typically have horizontal lines of bright or dark pixels.
#' As the background images is rotated through a series of angles, the row
#' averages are calculated. The standard deviation of these row averages is
#' the calculated and the rotation that maximizes this standard deviation
#' is chosen as the optimal rotation. This will occur when while and dark
#' lines are horizontally aligned.
#' 
#' After rotation, images will be cropped to remove the excess black space
#' introduced by the rotation.
#' @return A imageList with images of equal or smaller dimensions to the input.

flow_rotateImages <- function(imageList,
                              searchSpace=seq(-2,2,by=0.2)) {
  
  # TODO:  Don't have hardcoded [['phase']] in here
  
  # Background image is assumed to be first image
  background <- imageList[['phase']][[1]]
  
  # ----- Find background edges -----------------------------------------------
  
  if (getRunOptions('verbose')) cat('\tFinding background edges ...\n')
  
  edges <- filter_sobel(background) > 0.5
  edges <- EBImage::fillHull(edges)

  profilePoint('edges','seconds to find background edges')
  
  # ----- Check rotations -----------------------------------------------------
  
  if (getRunOptions('verbose')) cat('\tChecking rotations ...\n')
  
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
  
  profilePoint('checkRotations','seconds to check rotations')
  
  # Find the best rotation
  bestRotation <- searchSpace[which.max(unlist(lapply(rotationResults, sd)))]
  
  # ----- Calculate cropping --------------------------------------------------
  
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
  
  # ----- Rotate and crop -----------------------------------------------------
  
  if (getRunOptions('verbose')) cat('\tRotate and crop ...\n')
  
  rotateAndCrop <- function(x) {
    rotated <- EBImage::rotate(x, bestRotation)
    return(rotated[(xOffset):(dim(x)[[1]]-xOffset), (yOffset):(dim(x)[[2]]-yOffset)])
  }
  
  for (channel in names(imageList)) {
    imageList[[channel]] <- lapply(imageList[[channel]], rotateAndCrop)
  }

  profilePoint('rotateAndCrop','seconds to rotate and crop')
  
  return(imageList)
  
}

