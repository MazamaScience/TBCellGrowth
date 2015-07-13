#' @export
#' @title Rotate an Image
#' @param phase a list of phase images
#' @param dyes a list of lists dye images of equal length to phase
#' @param searchSpace a vector of rotation degrees to check.
#' @description Uses the EBImage rotate function to rotate a series
#' of dye and phase images. An algorithm determines which degree value
#' in the searchSpace will be applied. The image will also be cropped 
#' to remove the excess black space introduced by the rotate
#' @return an image of equal or smaller dimensions to the input.

flow_rotateImages <- function(phase, dyes,
                         searchSpace=seq(-2,2,by=0.2)) {
  
  # Always assume phase[[1]] is background image
  background <- phase[[1]]
  
  # Find background edges
  edges <- sobelFilter(background) > 0.5
  edges <- EBImage::fillHull(edges)
  
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
  
  rSample <- EBImage::rotate(background, bestRotation)
  
  yOffset <- min(which(rSample[1,]!=0))
  xOffset <- dim(rSample)[[1]] - max(which(rSample[,1]!=0))
  
  rotateAndCrop <- function(x) {
    rotated <- EBImage::rotate(x, bestRotation)
    return(rotated[(xOffset):(dim(x)[[1]]-xOffset), (yOffset):(dim(x)[[2]]-yOffset)])
  }
  
  return(list(
    phase=lapply(phase, rotateAndCrop),
    dyes=lapply(dyes, function(x) lapply(x, rotateAndCrop))))
}

