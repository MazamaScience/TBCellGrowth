#' @export
#' @title Rotate an Image
#' @param phase
#' @param dyes
#' @param 
#' @description Uses the EBImage rotate function to rotate an image.
#' The image will also be cropped to remove the excess black space
#' introduced by the rotate
#' @return an image of equal or smaller dimensions to the input.
#' 
rotateImages <- function(image) {
  
  rotations <- seq(-2,2,0.2)
  edges <- sobelFilter(image)
  
  
  
  checkRotations <- function(r, im) {
    rotated <- EBImage::rotate(im, r)
    testArea <- rotated[200:(dim(edges)[[1]]-200),
                      200:(dim(edges)[[2]]-200)]
    apply(testArea, 1, mean)
  }
  
  test <- lapply(rotations, checkRotations, edges)
  
  bestRotation <- rotations[which.max(unlist(lapply(test, sd)))]
  
  display(rotate(image,bestRotation))
  
}

