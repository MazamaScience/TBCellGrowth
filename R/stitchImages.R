#' @export
#' @title Stitch multiple images together.
#' @param images an ordered \code{list} of images to combine.
#' @param ncol the number of columns to arrange images in.
#' @param na.fill value to fill blank space with if the length of images isn't 
#' divisible by ncol 
#' @description Creates an animated .gif file from a list of images using ImageMagick.
#' Images must be the same dimensions.
#' @return an image matrix containing the original images.


# Combines two images in the order specified
stitchImages <- function(images, ncol=2, na.fill=NA) {

  
  a <- ceiling((1:length(images))/ncol)

  test <- vector("list", max(a))
  
  for (i in 1:max(a)) {
    b <- images[which(a == i)]
    c <- do.call(rbind, b)
    test[[i]] <- c
  }
  
  dimLast <- dim(test[[length(test)]])
  dimFirst <- dim(test[[1]])
  
  if (dimFirst[[1]] > dimLast[[1]]) {
    width <- dimFirst[[1]] - dimLast[[1]]
    filler <- matrix(na.fill, nrow = (dimFirst[[1]] - dimLast[[1]]), ncol=dimLast[[2]])
    test[[length(test)]] <- rbind(test[[length(test)]], filler)
  }
  
  d <- do.call(cbind, test)
  
  return(d)
  
}