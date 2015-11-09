#' @export
#' @title Remove Blobs By Size
#' @param image an image matrix
#' @param minSize lower threshold for removal
#' @param maxSize upper threshold for removal
#' @param label logical flag specifying whether to label images
#' @description Finds the size of each blob group in an image and removes
#' blobs under the given threshold of pixels. A blob is defined as a group of
#' pixels connected horizontally or vertically.
#' @return An image \code{matrix} of size equal to the input image
#' 

removeBlobs <- function(image, minSize=0, maxSize=Inf, label=TRUE) {
  # Label blobs
  if (label) image <- EBImage::bwlabel(image)
  # Save dimensions
  dims <- dim(image)
  # Unravel matrix and count the occurances of each label
  sorted <- sort(table(as.numeric(image)))
  # Labels which are under the threshold
  small <- as.numeric(names(sorted[sorted<minSize]))
  large <- as.numeric(names(sorted[sorted>maxSize]))
  # Remove pixels that fall into undersized labels
  image[image %in% small] <- 0
  image[image %in% large] <- 0
  # Turn back into matrix
  image <- matrix(image, nrow=dims[[1]], ncol=dims[[2]])
  return(image)
}