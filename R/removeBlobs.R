#' @export
#' @title Remove Blobs By Size
#' @param image an image matrix
#' @param size threshold for removal
#' @description Finds the size of each blob group in an image and removes
#' blobs under the given threshold of pixels. A blob is defined as a group of
#' pixels connected horizontally or vertically.
#' @return An image \code{matrix} of size equal to the input image
#' 

removeBlobs <- function(image, size) {
  # Label blobs
  image <- EBImage::bwlabel(image)
  # Save dimensions
  dims <- dim(image)
  # Unravel matrix and count the occurances of each label
  sorted <- sort(table(as.numeric(image)))
  # Labels which are under the threshold
  small <- as.numeric(names(sorted[sorted<size]))
  # Remove pixels that fall into undersized labels
  image[image %in% small] <- 0
  # Turn back into matrix
  image <- matrix(image, nrow=dims[[1]], ncol=dims[[2]])
  return(image)
}