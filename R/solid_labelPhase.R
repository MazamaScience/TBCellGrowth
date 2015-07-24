#' @export
#' @title Identify and Label Phase Microscopy Groups
#' @param image an image matrix to search for cell colonies
#' @description Searches an image for dark cell colonies and incrementally labels each colony.
#' @return A \code{matrix} of integer labeled blobs.

solid_labelPhase <- function(image) {
  

  
  print("Searching new image...")
  ptm <- proc.time()
  
  print("Searching new image...")
  
  # Blur image to reduce noise in edge finding
  imageEdit <- blur(image)
  
  # Sobel edge finding implementation
  imageEdit <- sobelFilter(imageEdit)
  
  # Threshold sharp edges
  imageEdit <- imageEdit > 0.4
  
  # Dilate and expand to join found edges
  imageEdit <- EBImage::closingGreyScale(imageEdit, EBImage::makeBrush(9, shape='disc'))
  
  # Mask out dark areas from original image to separate groups
  imageEdit[image < 0.4] <- 0
  
  # Dilate again to further join nearby blobs
  imageEdit <- EBImage::dilateGreyScale(imageEdit, EBImage::makeBrush(5, shape='disc'))
  
  # Fill centers of blobs since we only found edges
  imageEdit <- EBImage::fillHull(imageEdit)
  
  # Remove small blobs
  imageEdit <- removeBlobs(imageEdit, 50)
  
  # Label images
  imageEdit <- EBImage::bwlabel(imageEdit)
  
  print(proc.time() - ptm)
  
  return(imageEdit)
  
}
