#' @export
#' @title Identify and Label Phase Microscopy Groups
#' @param image an image matrix to search for cell colonies
#' @description Searches an image for dark cell colonies and incrementally labels each colony.
#' @return A \code{matrix} of integer labeled blobs.

solid_labelPhase <- function(image) {

  cat(".")
  
  # Blur image to reduce noise in edge finding
  imageEdit <- filter_blur(image, 5)
  
  # Sobel edge finding implementation
  imageEdit <- filter_sobel(imageEdit)
  
  # Threshold sharp edges
  imageEdit <- imageEdit > 0.15
  
  # Dilate and expand to join found edges
  imageEdit <- EBImage::closingGreyScale(imageEdit, EBImage::makeBrush(9, shape='disc'))

  imageEdit <- expandEdges(imageEdit, 4, 1)

  imageEdit <- EBImage::fillHull(imageEdit)
  
  imageEdit <- expandEdges(imageEdit, 4, 0)

  imageEdit <- EBImage::erodeGreyScale(imageEdit, EBImage::makeBrush(3, shape='disc'))
  
  # Mask out dark areas from original image to separate groups
  # imageEdit[image < 0.35] <- 0
  
  # Dilate again to further join nearby blobs
  # imageEdit <- EBImage::dilateGreyScale(imageEdit, EBImage::makeBrush(5, shape='disc'))
  
  # Fill centers of blobs since we only found edges
  imageEdit <- EBImage::fillHull(imageEdit)
  
  # Remove small blobs
  imageEdit <- removeBlobs(imageEdit, 75)
  
  # Label images
  imageEdit <- EBImage::bwlabel(imageEdit)
  
  
  # imageEdit[image < 0.35] <- 0
  
  # imageEdit <- EBImage::fillHull(imageEdit)
  
  return(imageEdit)
  
}

expandEdges <- function(im, width, val) {
  im[1:width,10:(dim(im)[2]-10)] <- val
  im[10:(dim(im)[1]-10),1:width] <- val
  im[(dim(im)[1]-width):(dim(im)[1]),10:(dim(im)[2]-10)] <- val
  im[10:(dim(im)[1]),(dim(im)[2]-width):(dim(im)[2])] <- val
  return(im)
}