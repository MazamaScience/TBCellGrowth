#' @export
#' @title Identify and Label Phase Microscopy Groups
#' @param image an image matrix to search for cell colonies
#' @description Searches an image for dark cell colonies and incrementally labels each colony.
#' @return A \code{matrix} of integer labeled blobs.

solid_labelPhase <- function(image) {

  if (getRunOptions('verbose')) cat('\tLabeling ...\n')
  
  image[image > 1] <- 1
  
  edges <- filter_sobel(image, FALSE, 2)
  
  imageEdit <- EBImage::closingGreyScale(edges, EBImage::makeBrush(7))
  
  imageEdit <- imageEdit > 0.5
  
  imageEdit[EBImage::equalize(image) > 0.98] <- 0
  
  # imageEdit <- EBImage::dilateGreyScale(imageEdit, EBImage::makeBrush(3))
  imageEdit <- EBImage::closingGreyScale(imageEdit, EBImage::makeBrush(7))
  
  imageEdit <- removeBlobs(imageEdit, 45)
  
  imageEdit <- EBImage::bwlabel(imageEdit)
  
  imageEdit[EBImage::equalize(image) > 0.98] <- 0
  
  imageEdit <- removeBlobs(imageEdit, 60, label=FALSE)
  
  # For checking results during development
  if (FALSE) {
    display(overlayOutlines(image, imageEdit))
  }
  
  return(imageEdit)
  
}
# 
# expandEdges <- function(im, width, val) {
#   im[1:width,10:(dim(im)[2]-10)] <- val
#   im[10:(dim(im)[1]-10),1:width] <- val
#   im[(dim(im)[1]-width):(dim(im)[1]),10:(dim(im)[2]-10)] <- val
#   im[10:(dim(im)[1]),(dim(im)[2]-width):(dim(im)[2])] <- val
#   return(im)
# }