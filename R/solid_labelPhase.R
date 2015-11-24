#' @export
#' @title Identify and Label Phase Microscopy Groups
#' @param image an image matrix to search for cell colonies
#' @param minColonySize all identified groups of pixels, aka "blobs", below this size are discarded
#' @description Searches an image for dark cell colonies and incrementally labels each colony.
#' @return A \code{matrix} of integer labeled blobs.

solid_labelPhase <- function(image, minColonySize=50) {
  
  if (getRunOptions('verbose')) cat('\tLabeling ...\n')
  
  # Ensure that the greatest pixel value is 1. Some EBImage functions
  # break if pixels greater than 1 are encountered
  image[image > 1] <- 1
  
  # Find the edge detections for the given image with NO blur
  # and a scalar of 2. This increases the sensitivity to edges
  edges <- filter_sobel(image, FALSE, 2)
  
  # ClosingGreyScale first expands the radius of bright objects by 7 pixels
  # and then reduces it by 7. This has the effect of joining blobs together
  # and creating solid blobs where there may have been a weak signal.
  imageEdit <- EBImage::closingGreyScale(edges, EBImage::makeBrush(7))
  
  # Mask the image, saving edge values greater than 60% as "true" and
  # values less than 60% as "false". Our assumption is that strong edges
  # equal bacteria so we keep the top 60%th percentile.
  imageEdit <- imageEdit > 0.6
  
  # Now equalize the original image, which flattens the value histogram.
  # This has the effect of turning bright areas extremely bright and dark areas
  # extremely dark. We then remove the very very brightest areas from the mask.
  # This is because solid images tend to be darkish blobs with a bright halo,
  # and we can use that halo to "carve out" fairly accurate outlines of blobs.
  imageEdit[EBImage::equalize(image) > 0.98] <- 0
  
  # Now add all of the brightest areas back into the image. This seems
  # counterintuitive since we just removed bright regions, but an equalized
  # image brightens everything to an extreme, even "sort of" bright areas.
  # It turns out that the actual brightest areas (in this case defined as
  # greater than 80% intensity) tend to be the centers of bacterial groups,
  # so we want to add those back in.
  imageEdit[image > 0.8] <- 1
  
  # Do a smaller closingGreyScale on the image. We want to pick up any
  # small areas around the blobs that should count as part of the same group
  # but had too weak of edges to be counted as such.
  imageEdit <- EBImage::closingGreyScale(imageEdit, EBImage::makeBrush(3))
  
  # Blobs under 50 pixels are very likely to be noise and there tends to be
  # a lot of those. Removing those speeds up later steps of the algorithm.
  imageEdit <- removeBlobs(imageEdit, 50)
  
  # We're only interested in outlines for solid images, so fill any
  # holes in the images.
  imageEdit <- EBImage::fillHull(imageEdit)
  
  # blabel assigns each distinct blob of white pixels a unique integer
  # value, which we'll later use to identify and track the blobs.
  imageEdit <- EBImage::bwlabel(imageEdit)
  
  # Now remove slightly bigger blobs as a precaution.
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
