#' @export
#' @title Identify and Label Phase Microscopy Groups
#' @param image an image matrix to search for cell colonies
#' @param minColonySize all identified groups of pixels, aka "blobs", below this size are discarded
#' @param minSizeExpansion used to determine size threshold in final removal of "junk"
#' @param detectionThreshold brightness level above which colonies are detected
#' @param haloQuantile brightness level of equalized image above which pixels are considred to be a colony "halo"
#' @param brightThreshold brightness level above which pixels ARE considered part of a colony's "internal halo" (different from flow_labelPhase)
#' @param dilateErodeBrush1 size of brush used to convert edges into full colonies
#' @param dilateErodeBrush2 size of brush used to merge nearby pixels that are likely part of the same colony
#' @description Searches an image for dark cell colonies with a light "halo" and incrementally labels each colony.
#' @note A lot of "photoshop magic" happens here and the algorithm has several arguments
#' that might need to be adjusted if the quality of images changes significantly. The algorithm 
#' was tailored to the "phase1" channel of solid substrate images that have the appearance of
#' dark cell colonies with a light "halo".
#' 
#' This algorithm is inappropriate for imagery that does not have this appearance.
#' 
#' Algorithm steps include:
#' \enumerate{
#'   \item{edge detection with a Sobel filter}
#'   \item{dilate/erode with a brush width of \code{dilateErodeBrush1} to get contiguous areas identified as colonies}
#'   \item{mark pixels above \code{detectionThreshold} as colonies (white)}
#'   \item{change the very brightest pixels (above \code{haloQuantile}) to black to get crsip edges}
#'   \item{change all pixels above \code{brightThreshold} to white to restore colony internal pixels}
#'   \item{dilate/erode with a brush width of \code{dilateErodeBrush2} to capture small detections around colonies}
#'   \item{discard all colonies smaller than minColonySize}
#'   \item{fill any holes in contiguous areas}
#'   \item{label colonies}
#'   \item{remove colonies larger than \code{minColonySize * minSizeExpansion} as a precaution}
#' }
#' @return A \code{matrix} of integer labeled blobs.

solid_labelPhase <- function(image,
                             minColonySize=50,
                             minSizeExpansion=1.2,
                             detectionThreshold=0.6,
                             haloQuantile=0.98,
                             brightThreshold=0.8,
                             dilateErodeBrush1=7,
                             dilateErodeBrush2=3) {
  
  # Ensure that the greatest pixel value is 1. Some EBImage functions
  # break if pixels greater than 1 are encountered
  image[image > 1] <- 1
  
  # Find the edge detections for the given image with NO blur
  # and a scalar of 2. This increases the sensitivity to edges
  edges <- filter_sobel(image, FALSE, 2)
  
  # ClosingGreyScale first expands the radius of bright objects by 7 pixels
  # and then reduces it by 7. This has the effect of joining blobs together
  # and creating solid blobs where there may have been a weak signal.
  imageEdit <- EBImage::closingGreyScale(edges, EBImage::makeBrush(dilateErodeBrush1))
  
  # Mask the image, saving edge values greater than detectionThreshold as "true" and
  # values less than detectionThreshold as "false". Our assumption is that strong edges
  # equal bacteria so we keep everything above detectionThreshold.
  imageEdit <- imageEdit > detectionThreshold
  
  # Now equalize the original image, which flattens the value histogram.
  # This has the effect of turning bright areas extremely bright and dark areas
  # extremely dark. We then remove the very very brightest areas from the mask.
  # This is because solid images tend to be darkish blobs with a bright halo,
  # and we can use that halo to "carve out" fairly accurate outlines of blobs.
  imageEdit[EBImage::equalize(image) > haloQuantile] <- 0
  
  # Now add all of the brightest areas back into the image. This seems
  # counterintuitive since we just removed bright regions, but an equalized
  # image brightens everything to an extreme, even "sort of" bright areas.
  # It turns out that the actual brightest areas (in this case defined as
  # greater than brightThreshold) tend to be the centers of bacterial groups,
  # so we want to add those back in.
  imageEdit[image > brightThreshold] <- 1
  
  # Do a smaller closingGreyScale on the image. We want to pick up any
  # small areas around the blobs that should count as part of the same group
  # but had too weak of edges to be counted as such.
  imageEdit <- EBImage::closingGreyScale(imageEdit, EBImage::makeBrush(dilateErodeBrush2))
  
  # Blobs under 50 pixels are very likely to be noise and there tends to be
  # a lot of those. Removing those speeds up later steps of the algorithm.
  imageEdit <- removeBlobs(imageEdit, minColonySize)
  
  # We're only interested in outlines for solid images, so fill any
  # holes in the images.
  imageEdit <- EBImage::fillHull(imageEdit)
  
  # bwlabel() assigns each distinct blob of white pixels a unique integer
  # value, which we'll later use to identify and track the blobs.
  imageEdit <- EBImage::bwlabel(imageEdit)
  
  # Now remove larger than a slightly expanded size as a precaution.
  expandedColonySize <- minColonySize * minSizeExpansion
  imageEdit <- removeBlobs(imageEdit, expandedColonySize, label=FALSE)
  
  # For checking results during development
  if (FALSE) {
    display(overlayOutlines(image, imageEdit))
  }
  
  return(imageEdit)
  
}
