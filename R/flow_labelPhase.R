#' @export
#' @title Identify and Label Phase Microscopy Groups
#' @param image an image matrix to search for cell colonies
#' @param artifactMask a mask of non biological features to ignore. See \link{flow_createArtifactMask}.
#' @param ignoredRegions a vector of row numbers to ignore. Blobs which have centroids in this range are removed.
#' @param minColonySize all identified groups of pixels, aka "blobs", below this size are discarded
#' @param minSizeExpansion used to determine size threshold in final removal of "junk"
#' @param detectionThreshold brightness level above which colonies are detected
#' @param haloQuantile brightness level of equalized image above which pixels are considred to be a colony "halo"
#' @param brightThreshold brightness level above which pixels ARE NOT considered part of a colony (different from solid_labelPhase)
#' @param dilateErodeBrush1 size of brush used to convert edges into full colonies
#' @param dilateErodeBrush2 size of brush used to merge nearby pixels that are likely part of the same colony
#' @description Searches an image for dark cell colonies and incrementally labels each colony.
#' @note A lot of "photoshop magic" happens here and the algorithm has several arguments
#' that might need to be adjusted if the quality of images changes significantly. The algorithm 
#' was tailored to the "phase1" channel of micro-fluidics images that have the appearance of
#' dark cell colonies with a light "halo".
#' 
#' This algorithm is inappropriate for imagery that does not have this appearance.
#' 
#' Algorithm steps include:
#' \enumerate{
#'   \item{replace pixels identified in the \code{artifactMask} with medium gray (background)}
#'   \item{edge detection with a Sobel filter}
#'   \item{dilate/erode with a brush width of \code{dilateErodeBrush1} to get contiguous areas identified as colonies}
#'   \item{mark pixels above \code{detectionThreshold} as colonies (white)}
#'   \item{change the very brightest pixels (above \code{haloQuantile}) to black to get crsip edges}
#'   \item{dilate/erode with a brush width of \code{dilateErodeBrush2} to capture small detections around colonies}
#'   \item{discard all colonies smaller than \code{minColonySize}}
#'   \item{label colonies}
#'   \item{remove colonies in ignored regions}
#'   \item{change all pixels above \code{brightThreshold} to black to more closely "hug" colonies}
#'   \item{remove colonies larger than \code{minColonySize * minSizeExpansion} as a precaution}
#' }
#' @return A \code{matrix} of integer labeled blobs.

flow_labelPhase <- function(image, artifactMask, ignoredRegions,
                            minColonySize=100,
                            minSizeExpansion=1.75,
                            detectionThreshold=0.5,
                            haloQuantile=0.8,
                            brightThreshold=0.775,
                            dilateErodeBrush1=7,
                            dilateErodeBrush2=3) {
  
  # Ensure that the greatest pixel value is 1. Some EBImage functions
  # break if pixels greater than 1 are encountered
  image[image > 1] <- 1
  
  # The imageMask is a duplicate of the original image with the artifact mask
  # applied to do. Areas inside the artifact mask are replaced with a reasonable
  # quantile value intended to make them blend in with the background (or at least
  # have very weak edges)
  imageMask <- image
  imageMask[artifactMask > 0] <- quantile(image, seq(0,1,0.05), na.rm=T)[[12]]
  
  # Find edges on the imageMask. This should mainly just have strong edges for
  # blobs since the effect of the artifacts was muted.
  imageEdit <- filter_sobel(imageMask, FALSE, 2)
  
  # ClosingGreyScale first expands the radius of bright objects by 7 pixels
  # and then reduces it by 7. This has the effect of joining blobs together
  # and creating solid blobs where there may have been a weak signal.
  imageEdit <- EBImage::closingGreyScale(imageEdit, EBImage::makeBrush(dilateErodeBrush1))
  
  # Mask the image, saving edge values greater than detectionThreshold as "true" and
  # values less than detectionThreshold as "false". Our assumption is that strong edges
  # equal bacteria so we keep everything above detectionThreshold.
  imageEdit <- imageEdit > detectionThreshold
  
  # Now equalize the original image, which flattens the value histogram.
  # This has the effect of turning bright areas extremely bright and dark areas
  # extremely dark. We then remove the very very brightest areas from the mask.
  # This is because flow images tend to be darkish blobs with a bright halo,
  # and we can use that halo to "carve out" fairly accurate outlines of blobs.
  imageEdit[EBImage::equalize(imageMask) > haloQuantile] <- 0
  
  # Expand the white blobs by 3 pixels. At this stage we're just trying to get
  # a general region of what is blob and what is background. Finer edge detection
  # comes later
  imageEdit <- EBImage::dilateGreyScale(imageEdit, EBImage::makeBrush(dilateErodeBrush2))
  
  # ???
  imageEdit[EBImage::dilateGreyScale(artifactMask, EBImage::makeBrush(dilateErodeBrush2)) > 0] <- 0
  
  # Blobs under 100 pixels are very likely to be noise and there tends to be
  # a lot of those. This is especially true since we expanded the radius.
  # Removing those speeds up later steps of the algorithm.
  imageEdit <- removeBlobs(imageEdit, minColonySize)
  
  # blabel assigns each distinct blob of white pixels a unique integer
  # value, which we'll later use to identify and track the blobs.
  imageEdit <- EBImage::bwlabel(imageEdit)
  
  # Now we remove blobs whose centroids fall inside of the "ignore" region.
  # We do this because it's too difficult to pick out good outlines in 
  # certain regions of the image, namely the dark lines in between plate
  # levels.
  
  # Find the centroids for all blobs
  centroids <- getCentroids(imageEdit)
  # Find the indices of blobs who fall into the ignord regions
  toRemove <- removeIgnored(centroids, ignoredRegions)
  # Remove those blobs
  imageEdit[!(imageEdit %in% toRemove)] <- 0
  
  # Do a final "carving" with a slightly less bright equalized image.
  # This makes the edges much finer. It's safe to assume that bacteria 
  # in the microfluidics images will not be brightly lit
  imageEdit[EBImage::equalize(imageMask) > brightThreshold] <- 0
  
  # Now remove larger than a slightly expanded size as as a precaution.
  # NOTE:  Experience has shown that 175% of minColonySize is a reasonable size at this stage.
  expandedColonySize <- minColonySize * minSizeExpansion
  imageEdit <- removeBlobs(imageEdit, expandedColonySize, label=FALSE)
  
  return(imageEdit)
  
}

removeIgnored <- function(df, ignoredRegions) {
  remove <- apply(ignoredRegions, 1, function(ig) (df$x > ig[[1]]) & (df$x < ig[[2]]) & (df$y > ig[[3]]) & (df$y < ig[[4]]))
  remove <- apply(remove, 1, function(x) sum(x) < 1)
  return(df[remove,]$index)
} 

