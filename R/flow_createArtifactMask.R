#' @export
#' @title Create a Mask of non-Biological Artifacts
#' @param bg preprocessed background image
#' @param maskDarkLines logical specifying whether or not dark lines should be masked out
#' @description Fluidomics apparatus used in phase microscopy have a number of physical
#' features that must be masked out before image processing algorithms to identify bacteria
#' are applied. This function is tailored to remove *pillars*, *squares* and the dark lines
#' that appear between different levels in the apparatus.
#' @return A \code{matrix} of boolean values with TRUE values identifying non biological artifacts. 

flow_createArtifactMask <- function(bg, maskDarkLines=FALSE) {
  
  # Find harshest edges
  edges <- filter_sobel(bg, blur=FALSE, 1)
  edges <- edges > 0.4

    # Expand to close circles
  edges <- EBImage::dilateGreyScale(edges, EBImage::makeBrush(9, 'disc'))

    # Fill holes
  edges <- EBImage::fillHull(edges)
  edges <- EBImage::erodeGreyScale(edges, EBImage::makeBrush(5, 'disc'))
  
  # Remove blobs smaller than threshold
  edges <- removeBlobs(edges, 250)
  
  return (edges > 0)
  
}
