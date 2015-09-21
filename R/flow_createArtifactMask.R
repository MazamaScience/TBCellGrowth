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
  
  ptm <- proc.time()
  cat("\nMaking artifact mask...")
  
  # Find harshest edges
  edges <- filter_sobel(bg, blur=FALSE) > 0.5
  # Expand to close circles
  edges <- EBImage::dilateGreyScale(edges, EBImage::makeBrush(9, 'disc'))
  # Fill holes
  edges <- EBImage::fillHull(edges)
  edges <- EBImage::erodeGreyScale(edges, EBImage::makeBrush(5, 'disc'))
  
  
  # Label blobs
  labeled <- EBImage::bwlabel(edges)

  # Remove dark / bright lines
#   for (i in 1:max(labeled)) {
#     xRange <- diff(range(which(labeled==i, arr.ind=T)[,1]))
#     if (xRange > 300) edges[labeled==i] <- 0
#   }
#   
  edges <- removeBlobs(edges, 250)
  
  cat(paste0("\nArtifact mask created in ", formatTime(ptm)))
  
  return (edges > 0)
  
}
