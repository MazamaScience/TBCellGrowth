#' @export
#' @title Identify and Label Florescent Dye
#' @param image an image matrix to search for dye
#' @param phase.labeled a list of labeled phase images from
#' \link{flow_labelPhase}
#' @param artifactMask a mask of non biological features to ignore. See \link{flow_createArtifactMask}.
#' @description Searches an image for dark cell colonies and incrementally labels each blob.
#' @return A \code{matrix} of integer labeled blobs.

flow_labelDye <- function(image, phase.labeled, artifactMask) {

  print("Searching new image...")
  ptm <- proc.time()
  
  # Threshold
  imageEdit <- image > 0.3
  
  # Dilate groups slightly too large for more inclusive labeling
  imageEdit <- EBImage::dilateGreyScale(imageEdit, EBImage::makeBrush(9, shape="disc"))
  imageEdit <- EBImage::erodeGreyScale(imageEdit, EBImage::makeBrush(5, shape="disc"))
  
  # Mask out artifacts
  imageEdit[artifactMask>0] <- 0
  
  # Label blobs
  imageEdit <- EBImage::bwlabel(imageEdit)
  
  # Shrink blobs to a more accurage size
  imageEdit[image < 0.3] <- 0
  
  dcp <- imageEdit
  for (i in 1:max(imageEdit)) {
    overlap <- phase.labeled[imageEdit == i]
    dcp[imageEdit == i] <- 0
    # If at least 20% overlap
    if (sum(overlap>0) > length(overlap)/5) {
      overlap <- overlap[overlap>0]
      unq <- unique(overlap)
      index <- unq[which.max(tabulate(match(overlap, unq)))]
      dcp[imageEdit == i] <- index
    }
  }
  
  return(dcp)
   
}
