#' @export
#' @title Identify and Label Phase Microscopy Groups
#' @param image an image matrix to search for cell colonies
#' @param artifactMask a mask of non biological features to ignore. See \link{createArtifactMask}.
#' @description Searches an image for dark cell colonies and incrementally labels each colony.
#' @return A \code{matrix} of integer labeled blobs.

flow_labelPhase <- function(image, artifactMask) {
  
  print("Searching new image...")
  ptm <- proc.time()
  
  # Calculate homogeneity
  homog <- glcm::glcm(image, n_grey=25, window=c(3,3), statistics=c("homogeneity"))[,,1]
  
  # This essentially finds edges
  homog <- (1-homog)^3
  
  # Apply artifact mask
  homog[artifactMask>0] <- 0
  
  # Close small gaps and invert
  homog <- 1 - (EBImage::closingGreyScale(homog, EBImage::makeBrush(7, shape='disc')) * 1.2)
  
  # Multiplying the edges by the original image darkens only the edges of the bacteria.
  # This reduces the chance of dark regions in the background being captured.
  imageEdit <- (homog * image)
  
  # Apply threshold
  imageEdit <- imageEdit < 0.25
  
  # Reapply artifact mask
  imageEdit[artifactMask>0] <- 0
  
  # Mild dilate/erode to close small gaps
  imageEdit <- EBImage::closingGreyScale(imageEdit, EBImage::makeBrush(7, shape='disc'))
  
  # Remove very small blobs
  imageEdit <- removeBlobs(imageEdit, 10)
  
  # Larger dilate/erode. This lets groups capture small bits that should
  # count as the same group.
  imageEdit <- EBImage::closingGreyScale(imageEdit, EBImage::makeBrush(11, shape='disc'))
  
  # Now remove medium blobs.
  imageEdit <- removeBlobs(imageEdit, 25)
  
  # Large dilate to garuntee all of the area around the bacteria is selected.
  imageEdit <- EBImage::dilateGreyScale(imageEdit, EBImage::makeBrush(15, shape='disc'))
  
  # Equalize m, which flattens the value histogram. This greatly increases the
  # contrast around the edges. Then select the lighter areas from that region and
  imageEdit[(EBImage::equalize(image)) > 0.5] <- 0
  
  # Remove noise
  imageEdit <- removeBlobs(imageEdit, 20)
  
  # Dilate to bring groups together
  imageEdit <- EBImage::dilateGreyScale(imageEdit, EBImage::makeBrush(3, shape='disc'))
  
  # Now label sections. It's important to label while still dilated so that labels
  # Are less sensitive.
  imageEdit <- EBImage::bwlabel(imageEdit)
  
  # Since sections are already labeled it's safe to remove a lot of the excess
  # White so we get a more accurate reading
  imageEdit[(EBImage::equalize(image)^0.5) > 0.5] <- 0
  
  imageEdit[artifactMask>0] <- 0
  
  print(proc.time() - ptm)
  
  return(imageEdit)
  
}