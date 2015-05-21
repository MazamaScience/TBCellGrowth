#' Searches an image for dark cell colonies and incrementally labels each colony.
#' @export
#' @param frame an image matrix to search for cell colonies
#' @param artifactMask a mask of non biological features to ignore. \link{createArtifactMask}
#' @return A \code{matrix} of integer labeled blobs.

labelColonies <- function(frame, artifactMask) {
  
  print("Searching new frame...")
  ptm <- proc.time()
  
  # Calculate homogeneity
  homog <- glcm::glcm(frame, n_grey=25, window=c(3,3), statistics=c("homogeneity"))[,,1]
  
  # This essentially finds edges
  homog <- (1-homog)^3
  
  # Apply artifact mask
  homog[artifactMask>0] <- 0
  
  # Close small gaps and invert
  homog <- 1 - (closingGreyScale(homog, makeBrush(7, shape='disc')) * 1.2)
  
  # Multiplying the edges by the original image darkens only the edges of the bacteria.
  # This reduces the chance of dark regions in the background being captured.
  frameEdit <- (homog * frame)
  
  # Apply threshold
  frameEdit <- frameEdit < 0.25
  
  # Reapply artifact mask
  frameEdit[artifactMask>0] <- 0
  
  # Mild dilate/erode to close small gaps
  frameEdit <- closingGreyScale(frameEdit, makeBrush(7, shape='disc'))
  
  # Remove very small blobs
  frameEdit <- removeBlobs(frameEdit, 10)
  
  # Larger dilate/erode. This lets groups capture small bits that should
  # count as the same group.
  frameEdit <- closingGreyScale(frameEdit, makeBrush(11, shape='disc'))
  
  # Now remove medium blobs.
  frameEdit <- removeBlobs(frameEdit, 25)
  
  # Large dilate to garuntee all of the area around the bacteria is selected.
  frameEdit <- dilateGreyScale(frameEdit, makeBrush(15, shape='disc'))
  
  # Equalize m, which flattens the value histogram. This greatly increases the
  # contrast around the edges. Then select the lighter areas from that region and
  frameEdit[(equalize(frame)) > 0.5] <- 0
  
  # Remove noise
  frameEdit <- removeBlobs(frameEdit, 20)
  
  # Dilate to bring groups together
  frameEdit <- dilateGreyScale(frameEdit, makeBrush(3, shape='disc'))
  
  # Now label sections. It's important to label while still dilated so that labels
  # Are less sensitive.
  frameEdit <- bwlabel(frameEdit)
  
  # Since sections are already labeled it's safe to remove a lot of the excess
  # White so we get a more accurate reading
  frameEdit[(equalize(frame)^0.5) > 0.5] <- 0
  
  frameEdit[artifactMask>0] <- 0
  
  print(proc.time() - ptm)
  
  return(frameEdit)
  
}

# Remove all blobs under a certain size threshold from a matrix.
# Assumes blobs are incrementally labeled. 
removeBlobs <- function(m, size) {
  # Label blobs
  m <- bwlabel(m)
  # Save dimensions
  dims <- dim(m)
  # Unravel matrix and count the occurances of each label
  sorted <- sort(table(as.numeric(m)))
  # Labels which are under the threshold
  small <- as.numeric(names(sorted[sorted<size]))
  # Remove pixels that fall into undersized labels
  m[m %in% small] <- 0
  # Turn back into matrix
  m <- matrix(m, nrow=dims[[1]], ncol=dims[[2]])
  return(m)
}