#' Load frames from a directory.
#' @export
#' @param dir A path to the directory containing image frames.
#' @param n The number of frames to load into memory. Loads all by default.
#' @param ext file extension, "tiff" by default.
#' @return A \code{list} of \code{matrices} extracted from the \code{EBImage}
#' \code{image} object.

loadFrames <- function(dir, ext, n=NA) {
  
  readf <- function(im) {
    return(EBImage::readImage(im)@.Data)
  }
  
  # List all files
  files <- list.files(dir, pattern=paste0("\\.",ext,"$"), full.names=TRUE)
  
  # Subset files if necessary
  if (!is.na(n)) {
    files <- files[1:n]
  }
  
  # Read each file and return the resulting list
  return(lapply(files, readf))
  
}