#' @export
#' @title Load Images into a List
#' @param dir path to the directory containing images
#' @param ext file extension
#' @param n number of images to load into memory (defaults to all)
#' @return A \code{list} of \code{matrices} extracted from the \code{EBImage}
#' \code{image} object.

loadImages <- function(dir, ext="tiff", n=NA) {
  
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