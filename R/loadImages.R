#' @export
#' @title Load Image Data into a List
#' @param dir path to the directory containing images
#' @param ext file extension
#' @param n number of images to load into memory (defaults to all)
#' @return A \code{list} with two attributes, "images" and "filenames".
#' Images is a \code{list} of image matrices extracted from the \code{EBImage}
#' \code{image} object. Filenames is a vector of filenames that correponds
#' to images.

loadImages <- function(dir, ext="tiff", n=NA, start=1) {
  
  readf <- function(im) {
    return(EBImage::readImage(im)@.Data)
  }
  
  # List all files
  files <- list.files(dir, pattern=paste0("\\.",ext,"$"), full.names=TRUE)
  
  # Jump to start
  files <- files[start:length(files)]
  
  # Subset files if necessary
  if (!is.na(n)) {
    files <- files[1:n]
  }
  
  # Function for extracting filenames
  parseFilenames <- function(file) {
    splt <- strsplit(file, "/")[[1]]
    return(strsplit(splt[length(splt)], "\\.")[[1]][[1]])
  }
  
  # Read each file and return the resulting list
  return(list(images=lapply(files, readf), labels=files))
         
}