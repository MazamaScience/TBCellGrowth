#' @export
#' @title Load Image Data into a List
#' @param dir path to the directory containing images
#' @param ext file extension
#' @param n number of images to load into memory (defaults to all)
#' @param start index at which to start loading files
#' @description This function uses the \pkg{EBImage::readimage} function
#' to read in a series of images. Filenames for images are assumed to be
#' ordered (e.g. with numeric indices). The return list contains
#' two attributes:
#' \itemize{
#' \item{images -- a \code{list} of image matrices extracted from the \code{EBImage}
#' \code{image} object.}
#' \item{filenames -- a vector of filenames that correponds
#' to the images.}
#' }
#' 
#' The individual images are simple 2D matrices.
#' @return A \code{list} with two attributes, "images" and "filenames".
#' @examples
#' \dontrun{
#' phase <- loadImages(dataDirPhase, n=30, ext="tif")
#' }

loadImages <- function(dir, ext="tiff", n=NA, start=1) {
  
  readf <- function(filename) {
    return(EBImage::readImage(filename)@.Data)
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
  return(list(images=lapply(files, readf), filenames=files))
         
}