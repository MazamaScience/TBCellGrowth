#' @export
#' @title Load Image Data into a List
#' @param dataDir path to the directory containing images
#' @param xy the xy number to load e.g. "xy2"
#' @param channels which channels to load for the given xy
#' @param channelNames a vector of names corresponding to the
#' given channels
#' @param ext file extension
#' @param start index at which to start loading files
#' @param n number of images to load into memory (defaults to all)
#' @description This function uses the \pkg{EBImage::readimage} function
#' to read in a series of images. Filenames for images are assumed to be
#' ordered (e.g. with numeric indices). The return is a list of image
#' matrices from the \code{.Data} slot of the EBImage Image object.

solid_loadImages <- function(dataDir, xy, channels=c("c1"), channelNames=c("phase"), ext="tiff", start=1, n=NA) {
  
  readf <- function(im) {
    return(EBImage::readImage(im)@.Data)
  }
  
  # List all time folders
  times <- list.files(dataDir)
  
  # Jump to start
  times <- times[start:length(times)]
  
  # Subset times if necessary
  if (!is.na(n)) {
    times <- times[1:n]
  }
  
  # Initialize images
  images <- list()
  
  for (xyn in xy) {
    images[[xyn]] <- list()
    for (channel in channels) {
      images[[xyn]][[channel]] <- lapply(times, function(t) readf(paste0(dataDir,"/",t,"/",xy,channel,".",ext)))
    }
    names(images[[xyn]]) <- channelNames
  }
  
  return(images)
  
}