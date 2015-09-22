#' @export
#' @title Load Image Data into a List
#' @param dataDir path to the directory containing images
#' @param chamber name of the chamber whose images are loaded e.g. "xy2"
#' @param channels which channels to load for the given chamber
#' @param channelNames a vector of names to be associated with the the given channels
#' @param ext file extension
#' @param startFrame index at which to start loading files
#' @param n number of images to load into memory
#' @description This function uses the \pkg{EBImage::readimage} function
#' to read in a series of images. Filenames for images are assumed to be
#' ordered (e.g. with numeric indices). The return is a multi-level list
#' of image matrices obtained by reading in files with \code{EBImage::readImage()}
#' and then extractng the \code{.Data} slot of the EBImage Image object.
#' 
#' The organizational structure is:
#' 
#' \code{images[[channel]][[timestep]]}
#' 
#' @return A list of image matrices.

loadImages <- function(dataDir, chamber, channels=c("c1"), channelNames=c("phase"), ext="tiff", startFrame=1, n=NULL) {
  
  # NOTE:  Suppress warnings to avoid seeing:
  # NOTE:
  # NOTE:  1: In readTIFF(x, all = all, ...) :
  # NOTE:    TIFFReadDirectory: Unknown field with tag 65325 (0xff2d) encountered
  
  # Simple function to return the data portion of an image
  readf <- function(im) {
    return(suppressWarnings(EBImage::readImage(im)@.Data))
  }
  
  # Sanity check
  if (is.null(dataDir) || is.null(chamber)) {
    stop(paste0('dataDir and chamber must both be specified.'))
  }
  
  # Get all time folders
  if ( !file.exists(dataDir) ) {
    stop(paste0('dataDir: "',dataDir,'" is not found.'))
  } else {
    timesteps <- list.files(dataDir)
  }
  
  # Jump to start
  timesteps <- timesteps[startFrame:length(timesteps)]
  
  # Subset timesteps if necessary
  if ( !is.null(n) ) {
    timesteps <- timesteps[1:n]
  }
  
  # Initialize images
  images <- list()
  
  for (channel in channels) {
    for (timestep in timesteps) {
      filename <- paste0(dataDir,"/",timestep,"/",chamber,channel,".",ext)
      if (getRunOptions('verbose')) {
        cat(paste0('\tLoading ',filename,'\n'))
      }
      images[[channel]][[timestep]] <- round(readf(filename),4)
    }
  }
  names(images) <- channelNames
  
  return(images)
  
}