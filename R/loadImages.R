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
#' ordered (e.g. with numeric indices). The return is a list of two multi-level lists.
#' 
#' The \code{imageList} is a list of image matrices obtained by reading in files with \code{EBImage::readImage()}
#' and then extractng the \code{.Data} slot of the EBImage Image object.
#' Images that could not be read in have an NA stored in their slot.
#' 
#' The organizational structure is:
#' 
#' \code{imageList[[channel]][[timestep]]}
#' 
#' @return A list of image matrices.

loadImages <- function(dataDir, chamber, channels=c("c1"), channelNames=c("phase"), ext="tiff", startFrame=1, n="all") {
  
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
  if ( n != "all" ) {
    timesteps <- timesteps[1:n]
  }
  
  # Initialize imageList
  imageList <- list()
  for (channel in channels) {
    
    imageList[[channel]] <- list()
    
    for (timestep in timesteps) {
      
      filename <- paste0(dataDir,"/",timestep,"/",chamber,channel,".",ext)
      if ( !file.exists(filename) ) {
        cat(paste0('\tWARNING: ',filename,' NOT FOUND\n'))
        imageList[[channel]][[timestep]] <- NA
        next
      }
      
      if (getRunOptions('verbose')) cat(paste0('\tLoading  ',filename,'\n'))

      result <- try( imageList[[channel]][[timestep]] <- round(readf(filename),4),
                     silent=TRUE )
      
      if ( class(result)[1] == "try-error" ) {
        err_msg <- geterrmessage()
        if ( stringr::str_detect(err_msg,"object must be an array") ) {
          cat(paste0('\tWARNING: ',filename,' CANNOT BE READ\n'))
        }
        imageList[[channel]][[timestep]] <- NA
      }
      
    }
    
  }
  
  # NOTE:  The first channel will always be called "phase" throughout the package, regardless of the
  # NOTE:  channelNames passed in. If the user passes in "phase1" or "phase2" this will only
  # NOTE:  be used in the final creation of directories.
  # NOTE:
  # NOTE:  The dye names, however, will be used as passed in.
  
  # Override first channelName
  channelNames[1] <- "phase"
  names(imageList) <- channelNames
  
  return(imageList)
  
}