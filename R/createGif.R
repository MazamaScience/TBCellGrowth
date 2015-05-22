#' @export
#' @title Creates a gif animation.
#' @param frames a sequential \code{list} of images.
#' @param filename name of output gif.
#' @param delay delay between each frame in milliseconds.
#' @param resize dimensions of gif frames as a percent of original image size.
#' @description Creates an animated .gif file from a list of images using ImageMagick.
#' @return none

# Accepts a list of matrices and creates a gif
createGif <- function(frames, filename, delay=15, rescale=100) {
  
  # Temporary directory location. We store frames here
  tempDir <- "temp2234g12hdq5gp/"
  
  # Create the directory. Hide warnings if it already exists.
  dir.create(tempDir, showWarnings = FALSE)
  
  # Save each frame as a .tiff file
  for (i in 1:length(frames)) {
    j <- ifelse(i<10,paste0("0",i),i)
    EBImage::writeImage(frames[[i]],paste0(tempDir,j,".tiff"))
  }
  
  # Convert frames to gif using a system call to ImageMagick
  system(paste0('convert -resize "', rescale, '%" -delay  ', delay, ' ', tempDir, '*.tiff ', filename))
  
  # Delete temporary directory
  unlink(tempDir, recursive=TRUE)
  
}