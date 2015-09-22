#' @export
#' @title Creates a gif animation.
#' @param dir the directory of images to read
#' @param filename name of output gif.
#' @param ext the image file extension to read.
#' @param framerate the number of frames per second in the output gif.
#' @param rescale dimensions of gif frames as a percent of original image size.
#' @description Creates an animated .gif file from a list of images using ImageMagick.
#' @return none

createGif <- function(dir, filename, ext="jpg", framerate=2, rescale=80) {
  delay <- 100 / framerate
  os = Sys.info()['sysname'][[1]]
  if (os=="Darwin") system(paste0('convert -resize "', rescale, '%" -delay  ', delay, ' "', dir, '"*.', ext, ' "', dir, '"/', filename))
  if (os=="Windows") shell(paste0('convert -resize "', rescale, '%" -delay  ', delay, ' "', dir, '"*.', ext, ' "', dir, '"/', filename))
}

# Accepts a list of matrices and creates a gif
createGifFromList <- function(images, filename, delay=15, rescale=100) {
  
  # Temporary directory location. We store images here
  tempDir <- "temp2234g12hdq5gp/"
  
  # Create the directory. Hide warnings if it already exists.
  dir.create(tempDir, showWarnings = FALSE)
  
  # Save each frame as a .tiff file
  for (i in 1:length(images)) {
    j <- ifelse(i<10,paste0("0",i),i)
    images[[i]][is.na(images[[i]])] <- 0
    EBImage::writeImage(images[[i]],paste0(tempDir,j,".tiff"))
  }
  
  # Convert images to gif using a system call to ImageMagick
  system(paste0('convert -resize "', rescale, '%" -delay  ', delay, ' ', tempDir, '*.tiff ', filename))
  
  # Delete temporary directory
  unlink(tempDir, recursive=TRUE)
  
}