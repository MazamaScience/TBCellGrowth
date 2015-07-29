resetImages <- function(dir) {
  
  print(paste0("diving into ", dir, "..."))
  
  dirs <- list.dirs(dir, full.names=TRUE)[-1]
  files <- list.files(dir, pattern=".tif", full.names=TRUE)
  
  for (dir in dirs) {
    resetImages(dir)
  }
  
  for (file in files) {
    image <- EBImage::readImage(file)
    file.remove(file)
    file <- strsplit(file, "\\.")[[1]]
    EBImage::writeImage(image, paste0(file[1],".jpg"))
  }
  
  
}