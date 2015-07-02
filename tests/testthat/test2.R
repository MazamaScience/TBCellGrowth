# WORK COMPUTER
dataDir <- "~/Desktop/TBData/solid/Time course"

# LAPTOP
dataDir <- "~/Desktop/tbtest/solid/Time course"

xy <- "xy2"             # Single section to look at
channels <- c("c1")     # One or more channels to look at, c1 required
cnames <- c("phase")    # Names of channels, 'phase' is required

loadImageByXY <- function(dataDir, xy, channels, cnames, ext="tif", start=1, n=NA) {
  
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
  
  for (channel in channels) {
    images[[channel]] <- lapply(times, function(t) readf(paste0(dataDir,"/",t,"/",xy,channel,".",ext)))
  }
  
  return(images)
  
}

images <- loadImageByXY(dataDir, "xy2", c("c1"), c("phase"))
filenames <- images[[2]]
images <- images[[1]]

images <- lapply(images, function(x) x * (0.45 / mean(x)))


equalizeImage <- function(im) {
  return(7.5 * (im - (mean(im)-0.02)))
}

t1 <- lapply(images, equalizeImage)


#########################################
### TEST BLOB LABELING SOLD SUBSTRATE ###
#########################################

labelGroups <- function(image) {
  
  print("Searching new image...")

  homog <- glcm::glcm(image, statistics=c("contrast"), n_grey=30, window=c(3,3), shift=c(2,2), min_x=0.3)
  
  test1 <- image * homog[,,1]
  
  test2 <- test1 > 0.7
  
  test3 <- EBImage::closingGreyScale(test2, EBImage::makeBrush(7, shape='disc'))
  
  test4 <- removeBlobs(test3, 40)
  
  test5 <- EBImage::bwlabel(test4)
  
  test5[image < 0.4] <- 0
  
  test6 <- fillHull(test5)
  
  return(test6)

}

test <- lapply(images, labelGroups)

output <- generateBlobTimeseries(test, minTimespan=3, maxDistance=60)

buildDirectoryStructure(output, images[[1]], test, list(),list(), c(1:14))




x <- paintObjects(test[[1]],t1,col='white')

f1 <- function(mask, image) {
  im = channel(Image(image), 'rgb')
  nuch1 = paintObjects(mask, im, thick=TRUE)
}
outlined <- mapply(f1, test, images, SIMPLIFY=FALSE)

image <- images[[1]]
mask <- test[[1]]

nucgray = channel(Image(image), 'rgb')
nuch1 = paintObjects(mask, nucgray, thick=TRUE)
