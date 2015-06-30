# WORK COMPUTER
dataDir <- "~/Desktop/TBData/solid/Time course"

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

images <- loadImageByXY(dataDir, "xy2", c("c1"), c("phase"), n=15)





#########################################
### TEST BLOB LABELING SOLD SUBSTRATE ###
#########################################

labelGroups <- function(image) {
  
  print("Searching new image...")

  homog <- glcm::glcm(image, statistics=c("mean", "contrast"), n_grey=16, window=c(5,5), shift=c(2,2))
  
  mask <- homog[,,1]*2 < 0.3
  
  test1 <- homog[,,2]
  test1[mask] <- 0
  
  test2 <- EBImage::closingGreyScale(test1, EBImage::makeBrush(7, shape='disc'))
  
  test3 <- test2 > 0.65
  
  test4 <- removeBlobs(test3, 25)
  
  test5 <- EBImage::dilateGreyScale(test4, EBImage::makeBrush(5, shape='disc'))
  
  test6 <- bwlabel(test5)
  
  test6[mask] <- 0
  
  return(test6)

}

test <- lapply(images[[1]], labelGroups)

output <- generateBlobTimeseries(test, minTimespan=3, maxDistance=60)

buildDirectoryStructure(output, images[[1]], test, list(),list(), c(1:14))
