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



brightnessScalar <- 7

equalizeImage <- function(im) {
  eq <- im * (0.45 / median(im))
  return(brightnessScalar * (eq - (0.45-sd(eq)*2)))
}

imagesPost <- lapply(images, equalizeImage)


#########################################
### TEST BLOB LABELING SOLD SUBSTRATE ###
#########################################

labelGroups <- function(image) {
  
  print("Searching new image...")

  homog <- glcm::glcm(image, statistics=c("contrast"), n_grey=20, window=c(3,3), shift=c(2,2), min_x=0.3)
  
  test1 <- EBImage::dilateGreyScale(homog[,,1], EBImage::makeBrush(9, shape='disc'))
  test2 <- EBImage::closingGreyScale(test1, EBImage::makeBrush(7, shape='disc'))
  
  test3 <- test2 > 0.75
  
  test4 <- removeBlobs(test3, 50)
  
  test5 <- test5 <- EBImage::bwlabel(test4)
  
  test6 <- test5
  test6[image < 0.3] <- 0
  
  
  test2 <- test1
  
  test3 <- EBImage::closingGreyScale(test2, EBImage::makeBrush(9, shape='disc'))
  
  test3 <- test3 > 0.6
  
  test4 <- removeBlobs(test3, 40)
  
  test5 <- EBImage::bwlabel(test4)
  
  test5[image < 0.4] <- 0
  
  test6 <- fillHull(test5)
  
  return(test6)
  
  
  ### METHOD 2
  
  test1 <- blur(image)
  
  homog <- glcm::glcm(test1, statistics=c("contrast"), n_grey=30, window=c(3,3), shift=c(1,1))
  
  test2 <- sobel(test1)
  
  test2 <- test1 > 0.6
  
  test3 <- EBImage::closingGreyScale(test2, EBImage::makeBrush(5, shape='disc'))

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



sobel <- function(im) {
  
  Gx = matrix(c(-1,0,1,-2,0,2,-1,0,1), nc=3, nr=3)
  Gy = matrix(c(1,2,1,0,0,0,-1,-2,-1), nc=3, nr=3)
  xx = filter2(im, Gx)
  yy = filter2(im, Gy)
  test <- (xx + yy)
  
}



blur <- function(im) {
  flo = makeBrush(11, shape='disc', step=FALSE)^2
  flo = flo/sum(flo)
  return(filter2(im,flo))
}
