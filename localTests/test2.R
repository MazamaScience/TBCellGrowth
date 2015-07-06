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
  return(brightnessScalar * (eq - (0.45-0.02)))
}

imagesPost <- lapply(images, equalizeImage)


#########################################
### TEST BLOB LABELING SOLD SUBSTRATE ###
#########################################

labelGroups <- function(image) {
  
  print("Searching new image...")
  
  im1 <- blur(image)
  test <- sobel2(im1)
  test2 <- test > 0.4
  test3 <- EBImage::closingGreyScale(test2, EBImage::makeBrush(9, shape='disc'))
  test3[image < 0.4] <- 0
  test4 <- EBImage::dilateGreyScale(test3, EBImage::makeBrush(5, shape='disc'))
  test5 <- EBImage::fillHull(test4)
  test6 <- removeBlobs(test5, 50)
  test6 <- EBImage::bwlabel(test5)

  return(test6)
}

imagesLabeled <- lapply(imagesPost, labelGroups)

output <- generateBlobTimeseries(test, minTimespan=3, maxDistance=60)

buildDirectoryStructure(output, images[[1]], test, list(),list(), c(1:14))




f1 <- function(mask, image) {
  im = channel(Image(image), 'rgb')
  nuch1 = paintObjects(mask, im, thick=TRUE)
}
outlined <- mapply(f1, imagesLabeled, imagesPost, SIMPLIFY=FALSE)







blur <- function(im) {
  flo = makeBrush(3, shape='disc', step=FALSE)^2
  flo = flo/sum(flo)
  return(filter2(im,flo))
}

# based on photoshop and imagejay implementation
# http://imagejdocu.tudor.lu/doku.php?id=faq:technical:what_is_the_algorithm_used_in_find_edges
sobel2 <- function(im1) {
  
  # Shift up
  p1 <- cbind(rbind(im1,0,0),0,0)
  p2 <- cbind(0,rbind(im1,0,0),0)
  p3 <- cbind(0,0,rbind(im1,0,0))
  
  # Shift center
  p4 <- cbind(rbind(0,im1,0),0,0)
  p6 <- cbind(0,0,rbind(0,im1,0))
  
  # Shift down
  p7 <- cbind(rbind(0,0,im1),0,0)
  p8 <- cbind(0,rbind(0,0,im1),0)
  p9 <- cbind(0,0,rbind(0,0,im1))
  
  im2 <- cbind(0,rbind(0,im1,0),0)
  
  sum1 <- p1 + 2*p2 + p3 - p7 - 2*p8 - p9
  sum2 <- p1  + 2*p4 + p7 - p3 - 2*p6 - p9
  
  sum <- sqrt(sum1*sum1 + sum2*sum2)[c(-1, -dim(sum1)[[1]]),c(-1, -dim(sum1)[[2]])]
  sum[1:3,] <- 0
  sum[,1:3] <- 0
  sum[(dim(sum)[[1]]-3):(dim(sum)[[1]]),] <- 0
  sum[,(dim(sum)[[2]]-3):(dim(sum)[[2]])] <- 0
  
  return(sum)
  
}



fx <- matrix(c(-1,-2,-1,0,0,0,1,2,1), ncol=3, byrow=TRUE)
