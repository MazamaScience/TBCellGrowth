
params <- list()

## WORK COMPUTER
if (FALSE) {
  params$inputDir <- "~/Desktop/TBData/solid/Time course"
  params$outputDir <- "~/Desktop/outputSolid/"
}

## WILL'S LAPTOP
if (FALSE) {
  params$inputDir <- "~/Desktop/tbTest/solid/Time course"
  params$outputDir <- "~/Desktop/outputSolid/"
}

params$xy <- c("xy2","xy3")             # Single section to look at
params$channels <- c("c1")         # One or more channels to look at, c1 required
params$channelNames <- c("phase")    # Names of channels, 'phase' is required

# How many frames to load
params$nFrames <- 10

# What file extension to read
params$extension <- "tif"


images <- loadImages(params$inputDir, params$xy, params$channels,
                           params$channelNames, params$extension, n=params$nFrames)

phase <- images$xy2$phase

### Trying to replicate the "auto tone" function in 
### Photoshop CS6
equalizeImage <- function(im) {
  
  # Make histogram of values
  valueHist <- hist(im, breaks=40)
  # Which index of histogram is highest
  index <- which.max(valueHist$counts) - 1
  # What value corresponds to that
  minVal <- valueHist$breaks[index]
  # Shift and stretch the image so the new minimum is this 
  # minVal and the max is 1
  im <- im - minVal
  im[im < 0] <- NA
  im <- im / max(im, na.rm=TRUE)
  # Make another histogram
  valueHist <- hist(im, breaks=40)
  index <- which(valueHist$counts/max(valueHist$counts) < 0.1 & c(FALSE,diff(valueHist$counts) < 0))[[1]]
  medianVal <- valueHist$breaks[index]
  # medianVal <- valueHist$breaks[which.min(diff(valueHist$counts)) + 1]
  im <- im / (medianVal * 2)
  im[im > 1] <- 1
  return(im)
  
}

test <- lapply(phase, equalizeImage)