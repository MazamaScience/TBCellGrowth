
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


images <- solid_loadImages(params$inputDir, params$xy, params$channels,
                           params$channelNames, params$extension, n=params$nFrames)

phase <- images$xy2$phase

equalizeImage <- function(im) {

  im <- im - quantile(im, probs=seq(0,1,0.1))[[6]]
  im <- im / max(im)
  im[im<0] <- 0
  im <- im / ( 7 * quantile(im)[[4]] )
  return(im)
#   test <- test / (quantile(test)[[4]]^0.5)
}

test <- lapply(phase, equalizeImage)


brightnessScalar <- 7

equalizeImage <- function(im) {
  im <- im - min(im)
  im <- im / max(im)
  return(im)
}
