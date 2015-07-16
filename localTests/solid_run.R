
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

### Trying to replicate the "auto tone" function in 
### Photoshop CS6

# PS Level adjustments           #   closest quantile
# phase[[1]]   0.3529 - 0.4980   #   50% - 2 * 25%
# phase[[2]]   0.4275 - 0.5647   #   20% - 2 * 
# phase[[4]]   0.4627 - 0.5765
# phase[[10]]  0.4667 - 0.5882
equalizeImage <- function(im) {
  im <- im - median(im)
#   im <- im - quantile(im, probs=seq(0,1,0.1))[[6]]
#   im <- im / max(im)
#   im[im<0] <- 0
#   im <- im / ( 7 * quantile(im)[[4]] )
#   return(im)
#   test <- test / (quantile(test)[[4]]^0.5)
}

im <- phase[[2]]
im <- im - median(im)
im[im < 0] <- NA
im <- im / max(im, na.rm=TRUE)
im <- im / (quantile(im, probs=seq(0,1,0.05), na.rm=TRUE)[[19]] * 2)

