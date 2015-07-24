
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

params$xy <- c("xy2")             # Single section to look at
params$channels <- c("c1")         # One or more channels to look at, c1 required
params$channelNames <- c("phase")    # Names of channels, 'phase' is required

# How many frames to load
params$nFrames <- 10

# What file extension to read
params$extension <- "tif"


images <- loadImages(params$inputDir, params$xy, params$channels,
                           params$channelNames, params$extension, n=params$nFrames)

for (xyName in names(images)) {
  
  xy <- images[[xyName]]
  test <- lapply(xy$phase, solid_equalizeImages)
  
  xy.labeled <- list()
  xy.labeled$phase <- solid_labelPhase(xy$phase)
}
