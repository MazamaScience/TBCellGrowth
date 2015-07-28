
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

params$startTime <- 0 # Time of first image
params$timestep <- 3 # Timestep in hours

# How many frames to load
params$nFrames <- 10

# What file extension to read
params$extension <- "tif"


images <- loadImages(params$inputDir, params$xy, params$channels,
                           params$channelNames, params$extension, n=params$nFrames)

for (xyName in names(images)) {
  
  xy <- images[[xyName]]
  xy$phase <- lapply(xy$phase, solid_equalizeImages)
  
  
  xy <- solid_alignImages(xy)
  
  xy.labeled <- list()
  xy.labeled$phase <- lapply(xy$phase, solid_labelPhase)
  
  output <- generateBlobTimeseries(xy.labeled$phase, minTimespan=7)
  
  # Generate filenames from timestamps
  # Assuming hours < 1000
  filenames <- params$startTime + ((0:(params$nFrames-1))*params$timestep)
  filenames <- unlist(lapply(filenames, function(x) if(x<10) paste0("00",x) else if(x<100) paste0("0",x) else x))
  
  # Apply timesteps to row names of timeseries
  rownames(output$timeseries) <- filenames
  
  
  
#   outlines <- mapply(overlayOutlines, xy$phase, xy.labeled$phase, SIMPLIFY=FALSE)
#   lapply(outlines, display)
  
}
