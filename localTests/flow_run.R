
params <- list()

# Input and output directories
params$inputDir <- "localData/fluid/Time Course"
params$backgroundDir <- "localdata/fluid/Background"
params$outputDir <- "~/Desktop/outputSolid/"

# Configure which dyes to use,
params$xy <- c("xy06")             # Single section to look at
params$channels <- c("c1","c4")         # One or more channels to look at, c1 required
params$channelNames <- c("phase","green")    # Names of channels, 'phase' is required

# How many frames to load
params$nFrames <- 10 # How many frames to load

# What file extension to read
params$extension <- "jpg"

# How to scale phase and dye
params$phaseMedian <- 0.4 # What value phase images should be equalized to

# Image alignment
params$numTargets <- 12 # How many target features to use for alignment
params$targetWidth <- 30 # How large of a region the targets should be
params$searchSpace <- 30 # How far left, top, right, down to search fo alignment

params$startTime <- 0 # Time of first image
params$timestep <- 3 # Timestep in hours

# How many frames a blob must be in to be included in output
params$minTimespan <- 5

# Which regions to ignore for various reasons
# Mainly used to deal with poor microscope shifting
params$ignoreSections <- list(xy06=c("topLeft"))





# Load images
images <- loadImages(params$inputDir, params$xy, params$channels,
                           params$channelNames, params$extension, n=params$nFrames)

# Load background images
backgrounds <- loadImages(params$backgroundDir, params$xy, params$channels,
                                params$channelNames, params$extension)

### Merge backgrounds into images list
for (xy in names(images)) {
  for (dye in names(images[[xy]])) {
    images[[xy]][[dye]] <- c(backgrounds[[xy]][[dye]], images[[xy]][[dye]]) 
  }
}
# Clear large objects out of memory
rm(backgrounds)
rm(dye)
rm(xy)



# for output, handle each xy region at a time
for (xyName in names(images)) {
  
  xy <- images[[xyName]]
  
  # Equalize phase images
  xy$phase <- lapply(xy$phase, flow_equalizeImages, params$phaseMedian)
  
 
  
  # Rotate and align all channels
  xy <- flow_rotateImages(xy)
  xy <- flow_alignImages(xy,
                         numTargets=params$numTargets,
                         targetWidth=params$targetWidth, 
                         searchSpace=params$searchSpace)
  
  artifactMask <- flow_createArtifactMask(xy$phase[[1]], TRUE)

  # Interpret ignore regions as pixels
  ignoredRegions <- flow_findIgnore(params$ignore[[xyName]], dim(xy$phase[[1]]))
  # Find dark line areas to ignore
  darkLines <- flow_findDarkLines(xy$phase[[1]])
  # Combine these two into a final ignore list
  ignoredRegions <- rbind(ignoredRegions, darkLines)
  
  xy.labeled <- list()
  xy.labeled$phase <- lapply(xy$phase, flow_labelPhase, artifactMask, ignoredRegions)
  
  # Equalize and label non-phase images
  for (channel in names(xy)[-(names(xy) == "phase")]) {
    xy[[channel]] <- lapply(xy[[channel]], flow_equalizeDyeImages, artifactMask)
    xy.labeled[[channel]] <- mapply(flow_labelDye, xy[[channel]], xy.labeled$phase, SIMPLIFY=FALSE)
  }
  
  output <- generateBlobTimeseries(xy.labeled$phase[-1], minTimespan=params$minTimespan)
  
  dyeOverlap <- list()
  for (channel in names(xy)[-(names(xy) == "phase")]) {
    dyeOverlap <- findDyeOverlap(xy.labeled[[channel]][-1], xy.labeled$phase[-1], output)
  }
  
  
  # Generate filenames from timestamps
  # Assuming hours < 1000
  filenames <- params$startTime + ((0:(params$nFrames-1))*params$timestep)
  filenames <- unlist(lapply(filenames, function(x) if(x<10) paste0("00",x) else if(x<100) paste0("0",x) else x))
  
  # Apply timesteps to row names of timeseries
  rownames(output$timeseries) <- filenames
  
  buildDirectoryStructure(output, 
                          phase=xy$phase[-1], 
                          labeled=xy.labeled[-1],
                          dyeOverlap=list(), 
                          filenames=filenames,
                          outputDir=params$outputDir)
  

#   # TEST
#   outlines <- mapply(overlayOutlines, xy$phase, xy.labeled$phase, SIMPLIFY=FALSE)
#   lapply(outlines, display)  
}


