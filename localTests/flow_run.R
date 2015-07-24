

params <- list()

## WORK COMPUTER
if (FALSE) {
  params$inputDir <- "~/Desktop/TBData/Kyle_data_2015_07_15/CellAsic, RvC, RPL22, & pEXCF-0023, 6-29-15/Time Course/"
  params$backgroundDir <- "~/Desktop/TBData/Kyle_data_2015_07_15/CellAsic, RvC, RPL22, & pEXCF-0023, 6-29-15/Background/"
  params$outputDir <- "~/Desktop/output/"
}

## LAPTOP
if (FALSE) {
  params$inputDir <- "/Volumes/MazamaDataMobile/Data/Kyle_data_2015_07_15/CellAsic, RvC, RPL22, & pEXCF-0023, 6-29-15/Time Course/"
  params$backgroundDir <- "/Volumes/MazamaDataMobile/Data/Kyle_data_2015_07_15/CellAsic, RvC, RPL22, & pEXCF-0023, 6-29-15/Background/"
  params$outputDir <- "~/Desktop/output/"
}


# Configure which dyes to use,
params$xy <- c("xy02")             # Single section to look at
params$channels <- c("c1")         # One or more channels to look at, c1 required
params$channelNames <- c("phase")    # Names of channels, 'phase' is required

# How many frames to load
params$nFrames <- 5 # How many frames to load

# What file extension to read
params$extension <- "tif" # Image file extension

# How to scale phase and dye
params$phaseMedian <- 0.4 # What value phase images should be equalized to
params$dyeMedian <- 0.02 # What value dye images should be equalized to

params$numTargets <- 12 # How many target features to use for alignment
params$targetWidth <- 30 # How large of a region the targets should be
params$searchSpace <- 30 # How far left, top, right, down to search fo alignment

params$startTime <- 0 # Time of first image
params$timestep <- 3 # Timestep in hours

# Which regions to ignore for various reasons
params$ignoreSections <- list(xy02=c("topRight","topCenter","topLeft"))






### TODO Separate script for testing alignment and normalization?
### TODO Ask Kyle about filenaming
### TODO the file loading sequence will change, my example
### file structure isn't in the final form
images <- solid_loadImages(params$inputDir, params$xy, params$channels,
                           params$channelNames, params$extension, n=params$nFrames)

backgrounds <- loadImages(params$backgroundDir, params$xy, params$channels,
                                params$channelNames, params$extension)


### Merge backgrounds into images list
for (xy in names(images)) {
  for (dye in names(images[[xy]])) {
    images[[xy]][[dye]] <- c(backgrounds[[xy]][[dye]], images[[xy]][[dye]]) 
  }
}
rm(backgrounds)
rm(dye)
rm(xy)



# for output, handle each xy region at a time
### If we need to merge the xy output, make this a function and use lapply
for (xyName in names(images)) {
  
  xy <- images[[xyName]]
  
  xy <- lapply(xy, function(dye) lapply(dye, flow_equalizeImages, params$phaseMedian))
  xy <- flow_rotateImages(xy)
  xy <- flow_alignImages(xy,
                         numTargets=params$numTargets,
                         targetWidth=params$targetWidth, 
                         searchSpace=params$searchSpace)
  
  artifactMask <- flow_createArtifactMask(xy$phase[[1]], TRUE)
  
  # Interpret ignore regions as pixels
  ignore <- flow_findIgnore(params$ignore[[xyName]], dim(xy$phase[[1]]))
  # Find dark line areas to ignore
  darkLines <- flow_findDarkLines(xy$phase[[1]])
  # Combine these two into a final ignore list
  ignore <- rbind(ignore, darkLines)
  
  xy.labeled <- list()
  xy.labeled$phase <- lapply(xy$phase, flow_labelPhase, artifactMask, ignore)
  
  output <- generateBlobTimeseries(xy.labeled$phase, minTimespan=2)
  
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


