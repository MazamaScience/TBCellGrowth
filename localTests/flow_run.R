

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
params$xy <- c("xy01","xy02")             # Single section to look at
params$channels <- c("c1")         # One or more channels to look at, c1 required
params$channelNames <- c("phase")    # Names of channels, 'phase' is required

# How many frames to load
params$nFrames <- 10

# What file extension to read
params$extension <- "tif"

# How to scale phase and dye
params$phaseMedian <- 0.4
params$dyeMedian <- 0.02

params$numTargets <- 12
params$targetWidth <- 30
params$searchSpace <- 30

# Which regions to ignore for various reasons
params$ignoreSections <- list(xy01=c("topRight","bottomLeft"))






### TODO Separate script for testing alignment and normalization?
### TODO Ask Kyle about filenaming
### TODO the file loading sequence will change, my example
### file structure isn't in the final form
images <- solid_loadImages(params$inputDir, params$xy, params$channels,
                           params$channelNames, params$extension, n=params$nFrames)

backgrounds <- solid_loadImages(params$backgroundDir, params$xy, params$channels,
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
  
  xy.labeled <- list()
  xy.labeled$phase <- lapply(xy$phase, flow_labelPhase, artifactMask)
  
  # Interpret ignore regions as pixels
  ignore <- flow_findIgnore(params$ignore[[xyName]], dim(xy$phase[[1]]))
  # Find dark line areas to ignore
  darkLines <- flow_findDarkLines(xy$phase[[1]])
  # Combine these two into a final ignore list
  ignore <- rbind(ignore, darkLines)
  
  output <- generateBlobTimeseries(xy.labeled$phase, ignore=ignore)
  
  buildDirectoryStructure(output, 
                          xy$phase, 
                          xy.labeled=list(),
                          dyeOverlap=list(), 
                          params$filenames,
                          outputDir=params$outputDir)
  

#   # TEST
#   outlines <- mapply(overlayOutlines, xy$phase, xy.labeled$phase, SIMPLIFY=FALSE)
#   lapply(outlines, display)  
}


