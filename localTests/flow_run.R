

params <- list()

## WORK COMPUTER
params$inputDir <- "~/Desktop/TBData/xy6/"
params$outputDir <- "~/Desktop/output/"

## LAPTOP
params$inputDir <- "~/Desktop/tbTest/xy6/"
params$outputDir <- "~/Desktop/output/"



# Configure which dyes to use,
params$phaseChannel <- "c1"
params$dyeChannels <- c("c2","c3")
params$channelNames <- c("green","red")

# How many frames to load
params$nFrames <-30

# What file extension to read
params$extension <- "tif"

# How to scale phase and dye
params$phaseMedian <- 0.4
params$dyeMedian <- 0.02

# Transformation parameters
params$rotate <- -1

params$alignmentTargets <- list(c(728,301), c(909,118), c(548,110))
params$targetWidth <- 25
params$searchSpace <- 25






### TODO Separate script for testing alignment and normalization?
### TODO Ask Kyle about filenaming
### TODO the file loading sequence will change, my example
### file structure isn't in the final form
phase <- flow_loadImages(paste0(params$inputDir,"phase"), n=params$nFrames, ext=params$extension)[[1]]

dyes <- list()
if (length(params$dyeChannels) > 0) {
  
  for (i in 1:length(params$channelNames)) {
    channel <- params$channelNames[[i]]
    dyes[[channel]] <- flow_loadImages(paste0(params$inputDir,channel), 
                                       n=params$nFrames, 
                                       ext=params$extension)[[1]]
  }
  
}

# Normalize Images
phase <- lapply(phase, normalizeImages, params$phaseMedian)
dyes <- lapply(dyes, function(x) lapply(x, normalizeImages, params$dyeMedian))

# Apply image transformations
processed <- flow_alignImages(phase=phase, dyes=dyes, 
                                alignmentTargets=params$alignmentTargets, 
                                targetWidth=params$targetWidth, 
                                searchSpace=params$searchSpace)

# Pull out phase and dyes, then removed process to save memory
phase <- processed$phase
dyes <- processed$dyes
rm(processed)

# Because of all of the non biological artifacts, flow experiments
# require an artifact mask.
artifactMask <- flow_createArtifactMask(phase[[1]])

# Label phase
phase.labeled <- lapply(phase, flow_labelPhase, artifactMask)

