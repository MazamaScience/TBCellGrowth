

params <- list()

## WORK COMPUTER
if (FALSE) {
  params$inputDir <- "~/Desktop/TBData/Kyle_data_2015_07_15/CellAsic, RvC, RPL22, & pEXCF-0023, 6-29-15/Time Course/"
  params$backgroundDir <- "~/Desktop/TBData/Kyle_data_2015_07_15/CellAsic, RvC, RPL22, & pEXCF-0023, 6-29-15/Background/"
  params$outputDir <- "~/Desktop/output/"
}

## LAPTOP
if (FALSE) {
  params$inputDir <- "~/Desktop/tbTest/xy6/"
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

params$alignmentTargets <- list(c(728,301), c(909,118), c(548,110))
params$targetWidth <- 25
params$searchSpace <- 25






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
    print(dye)
    images[[xy]][[dye]] <- c(backgrounds[[xy]][[dye]], images[[xy]][[dye]]) 
  }
}
rm(backgrounds)



# for output, handle each xy region at a time
### If we need to merge the xy output, make this a function and use lapply
for (xy in images) {
  
  xy <- lapply(xy, function(dye) lapply(dye, flow_equalizeImages, params$phaseMedian))
  test <- flow_rotateImages(xy)
  
  
}













### Equalize Images

test <- lapply(images, )




 # Normalize Images
phase <- lapply(phase, normalizeImages, params$phaseMedian)
dyes <- lapply(dyes, function(x) lapply(x, normalizeImages, params$dyeMedian))

# Apply image rotation
rotated <- flow_rotateImages(phase=phase, dyes=dyes)
phase <- rotated$phase
dyes <- rotated$dyes

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
dyes.labeled <- lapply(dyes, function(x) mapply(flow_labelDye, x, phase.labeled, list(artifactMask), SIMPLIFY=FALSE))

# Find dark lines
darkLines <- flow_findDarkLines(phase[[1]])

output <- generateBlobTimeseries(phase.labeled, ignore=darkLines)

dyeOverlap <- lapply(dyes.labeled, findDyeOverlap, phase.labeled, output)

## Filenames TODO how should this work
params$filenames <- unlist(lapply(1:6, function(x) if (x < 10) paste0(0,x) else x))


buildDirectoryStructure(output, phase, phase.labeled, dyes.labeled, dyeOverlap, params$filenames,
                        outputDir=params$outputDir)
