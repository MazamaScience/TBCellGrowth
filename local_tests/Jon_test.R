#!/usr/bin/env Rscript

# Jon's test script

library(TBCellGrowth)
library(methods)

# ----- Set up params ---------------------------------------------------------

###
# opt <- list(inputDir='../TBCellGrowth_tests/data/fluid',
#             dataDir='../TBCellGrowth_tests/data/fluid/Time\ Course',
#             backgroundDir='../TBCellGrowth_tests/data/fluid/Background',
#             extension='jpg',
### Mazama Mobile drive
# opt <- list(inputDir='/Volumes/MAZAMAMOB/Data/Kyle_data_2015_07_15/CellAsic, RvC, RPL22, & pEXCF-0023, 6-29-15',
#             dataDir='/Volumes/MAZAMAMOB/Data/Kyle_data_2015_07_15/CellAsic, RvC, RPL22, & pEXCF-0023, 6-29-15/Time Course',
#             backgroundDir='/Volumes/MAZAMAMOB/Data/Kyle_data_2015_07_15/CellAsic, RvC, RPL22, & pEXCF-0023, 6-29-15/Background',
### Mazama Data1 drive
opt <- list(inputDir='/Volumes/MazamaData1/Data/TBData/CellAsic, RvC, RPL22, & pEXCF-0023, 6-29-15',
            dataDir='/Volumes/MazamaData1/Data/TBData/CellAsic, RvC, RPL22, & pEXCF-0023, 6-29-15/Time Course',
            backgroundDir='/Volumes/MazamaData1/Data/TBData/CellAsic, RvC, RPL22, & pEXCF-0023, 6-29-15/Background',
            extension='tif',
            outputDir='~/BOP',
            verbose=TRUE,
            profile=TRUE,
            debug_image=TRUE,
            backgroundIndex=1,
            chambers='xy06',
            channels='c1',
            channelNames='phase',
            startFrame=1,
            nFrames=6,
            startTime=0,
            timestep=3,
            minTimespan=6,
            distanceScale=0.21,
            help=FALSE,
            phaseMedian=0.4,
            numTargets=10,
            targetWidth=30,
            searchSpace=110)


# Store run options internally
setRunOptions(opt)

# Begin profiling
profileStart()

# for output, handle each xy region at a time
###for (chamber in opt$chambers) {

chamber <- 'xy06'

if (getRunOptions('verbose')) {
  cat(paste0('Processing chamber "',chamber,'" on ',Sys.time(),' ------------------------------\n\n'))
}

# ----- Create output directories -------------------------------------------

outputDir <- paste0(opt$outputDir, "/", chamber, "/")

# Make directories and open file
dir.create(outputDir, showWarnings=FALSE)


# ----- Load images ---------------------------------------------------------

if (getRunOptions('verbose')) cat('Loading images ...\n')

backgrounds <- loadImages(opt$backgroundDir, chamber, opt$channels,
                          opt$channelNames, opt$extension, startFrame=opt$backgroundIndex, n=1)

imageList <- loadImages(opt$dataDir, chamber, opt$channels,
                        opt$channelNames, opt$extension, n=opt$nFrames,
                        startFrame=opt$startFrame)

# Merge backgrounds into imageList
for (channel in names(imageList)) {
  
  imageList[[channel]] <- c(backgrounds[[channel]], imageList[[channel]])
  names(imageList[[channel]])[1] <- '000'
  
  # Sanity check -- all dimensions should be the same
  dims <- lapply(imageList[[channel]], dim)
  if ( length(unique(dims)) > 1 ) {
    stop(paste0('The channel named "',channel,'" has ',length(unique(dims)),' different image dimensions.'))
  }
  
}

# Clear large objects from memory
rm(backgrounds)

profilePoint('loadImages','seconds to load images')


# ----- Equalise phase images -----------------------------------------------

if (getRunOptions('verbose')) cat('Equalizing images ...\n')

for (channel in names(imageList)) {
  if (channel == 'phase') {
    imageList[[channel]] <- lapply(imageList[[channel]], flow_equalizePhase, opt$phaseMedian)
  } else {
    stop(paste0('This script does not handle channels named "',channel,'".'))
  }
}

profilePoint('flow_equalizePhase','seconds to equalize images')

if (getRunOptions('debug_image')) {
  saveImageList(imageList,opt$outputDir,chamber,'A_equalized')    
  profilePoint('saveImages','seconds to save images')
}


# ----- Rotate images -------------------------------------------------------

if (getRunOptions('verbose')) cat('Rotating images ...\n')

imageList <- flow_rotateImages(imageList)

###profilePoint('flow_rotatePhase','seconds to rotate images')

if (getRunOptions('debug_image')) {
  saveImageList(imageList,opt$outputDir,chamber,'B_rotated')    
  profilePoint('saveImages','seconds to save images')
}


# ----- Align images --------------------------------------------------------

if (getRunOptions('verbose')) cat('Aligning images ...\n')

imageList <- flow_alignImages(imageList,
                              numTargets=opt$numTargets,
                              targetWidth=opt$targetWidth, 
                              searchSpace=opt$searchSpace)

# Profiling handled inside flow_alignImages()

if (getRunOptions('debug_image')) {
  saveImageList(imageList,opt$outputDir,chamber,'C_aligned')    
  profilePoint('saveImages','seconds to save images')
}


# ----- Create artifact mask ------------------------------------------------

if (getRunOptions('verbose')) cat('Creating artifact mask ...\n')

artifactMask <- flow_createArtifactMask(imageList$phase[[1]], TRUE)

profilePoint('flow_createArtifactMask','seconds to create artifact mask')


# ----- Ignore certain regions ----------------------------------------------

if (getRunOptions('verbose')) cat('Ignoring regions ...\n')

# Interpret ignore regions as pixels
ignoredRegions <- flow_findIgnore(opt$ignore[[chamber]], dim(imageList$phase[[1]]))
# Find dark line areas in the to ignore
darkLines <- flow_findDarkLines(imageList$phase[[1]])
# Combine these two into a final ignore list
ignoredRegions <- rbind(ignoredRegions, darkLines)

profilePoint('ignoreRegions','seconds to create ignored regions')   


# ----- Label colonies --------------------------------------------------------

if (getRunOptions('verbose')) cat('Labeling images ...\n')

# At this point we no longer need backgrounds
for (channel in names(imageList)) {
  imageList[[channel]][[1]] <- NULL
}

# TODO:  Probably want timing inside of flow_labelPhase()

labeledImageList <- list()
labeledImageList$phase <- lapply(imageList$phase, flow_labelPhase, artifactMask, ignoredRegions)

profilePoint('flow_labelPhase','seconds to create labeled images')   

if (getRunOptions('debug_image')) {
  saveImageList(labeledImageList,opt$outputDir,chamber,'D_labeled')    
  profilePoint('saveImages','seconds to save images')
}


# ----- Generate timeseries ---------------------------------------------------

if (getRunOptions('verbose')) cat('Generating timeseries ...\n')


#   
#   output <- generateBlobTimeseries(labeledImageList$phase, 
#                                    minTimespan=opt$minTimespan)
#   
#   # Equalize and label non-phase images
#   for (channel in names(imageList)[-(names(imageList) == "phase")]) {
#     ptm <- proc.time()
#     cat(paste0("\nEqualizing ",channel,", formula (image-a)*"))
#     imageList[[channel]] <- lapply(imageList[[channel]], flow_equalizeDye, artifactMask)
#     cat(paste0("\nLabeling ",channel))
#     labeledImageList[[channel]] <- mapply(flow_labelDye, imageList[[channel]], labeledImageList$phase, SIMPLIFY=FALSE)
#     cat(paste0("\n", channel, " equalized and labeled in ", formatTime(ptm)))
#   }
#   
#   dyeOverlap <- list()
#   for (channel in names(imageList)[-(names(imageList) == "phase")]) {
#     ptm <- proc.time()
#     cat(paste0("\nFinding ",channel, " overlap"))
#     dyeOverlap[[channel]] <- findDyeOverlap(labeledImageList[[channel]], labeledImageList$phase, output)
#     cat(paste0("\n", channel, " overlap found in ", formatTime(ptm)))
#   }
#   
#   # Generate filenames from timestamps
#   # Assuming hours < 1000
#   filenames <- opt$startTime + ((0:(length(imageList$phase)-1))*opt$timestep)
#   filenames <- unlist(lapply(filenames, function(x) if(x<10) paste0("00",x) else if(x<100) paste0("0",x) else x))
#   
#   # Apply timesteps to row names of timeseries
#   rownames(output$timeseries) <- filenames
#   # Apply timesteps to overlap row names
#   for (channel in names(dyeOverlap)) {
#     rownames(dyeOverlap[[channel]]) <- filenames
#   }
#   
#   buildDirectoryStructure(output, 
#                           phase=imageList$phase, 
#                           labeled=labeledImageList,
#                           dyeOverlap=dyeOverlap,
#                           filenames=filenames,
#                           outputDir=outputDir,
#                           distanceScale=opt$distanceScale)
#   
#   
#   cat("\n---------------------------")
#   cat(paste0("\nFinished ",chamber, " in ", formatTime(regionTime)))
#   cat("\n---------------------------")
#   
#   rm(imageList)
#   rm(labeledImageList)
#   rm(dyeOverlap)
#   rm(output)
#   
# ###  if(!opt$debug) sink()
#   
# ###}
# 

profileEnd()

