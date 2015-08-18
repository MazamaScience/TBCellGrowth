
params <- list()

# Massive run
if (FALSE) {
  
# Input and output directories
params$inputDir <- "~/Desktop//TBData//Kyle_data_2015_07_15//CellAsic, RvC, RPL22, & pEXCF-0023, 6-29-15//Time Course"
params$backgroundDir <- "~/Desktop//TBData//Kyle_data_2015_07_15//CellAsic, RvC, RPL22, & pEXCF-0023, 6-29-15//Background"
params$outputDir <- "~/Desktop/outputFlow_08172015"

# Configure which dyes to use,
params$xy <- c("xy01","xy02","xy03","xy04",
               "xy05","xy06","xy07","xy08",
               "xy09","xy10","xy11","xy12")             # Single section to look at
params$channels <- c("c1")         # One or more channels to look at, c1 required
params$channelNames <- c("phase")    # Names of channels, 'phase' is required

}

# Testing run
if (FALSE) {
  # Input and output directories
  params$inputDir <- "localData/fluid/Time Course/"
  params$backgroundDir <- "localData/fluid/Background/"
  params$outputDir <- "~/Desktop/outputTest"
  
  # Configure which dyes to use,
  params$xy <- c("xy06")             # Single section to look at
  params$channels <- c("c1","c4")         # One or more channels to look at, c1 required
  params$channelNames <- c("phase","green")    # Names of channels, 'phase' is required
  
}


# How many frames to load
params$nFrames <- 20 # How many frames to load

# What file extension to read
params$extension <- "tif"

# How to scale phase and dye
params$phaseMedian <- 0.4 # What value phase images should be equalized to

# Image alignment
params$numTargets <- 12 # How many target features to use for alignment
params$targetWidth <- 30 # How large of a region the targets should be
params$searchSpace <- 30 # How far left, top, right, down to search for alignment

params$startTime <- 0 # Time of first image
params$timestep <- 3 # Timestep in hours

# How many frames a blob must be in to be included in output
params$minTimespan <- 5

params$distanceScale <- 0.21

# Which regions to ignore for various reasons
# Mainly used to deal with poor microscope shifting
params$ignoreSections <- list(xy01=c("bottomLeft","topRight"),
                              xy02=c("topLeft","topCenter","topRight"),
                              xy03=c("bottomLeft","left"),
                              xy04=c("topLeft","topCenter"),
                              xy05=c("topLeft","topCenter","topRight"),
                              xy06=c("topLeft"),
                              xy07=c("bottomLeft","bottomRight"),
                              xy08=c("topLeft","topCenter","topRight"),
                              xy09=c("topLeft","topCenter","topRight"),
                              xy10=c("topLeft","topCenter","topRight"),
                              xy11=c("topLeft","topCenter","topRight","right"),
                              xy12=c())






DEBUG = FALSE

ptmTotal <- proc.time()
cat("Starting run...")

# for output, handle each xy region at a time
for (xyName in params$xy) {

  outputDir <- paste0(params$outputDir,"_",xyName,"/")
  
  # Make directories and open file
  dir.create(outputDir)
  if(!DEBUG) sink(file=paste0(outputDir,"run_output.txt"), type="output")
  
  regionTime <- proc.time()
  cat("\n---------------------------")
  cat(paste0("\nPROCESSING ",xyName))
  cat("\n---------------------------")
  
  # Load images
  xy <- loadImages(params$inputDir, c(xyName), params$channels,
                       params$channelNames, params$extension, n=params$nFrames)[[xyName]]
  
  # Load background images
  backgrounds <- loadImages(params$backgroundDir, c(xyName), params$channels,
                            params$channelNames, params$extension)[[xyName]]
  
  ### Merge backgrounds into images list

  for (dye in names(xy)) {
    xy[[dye]] <- c(backgrounds[[dye]], xy[[dye]]) 
  }

  # Clear large objects out of memory
  rm(backgrounds)
  rm(dye)
  
  # Equalize phase images
  cat("\nEqualizing phase images, formula (image-a)*b")
  ptm <- proc.time()
  xy$phase <- lapply(xy$phase, flow_equalizePhase, params$phaseMedian)
  cat(paste0("\nImages equalized in ", formatTime(ptm)))
  
  # Rotate and align all channels
  xy <- flow_rotateImages(xy)
  xy <- flow_alignImages(xy,
                         numTargets=params$numTargets,
                         targetWidth=params$targetWidth, 
                         searchSpace=params$searchSpace)
  
  artifactMask <- flow_createArtifactMask(xy$phase[[1]], TRUE)
  
  cat("\nFinding regions to ignore...")
  ptm <- proc.time()
  
  # Interpret ignore regions as pixels
  ignoredRegions <- flow_findIgnore(params$ignore[[xyName]], dim(xy$phase[[1]]))
  # Find dark line areas to ignore
  darkLines <- flow_findDarkLines(xy$phase[[1]])
  # Combine these two into a final ignore list
  ignoredRegions <- rbind(ignoredRegions, darkLines)
  
  cat(paste0("\nIgnored regions found in ", formatTime(ptm)))
  
  # At this point we no longer need backgrounds
  for (channel in names(xy)) {
    xy[[channel]][[1]] <- NULL
  }
  
  cat("\nLabeling phase images")
  ptm <- proc.time()
  
  xy.labeled <- list()
  xy.labeled$phase <- lapply(xy$phase, flow_labelPhase, artifactMask, ignoredRegions)
  
  cat(paste0("\nPhase images labeled in ", formatTime(ptm)))
  
  output <- generateBlobTimeseries(xy.labeled$phase, 
                                   minTimespan=params$minTimespan)

  # Equalize and label non-phase images
  for (channel in names(xy)[-(names(xy) == "phase")]) {
    ptm <- proc.time()
    cat(paste0("\nEqualizing ",channel,", formula (image-a)*"))
    xy[[channel]] <- lapply(xy[[channel]], flow_equalizeDye, artifactMask)
    cat(paste0("\nLabeling ",channel))
    xy.labeled[[channel]] <- mapply(flow_labelDye, xy[[channel]], xy.labeled$phase, SIMPLIFY=FALSE)
    cat(paste0("\n", channel, " equalized and labeled in ", formatTime(ptm)))
  }
  
  dyeOverlap <- list()
  for (channel in names(xy)[-(names(xy) == "phase")]) {
    ptm <- proc.time()
    cat(paste0("\nFinding ",channel, " overlap"))
    dyeOverlap[[channel]] <- findDyeOverlap(xy.labeled[[channel]], xy.labeled$phase, output)
    cat(paste0("\n", channel, " overlap found in ", formatTime(ptm)))
  }
  
  # Generate filenames from timestamps
  # Assuming hours < 1000
  filenames <- params$startTime + ((0:(params$nFrames-1))*params$timestep)
  filenames <- unlist(lapply(filenames, function(x) if(x<10) paste0("00",x) else if(x<100) paste0("0",x) else x))
  
  # Apply timesteps to row names of timeseries
  rownames(output$timeseries) <- filenames
  # Apply timesteps to overlap row names
  for (channel in names(dyeOverlap)) {
    rownames(dyeOverlap[[channel]]) <- filenames
  }
  
  buildDirectoryStructure(output, 
                          phase=xy$phase, 
                          labeled=xy.labeled,
                          dyeOverlap=dyeOverlap,
                          filenames=filenames,
                          outputDir=outputDir,
                          distanceScale=params$distanceScale)
  

  cat("\n---------------------------")
  cat(paste0("\nFinished ",xyName, " in ", formatTime(regionTime)))
  cat("\n---------------------------")
  
  rm(xy)
  rm(xy.labeled)
  rm(dyeOverlap)
  rm(output)
  
  if(!DEBUG) sink()
  
}

cat(paste0("\nComplete run finished in ", formatTime(ptmTotal)))

