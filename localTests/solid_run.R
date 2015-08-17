
params <- list()

### TESTING
if (FALSE) {
  params$inputDir <- "localData/solid/"
  # params$outputDir <- "~/Desktop/outputTest01"
  params$outputDir <- "/Users/Jonathan/Desktop/outputSolidTest"
  params$nFrames <- 7
  params$minTimespan <- 5
  params$extension <- "jpg"
  params$xy <- c("xy2")             # Single section to look at
  params$channels <- c("c1","c3")         # One or more channels to look at, c1 required
  params$channelNames <- c("phase","red")    # Names of channels, 'phase' is required
}

if (FALSE) {
  params$inputDir <- "~/Desktop/TBData/solid/Time course/"
  params$outputDir <- "~/Desktop/outputSolid_08152015"
  params$nFrames <- 20
  params$extension <- "tif"
  params$xy <- c("xy2")             # Single section to look at
  params$channels <- c("c1","c3")         # One or more channels to look at, c1 required
  params$channelNames <- c("phase","red")    # Names of channels, 'phase' is required
  params$minTImespan <- 8
}


params$startTime <- 0 # Time of first image
params$timestep <- 3 # Timestep in hours
params$distanceScale <- 0.43 # units: pixels / um

params$numTargets <- 12 # How many target features to use for alignment
params$targetWidth <- 50 # How large of a region the targets should be
params$searchSpace <- 70 # How far left, top, right, down to search for alignment


DEBUG <- FALSE


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
  
  xy <- loadImages(params$inputDir, c(xyName), params$channels,
                   params$channelNames, params$extension, n=params$nFrames)[[xyName]]
  
  # Equalize phase images
  cat("\nEqualizing phase images, formula (image-a)*b")
  ptm <- proc.time()
  xy$phase <- lapply(xy$phase, solid_equalizePhase)
  cat(paste0("\nImages equalized in ", formatTime(ptm)))
  
  xy <- solid_alignImages(xy,
                          numTargets=params$numTargets,
                          targetWidth=params$targetWidth, 
                          searchSpace=params$searchSpace)
  
  cat("\nLabeling phase images")
  
  xy.labeled <- list()
  xy.labeled$phase <- lapply(xy$phase, solid_labelPhase)
  
  output <- generateBlobTimeseries(xy.labeled$phase, 
                                   minTimespan=params$minTimespan, 
                                   maxDistance=params$maxDistance)
  
  # Generate filenames from timestamps
  # Assuming hours < 1000
  filenames <- params$startTime + ((0:(params$nFrames-1))*params$timestep)
  filenames <- unlist(lapply(filenames, function(x) if(x<10) paste0("00",x) else if(x<100) paste0("0",x) else x))
  
  # Apply timesteps to row names of timeseries
  rownames(output$timeseries) <- filenames
  
  # Equalize and label non-phase images
  for (channel in names(xy)[-(names(xy) == "phase")]) {
    ptm <- proc.time()
    cat(paste0("\nEqualizing ",channel,", formula (image-a)*b"))
    xy[[channel]] <- lapply(xy[[channel]], solid_equalizeDye)
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
  
  
  #   # USEFUL FOR TESITNG
  #   outlines <- mapply(overlayOutlines, xy$phase, xy.labeled$phase, SIMPLIFY=FALSE)
  #   lapply(outlines, display)
  
}

cat(paste0("\nComplete run finished in ", formatTime(ptmTotal)))
params$maxDistance <- 75 # How far a blob can travel in pixels



