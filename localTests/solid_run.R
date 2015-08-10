
params <- list()

# Input and output directories
params$inputDir <- "localData/solid/"
params$outputDir <- "~/Desktop/outputSolidTest01/"

# Which channels and regions to load
params$xy <- c("xy2")             # Single section to look at
params$channels <- c("c1","c2","c3")         # One or more channels to look at, c1 required
params$channelNames <- c("phase","green","red")    # Names of channels, 'phase' is required

params$startTime <- 0 # Time of first image
params$timestep <- 3 # Timestep in hours
params$distanceScale <- 0.43 # units: pixels / um

params$numTargets <- 10 # How many target features to use for alignment
params$targetWidth <- 30 # How large of a region the targets should be
params$searchSpace <- 70 # How far left, top, right, down to search for alignment

params$minTimespan <- 7 # How long a blob must be active to appear on table
params$maxDistance <- 75 # How far a blob can travel in pixels

# How many frames to load
params$nFrames <- 15

# What file extension to read
params$extension <- "jpg"




ptmTotal <- proc.time()
cat("Starting run...")

# for output, handle each xy region at a time
for (xyName in params$xy) {
  
  regionTime <- proc.time()
  cat("\n---------------------------")
  cat(paste0("\nPROCESSING ",xyName))
  cat("\n---------------------------")
  
  xy <- loadImages(params$inputDir, c(xyName), params$channels,
                   params$channelNames, params$extension, n=params$nFrames)[[xyName]]
  
  # Equalize phase images
  cat("\nEqualizing images")
  ptm <- proc.time()
  xy$phase <- lapply(xy$phase, solid_equalizeImages)
  cat(paste0("\nImages equalized in ", (proc.time() - ptm)[[3]]))
  
  test <- solid_alignImages(xy,
                         numTargets=params$numTargets,
                         targetWidth=params$targetWidth, 
                         searchSpace=params$searchSpace)
  
  
  
  
  
  
  xy <- solid_alignImages(xy)
  
  xy.labeled <- list()
  xy.labeled$phase <- lapply(xy$phase, solid_labelPhase)
  
  output <- generateBlobTimeseries(xy.labeled$phase, 
                                   minTimespan=params$minTimespan, 
                                   maxDistance=params$maxDistance,
                                   distanceScale=params$distanceScale)
  
  # Generate filenames from timestamps
  # Assuming hours < 1000
  filenames <- params$startTime + ((0:(params$nFrames-1))*params$timestep)
  filenames <- unlist(lapply(filenames, function(x) if(x<10) paste0("00",x) else if(x<100) paste0("0",x) else x))
  
  # Apply timesteps to row names of timeseries
  rownames(output$timeseries) <- filenames
  
  buildDirectoryStructure(output, xy$phase, xy.labeled, list(), 
                          filenames, params$outputDir, params$distanceScale)
  
  
  
#   outlines <- mapply(overlayOutlines, xy$phase, xy.labeled$phase, SIMPLIFY=FALSE)
#   lapply(outlines, display)
  
}
