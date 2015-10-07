
library(TBCellGrowth)
library(methods)

option_list <- list(
  
  optparse::make_option(c("--startRun"), default=TRUE),
  
  # If TRUE debugging output will output to console instead of file
  optparse::make_option(c("--debug"), default=FALSE),
  
  # File paths, all required
  optparse::make_option(c("--inputDir"), type='character'),
  optparse::make_option(c("--outputDir"), type='character'),
  optparse::make_option(c("--dataDir"), default="Time Course", type='character'),  # Relative
  optparse::make_option(c("--backgroundIndex"), default=1, type='integer'),      # Relative
  
  # Comma separated string of xy regions, xy01,xy02,...,xyn
  optparse::make_option(c("--xy"), type='character'),
  # Comma separated string of channels, c1,c2,...,cn
  optparse::make_option(c("--channels"), default="c1", type='character'),
  # Comma separated string of channel names, phase,green,...,red
  optparse::make_option(c("--channelNames"), default="phase", type='character'),
  
  # Which frame to start from
  optparse::make_option(c("--startFrame"), default=1, type='integer'),
  # How many frames to read. If argument is missing will read all frames
  optparse::make_option(c("--nFrames"), default="all", type='character'),
  # File extension of images
  optparse::make_option(c("--extension"), default="tif", type='character'),
  
  # Starting hour, usually 0
  optparse::make_option(c("--startTime"), default=0, type='integer'),
  # Number of hours between image aquisitions
  optparse::make_option(c("--timestep"), default=3, type='integer'),
  # How many frames a colony should be named to be included in output
  optparse::make_option(c("--minTimespan"), default=5, type='integer'),
  # pixels/micrometer
  optparse::make_option(c("--distanceScale"), default=0.21, type='double'),
  
  # Should the image be cropped into a center rectangle
  optparse::make_option(c("--cropRect"), default=FALSE),
  # Crop bounds
  optparse::make_option(c("--cropRectX"), default=2000),
  optparse::make_option(c("--cropRectY"), default=1500)
  
)

if (FALSE) {
  
#   'CellAsic, RvC, RPL22, & pEXCF-0023, 6-29-15'
#   'CellAsic, RvC, limiting PI, 9-1-15'
#   'CellAsic, RvC, limiting PI 2, 9-16-15'
   
  args <- c('--inputDir=/Volumes/MAZAMAMOB/data/CellAsic, RvC, limiting PI, 9-1-15',
            '--outputDir=~/desktop/Analysis',
            '--xy=xy01,xy02,xy03,xy04,xy05,xy06,xy07,xy08,xy09,xy10,xy11,xy12',
            '--dataDir=Experimental images',
            '--channels=c1,c3',
            '--channelNames=phase,green',
            '--minTimespan=10',
            '--backgroundIndex=1',
            '--startFrame=8',
            '--nFrames=12',
            '--cropRect=TRUE')
  
  xyName <- "xy02"
  
  params <- optparse::parse_args(optparse::OptionParser(option_list=option_list), args=args)
  
}

if (FALSE) {
  
  args <- c('--inputDir=/Volumes/MAZAMAMOB/data/Kyle_data_2015_07_15/CellAsic, RvC, RPL22, & pEXCF-0023, 6-29-15',
            '--outputDir=~/desktop/full_test_1',
            '--xy=xy01',
            '--dataDir=Time Course',
            '--channels=c1,c3',
            '--channelNames=phase,green',
            '--minTimespan=6',
            '--backgroundIndex=1',
            '--startFrame=1',
            '--nFrames=8')
  
  xyName <- "xy01"
  
  params <- optparse::parse_args(optparse::OptionParser(option_list=option_list), args=args)
  
}


params <- optparse::parse_args(optparse::OptionParser(option_list=option_list))


# How to scale phase and dye
params$phaseMedian <- 0.4 # What value phase images should be equalized to

# Image alignment
params$numTargets <- 5 # How many target features to use for alignment
params$targetWidth <- 50 # How large of a region the targets should be
params$searchSpace <- 125 # How far left, top, right, down to search for alignment

#########
### CHECK PARAMETERS
#########

if (!("xy" %in% names(params))) stop("xy is a required parameter")
if (!("inputDir" %in% names(params))) stop("inputDir is a required parameter")
if (!("outputDir" %in% names(params))) stop("outputDir is a required parameter")

if (params$nFrames != "all") {
  params$nFrames <- strtoi(params$nFrames)
  if (params$nFrames <= params$minTimespan) stop ("nFrames must be greater than minTimespan")
}

if (is.na(as.numeric(params$backgroundIndex))) stop("backgroundIndex must be an integer")

params$dataDir <- paste0(params$inputDir, "/", params$dataDir)
params$backgroundDir <- paste0(params$inputDir, "/Background")

if (!file.exists(params$inputDir)) stop("inputDir: directory does not exist")
if (!file.exists(params$dataDir)) stop(paste0("dataDir: directory does not exist: '",params$dataDir,"'"))
if (!file.exists(params$backgroundDir)) stop("inputDir must contain folder 'Background'")

# Arguments passed as comma separated strings to vectors
params$xy <- strsplit(params$xy,",")[[1]]
params$channels <- strsplit(params$channels,",")[[1]]
params$channelNames <- strsplit(params$channelNames,",")[[1]]


if (!params$startRun) print(params)

params$debug_image = FALSE

setRunOptions(params)
profileStart()

run <- function() {
  
  ptmTotal <- proc.time()
  cat("Starting run...")
  
  # for output, handle each xy region at a time
  for (xyName in params$xy) {
    
    outputDir <- paste0(params$outputDir, "_", xyName)
    
    # Make directories and open file
    dir.create(outputDir, showWarnings=FALSE)
    if(!params$debug) sink(file=paste0(outputDir,"/run_output.txt"), type="output")
    
    regionTime <- proc.time()
    cat("\n---------------------------")
    cat(paste0("\nPROCESSING ",xyName))
    cat("\n---------------------------")
    
    # Load images
    xy <- loadImages(params$dataDir, xyName, params$channels,
                     params$channelNames, params$extension, n=params$nFrames,
                     startFrame=params$startFrame)
    
    # Load background images
    backgrounds <- loadImages(params$backgroundDir, c(xyName), params$channels,
                              params$channelNames, params$extension, startFrame=params$backgroundIndex, n=1)
    
    ### Merge backgrounds into images list
    
    for (dye in names(xy)) {
      xy[[dye]] <- c(backgrounds[[dye]], xy[[dye]]) 
    }
    
    # Clear large objects out of memory
    rm(backgrounds)
    rm(dye)
    
    # Apply crop rectangle
    if (params$cropRect) xy <- lapply(xy, function(x) lapply(x, applyCropRect, params$cropRectX, params$cropRectY))
    
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
      # cat(paste0("\nEqualizing ",channel,", formula (image-a)*"))
      xy[[channel]] <- flow_equalizeDye(xy[[channel]], artifactMask)
      cat(paste0("\nLabeling ",channel))
      xy.labeled[[channel]] <- mapply(flow_labelDye, xy[[channel]], xy.labeled$phase, SIMPLIFY=FALSE)
      cat(paste0("\n", channel, " equalized and labeled in ", formatTime(ptm)))
      
      # For debugging, write dye images
      dir.create(paste0(outputDir, "/eqDye", channel))
      dir.create(paste0(outputDir, "/eqDyeOverlay", channel))
      
      outlined <- mapply(overlayOutlines, xy[[channel]], xy.labeled[["phase"]], "yellow", SIMPLIFY=FALSE)
      outlined <- mapply(overlayOutlines, outlined, xy.labeled[[channel]], "blue", SIMPLIFY=FALSE)
      outlined <- mapply(overlayOutlines, outlined, list(artifactMask), "red", SIMPLIFY=FALSE)
      for (i in 1:length(outlined)) {
        EBImage::writeImage(outlined[[i]],  paste0(outputDir, "/eqDyeOverlay", channel,"/im",i,".jpg"))
      }
      for (i in 1:length(outlined)) {
        EBImage::writeImage(xy[[channel]][[i]],  paste0(outputDir, "/eqDye", channel,"/im",i,".jpg"))
      }
      
    }
    
    # For debugging, write dye images
    
    dyeOverlap <- list()
    for (channel in names(xy)[-(names(xy) == "phase")]) {
      ptm <- proc.time()
      cat(paste0("\nFinding ",channel, " overlap"))
      dyeOverlap[[channel]] <- findDyeOverlap(xy.labeled[[channel]], xy.labeled$phase, output)
      cat(paste0("\n", channel, " overlap found in ", formatTime(ptm)))
    }
    
    # Generate filenames from timestamps
    # Assuming hours < 1000
    filenames <- params$startTime + ((0:(length(xy$phase)-1))*params$timestep)
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
    
    if(!params$debug) sink()
    
  }
  
  cat(paste0("\nComplete run finished in ", formatTime(ptmTotal)))
  
}

if (params$startRun) run()
