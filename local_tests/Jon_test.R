#
# Jon's test script

library(TBCellGrowth)

#   $ startRun       : logi TRUE
#   $ debug          : logi TRUE
#   $ inputDir       : chr "../TBCellGrowth_tests/data/fluid"
#   $ outputDir      : chr "~/BOP"
#   $ dataDir        : chr "../TBCellGrowth_tests/data/fluid/Time Course"
#   $ backgroundIndex: int 1
#   $ xy             : chr "xy06"
#   $ channels       : chr "c1"
#   $ channelNames   : chr "phase"
#   $ startFrame     : int 1
#   $ nFrames        : chr "all"
#   $ extension      : chr "jpg"
#   $ startTime      : int 0
#   $ timestep       : int 3
#   $ minTimespan    : int 6
#   $ distanceScale  : num 0.21
#   $ help           : logi FALSE
#   $ phaseMedian    : num 0.4
#   $ numTargets     : num 10
#   $ targetWidth    : num 30
#   $ searchSpace    : num 110
#   $ backgroundDir  : chr "../TBCellGrowth_tests/data/fluid/Background/"

# ----- Set up params ---------------------------------------------------------

params <- list(inputDir='../TBCellGrowth_tests/data/fluid',
               dataDir='../TBCellGrowth_tests/data/fluid/Time\ Course',
               backgroundDir='../TBCellGrowth_tests/data/fluid/Background',
               outputDir='~/BOP',
               debug=TRUE,
               backgroundIndex=1,
               xy='xy06',
               channels='c1',
               channelNames='phase',
               startFrame=1,
               nFrames='all',
               extension='jpg',
               startTime=0,
               timestep=3,
               minTimespan=6,
               distanceScale=0.21,
               help=FALSE,
               phaseMedian=0.4,
               numTargets=10,
               targetWidth=30,
               searchSpace=110)


###ptmTotal <- proc.time()

# for output, handle each xy region at a time
###for (xyName in params$xy) {

  xyName <- 'xy06'
  
  # ----- Create output directories -------------------------------------------
  
  outputDir <- paste0(params$outputDir, "/", xyName, "/")
  
  # Make directories and open file
  dir.create(outputDir, showWarnings=FALSE)
###  if(!params$debug) sink(file=paste0(outputDir,"run_output.txt"), type="output")
  
  regionTime <- proc.time()
  cat("\n---------------------------")
  cat(paste0("\nPROCESSING ",xyName))
  cat("\n---------------------------")
  
  
  # ----- Load images ---------------------------------------------------------
  
  backgrounds <- loadImages(params$backgroundDir, c(xyName), params$channels,
                            params$channelNames, params$extension, startFrame=params$backgroundIndex, n=1)[[xyName]]
  
  xy <- loadImages(params$dataDir, c(xyName), params$channels,
                   params$channelNames, params$extension, n=params$nFrames,
                   startFrame=params$startFrame)[[xyName]]
  
  # Merge backgrounds into images list
  for (dye in names(xy)) {
    xy[[dye]] <- c(backgrounds[[dye]], xy[[dye]])
    # TODO:  Check if it is appropriate to rename the Background image from '001' to '000' at this point?
    names(xy[[dye]])[1] <- '000'
  }
  
  # Clear large objects out of memory
  rm(backgrounds)

  
  # ----- Equalise phase images -----------------------------------------------
  
  # Equalize phase images
  cat("\nEqualizing phase images, formula (image-a)*b")
  ptm <- proc.time()
  xy$phase <- lapply(xy$phase, flow_equalizePhase, params$phaseMedian)
  cat(paste0("\nImages equalized in ", formatTime(ptm)))

  if (params$savePreprocessing) {
    for (dye in names(xy)) {
      for (timestep in names(xy[[dye]])) {
        file <- paste0(params$outputDir,'/',dye,'_',timestep,'_',xyName,'_equalized.jpg')
        EBImage::writeImage(xy[[dye]][[timestep]], file)
      }
    }
  }
  
    
  # ----- Rotate images -------------------------------------------------------
  
  xy <- flow_rotateImages(xy)
  
  if (params$savePreprocessing) {
    for (dye in names(xy)) {
      for (timestep in names(xy[[dye]])) {
        file <- paste0(params$outputDir,'/',dye,'_',timestep,'_',xyName,'_rotated.jpg')
        EBImage::writeImage(xy[[dye]][[timestep]], file)
      }
    }
  }

    
  # ----- Align images --------------------------------------------------------
  
  xy <- flow_alignImages(xy,
                         numTargets=params$numTargets,
                         targetWidth=params$targetWidth, 
                         searchSpace=params$searchSpace)
  
  if (params$savePreprocessing) {
    for (dye in names(xy)) {
      for (timestep in names(xy[[dye]])) {
        file <- paste0(params$outputDir,'/',dye,'_',timestep,'_',xyName,'_aligned.jpg')
        EBImage::writeImage(xy[[dye]][[timestep]], file)
      }
    }
  }
  
  
  # ----- Create Artifact mask ------------------------------------------------
  
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
  
###  if(!params$debug) sink()
  
###}

