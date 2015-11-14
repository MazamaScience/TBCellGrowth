#!/depot/R/3.2.1/bin/Rscript
#
# Executable script for processing flow images


###############################################################################
# Initialize
###############################################################################

# Required packages
library(methods)
library(TBCellGrowth)

print(getwd())
# Utility functions (for parsing and validating arguments)
source('utils_exec.R')

# Obtain and validate command line arguments
opt <- solid_parseCommandLineArguments()

# Create overall directory
dir.create(opt$outputDir, showWarnings=FALSE)

###############################################################################
# Process images
###############################################################################

# for output, handle each xy region at a time
for (chamber in opt$chambers) {
  
  # Begin profiling
  profileStart()
  
  # ----- Create output directories -------------------------------------------
  
  chamberOutputDir <- paste0(opt$outputDir, "/", chamber)
  debugDir <- paste0(chamberOutputDir,"/DEBUG")
  
  # Make directories and open file
  dir.create(chamberOutputDir, showWarnings=FALSE)
  dir.create(debugDir, showWarnings=FALSE)
  
  # Divert all output to the transcript
  transcriptFile <- file(paste0(chamberOutputDir,'/TRANSCRIPT.txt'))
  sink(transcriptFile,type='output')
  sink(transcriptFile,type='message')
  
  if (getRunOptions('verbose')) {
    cat(paste0('\nProcessing chamber "',chamber,'" on ',Sys.time(),' ------------------------------\n\n'))
  }
  
  # ----- Load images ---------------------------------------------------------
  
  if (getRunOptions('verbose')) cat('\tLoading images ...\n')
  
  imageList <- loadImages(opt$dataDir, chamber, opt$channels,
                          opt$channelNames, opt$extension,
                          startFrame=opt$startFrame, n=opt$nFrames)
  
  # NOTE:  The imageList should only have good images for the purposes of tracking.
  # NOTE:  We will remove any timesteps that have a missing image in the phase
  # NOTE:  or dye channel.
  # NOTE:
  # NOTE:  But we also need to keep track of missing images for future insertion of empty
  # NOTE:  rows into the csv file and creation of 'missing' thumbnails.
  
  # TODO:  Handle cases where phase image is presnet by dye image is missing
  
  # Keep track of missing images
  goodImageMask <- rep(TRUE,length(imageList[['phase']]))
  goodImageList <- list()
  for (channel in names(imageList)) {
    goodImageList[[channel]] <- !is.na(imageList[[channel]])
    goodImageMask <- goodImageMask & goodImageList[[channel]]
  }
  goodImageList$goodTimestep <- goodImageMask
  
  # Remove any timesteps that have a missing image in either channel
  for (channel in names(imageList)) {
    # Remove timesteps with any missing images, starting at the end 
    # so that our ordering doesn't get messed up.
    for (i in length(goodImageMask):1) {
      if ( !goodImageMask[i] ) {
        imageList[[channel]][[i]] <- NULL           
      }
    }
  }
  
  
  # Check dimensions
  for (channel in names(imageList)) {
    
    # Sanity check -- all dimensions should be the same
    dims <- lapply(imageList[[channel]], dim)
    if ( length(unique(dims)) > 1 ) {
      cat(paste0('ERROR:\tTimesteps and image dimensions for the ',channel,' channel:\n'))
      str(dims)
      stop(paste0('The channel named "',channel,'" has ',length(unique(dims)),' different image dimensions.'))
      # TODO:  Don't just quit at this point.
    }
    
  }
  
  profilePoint('loadImages','seconds to load images')
  if (getRunOptions('verbose')) printMemoryUsage()
  
  
  # ----- Equalise phase images -----------------------------------------------
  
  if (getRunOptions('verbose')) cat('\tEqualizing images ...\n')
  
  # Assume that we always have phase
  imageList[[1]] <- lapply(imageList[[1]], flow_equalizePhase, opt$phaseMedian)
  
  profilePoint('flow_equalizePhase','seconds to equalize phase images')
  
  if (getRunOptions('debug_images')) {
    saveImageList(imageList,debugDir,chamber,'A_equalized')    
    profilePoint('saveImages','seconds to save images')
  }
  

  ### TODO should these imagetypes be aligned?
  # ----- Align images --------------------------------------------------------
#   
#   if (getRunOptions('verbose')) cat('\tAligning images ...\n')
#   
#   imageList <- flow_alignImages(imageList,
#                                 numTargets=opt$numTargets,
#                                 targetWidth=opt$targetWidth, 
#                                 searchSpace=opt$searchSpace)
#   
#   # Profiling handled inside flow_alignImages()
#   
#   if (getRunOptions('debug_images')) {
#     saveImageList(imageList,debugDir,chamber,'C_aligned')    
#     profilePoint('saveImages','seconds to save images')
#   }
#   
  
  # ----- Label colonies --------------------------------------------------------
  
  if (getRunOptions('verbose')) cat('\tLabeling images ...\n')
  
  labeledImageList <- list()
  labeledImageList[[1]] <- lapply(imageList[[1]], solid_labelPhase)
  
  profilePoint('flow_labelPhase','seconds to create labeled images')   
  
  if (getRunOptions('debug_images')) {
    saveImageList(labeledImageList,chamberOutputDir,chamber,'D_labeled')    
    profilePoint('saveImages','seconds to save images')
  }
  
  
  # ----- Generate timeseries ---------------------------------------------------
  
  if (getRunOptions('verbose')) cat('\tGenerating timeseries ...\n')
  
  timeseriesList <- generateBlobTimeseries(labeledImageList[[1]], 
                                   minTimespan=opt$minTimespan)
  
  if (getRunOptions('verbose')) {
    cat(paste0('\nPhase timeseries generated ---------------------------------\n\n'))
    printMemoryUsage()
  }

    
  # ----- Equalize and label non-phase images -----------------------------------
  
  # NOTE:  This is done here because labeling depends on the 'phase' labels.
  
  if (getRunOptions('verbose')) cat('\tEqualizing and labeling non-phase images ...\n')
  
  for (channel in names(imageList)[-1]) { # TODO:  Improve this logic
    if (getRunOptions('verbose')) cat(paste0("\tEqualizing ",channel," ...\n"))
    for (i in 1:length(imageList[[channel]])) {
      imageList[[channel]][[i]] <- solid_equalizeDye(imageList[[channel]][[i]])
      # Profiling handled inside flow_equalizeDye
      if (getRunOptions('verbose')) cat(paste0("\tLabeling ",channel," ...\n"))
      labeledImageList[[channel]][[i]] <- flow_labelDye(imageList[[channel]][[i]], labeledImageList[[1]][[i]])
      # Profiling handled inside flow_equalizeDye
    }
  }
  
  profilePoint('non_phase','seconds to equalize and label non-phase images')   
  
  if (getRunOptions('verbose')) cat('\tFinding overlaps for non-\'phase\' images ...\n')
  
  # TODO:  Should this be inside the previous loop?
  dyeOverlap <- list()
  for (channel in names(imageList)[-1]) { # TODO:  Improve this logic
    if (getRunOptions('verbose')) cat(paste0("\tFinding ",channel, " overlap ...\n"))
    dyeOverlap[[channel]] <- findDyeOverlap(labeledImageList[[channel]], timeseriesList)
    profilePoint('overlap','seconds to findn dye overlaps')   
  }
  
  profilePoint('non_phase','seconds to find dye image overlaps')   
  
  
  # ----- Create output -------------------------------------------------------
  
  if (getRunOptions('verbose')) cat('\tCreating output csv and images ...\n')
  
  # Generate filenames from timestamps
  # Assuming hours < 1000
  hours <- opt$startTime + ((0:(length(imageList[[1]])-1))*opt$timestep)
  filenames <- stringr::str_sub(paste0('000',hours),-3)
  
  # Apply timesteps to row names of timeseries
  rownames(timeseriesList$timeseries) <- filenames
  # Apply timesteps to overlap row names
  for (channel in names(dyeOverlap)) {
    rownames(dyeOverlap[[channel]]) <- filenames
  }
  
  # Create csv files ------------------
  
  if (getRunOptions('verbose')) cat("\tCreating csv files ...\n")
  
  # phase channel
  # TODO:  Should we use opt$channelNames[1] here instead of "phase"?
  writeExcel(timeseriesList$timeseries, chamberOutputDir, "phase", filenames, chamber)
  
  # All dye channels
  for (name in names(dyeOverlap)) {
    writeExcel(dyeOverlap[[name]], chamberOutputDir, name, filenames, chamber)
  }
  
  # Create full-frame images ------------------------------
  
  if (getRunOptions('verbose')) cat("\tCreating full-frame images ...\n")
  
  result <- try( writeFullFrameImages(timeseriesList, 
                                      phase=imageList[[1]], 
                                      labeledImageList=labeledImageList,
                                      dyeOverlap=dyeOverlap,
                                      filenames=filenames,
                                      outputDir=chamberOutputDir,
                                      distanceScale=opt$distanceScale),
                 silent=FALSE )
  
  if ( class(result)[1] == "try-error" ) {
    err_msg <- geterrmessage()
    cat(paste0('\tWARNING:  While creating full-frame images:\n\t',err_msg,'\n'))
  }
  
  # Profiling handled inside writeFullFrameImages()
  
  # Create individual images ------------------------------
  
  if ( !getRunOptions('noHyperlinks') ) {
    
    if (getRunOptions('verbose')) cat("\tCreating individual images ...\n")
    
    result <- try( writeIndividualImages(timeseriesList, 
                                         phase=imageList[[1]], 
                                         labeledImageList=labeledImageList,
                                         dyeOverlap=dyeOverlap,
                                         filenames=filenames,
                                         outputDir=chamberOutputDir,
                                         distanceScale=opt$distanceScale),
                   silent=FALSE )
    
    if ( class(result)[1] == "try-error" ) {
      err_msg <- geterrmessage()
      cat(paste0('\tWARNING:  While creating individual images:\n\t',err_msg,'\n'))
    }
    
    # Profiling handled inside writeIndividualImages()
    
  }
  
  # ----- Cleanup -------------------------------------------------------------
  
  rm(imageList)
  rm(labeledImageList)
  rm(dyeOverlap)
  rm(timeseriesList)
  
  if (getRunOptions('verbose')) {
    profileEnd(paste0('seconds total for chamber ',chamber,'\n'))
    cat(paste0('\nFinished chamber "',chamber,'" at ',Sys.time(),' --------------------------------\n\n'))
    printMemoryUsage()
  }
  
  
  # Restore normal output
  sink(type='message')
  sink()
  
} # END of chamber loop

###############################################################################
# END
###############################################################################

if (FALSE) {
  
  outlined <- mapply(overlayOutlines, imageList[[1]], labeledImageList[[1]], col="yellow", SIMPLIFY=FALSE)
  
  for (i in 1:length(outlined)) {
    im <- outlined[[i]]
    if (i<10) i = paste0("0",i)
    writeImage(im, paste0("im",i,".jpg"), quality=60)
  }
  
  ### TEST DIFFERENT TIMESERIES ALGORITHMS
  plot(0, 0, type="n", xlim=c(0,dim(timeseriesList$timeseries)[1]), ylim=c(0,max(timeseriesList$timeseries, na.rm=TRUE)))
  apply(timeseriesList$timeseries, 2, lines, col=rgb(0,0,0,0.1))

}



