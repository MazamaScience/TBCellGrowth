#!/usr/bin/env Rscript
#
# Executable script for processing solid images


###############################################################################
# Initialize
###############################################################################

# Required packages
library(methods)
library(TBCellGrowth)

# Utility functions (for parsing and validating arguments)
source('utils_exec.R')

# Print out session information
cat(paste0('\nWorking directory: ',getwd(),'\n'))
print(sessionInfo())

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
    options(width=160)
    cat(paste0('\nWorking directory: ',getwd(),'\n'))
    print(sessionInfo())
    cat(paste0('\nRun options:\n'))
    str(opt)
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
  
  # NOTE:  flow_equalizePhase() has the preferred equalization algorithm, even for solid images.
  
  # Assume that we always have phase
  imageList[['phase']] <- lapply(imageList[['phase']], flow_equalizePhase, opt$phaseMedian)
  
  profilePoint('flow_equalizePhase','seconds to equalize phase images')
  
  if (getRunOptions('debug_images')) {
    saveImageList(imageList,debugDir,chamber,'A_equalized')    
    profilePoint('saveImages','seconds to save images')
  }
  
  
  # ----- Label colonies --------------------------------------------------------
  
  # NOTE:  solid_labelPhase() is optimized for dark cell colonies with a bright "halo".
  # NOTE:  This detection and labeling function will not perform will on images that
  # NOTE:  do not have these characteristics.
  
  if (getRunOptions('verbose')) cat('\tLabeling images ...\n')

  # NOTE:  All arguments to flow_labelPhase() are specified here. Named arguments match the 
  # NOTE:  default values for this function.  This "daylighting" of the parameters
  # NOTE:  make them available to anyone who may wish to create a copy of this script
  # NOTE:  and modify these values for experimentation.
  # NOTE: 
  # NOTE:  But be warned that these defaults were arrived at after much tinkering 
  # NOTE:  with immediate visual feedback and are best adjusted in an interactive 
  # NOTE:  R session with the environment loaded with data as would be found at this
  # NOTE:  point in the execution of this script.
  
  labeledImageList <- list()
  labeledImageList[['phase']] <- list()
  for (i in 1:length(imageList[['phase']])) {    
    if (getRunOptions('verbose')) cat(paste0('\tLabeling ',i,' ...\n'))
    labeledImageList[['phase']][[i]] <- solid_labelPhase(imageList[['phase']][[i]],
                                                         minColonySize=opt$minColonySize,
                                                         minSizeExpansion=opt$minSizeExpansion,
                                                         detectionThreshold=opt$detectionThreshold,
                                                         haloQuantile=opt$haloQuantile,
                                                         brightThreshold=opt$brightThreshold,
                                                         dilateErodeBrush1=opt$dilateErodeBrush1,
                                                         dilateErodeBrush2=opt$dilateErodeBrush2)
  }
  
  profilePoint('solid_labelPhase','seconds to create labeled images')   
  
  if (getRunOptions('debug_images')) {
    saveImageList(labeledImageList,chamberOutputDir,chamber,'D_labeled')    
    profilePoint('saveImages','seconds to save images')
  }
  
  
  # ----- Generate timeseries ---------------------------------------------------
  
  if (getRunOptions('verbose')) cat('\tGenerating timeseries ...\n')
  
  result <- try( timeseriesList <- generateBlobTimeseries(labeledImageList[[1]], 
                                           minTimespan=opt$minTimespan),
                 silent=FALSE )

  if ( class(result)[1] == "try-error" ) {
    err_msg <- geterrmessage()
    cat(paste0('\tWARNING:  "',err_msg,'" Skipping chamber ',chamber))
    next
  }
  
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
    profilePoint('overlap','seconds to find dye overlaps')   
  }
  
  profilePoint('non_phase','seconds to find dye image overlaps')   
  
  
  # ----- Save .RData file for debugging --------------------------------------
  
  filename <- paste0(debugDir,'/timeseriesList.RData')
  result <- try( save(timeseriesList,file=filename),
                 silent=FALSE )
  
  if ( class(result)[1] == "try-error" ) {
    err_msg <- geterrmessage()
    cat(paste0('\tWARNING:  Unable to save timeseriesList.RData to debug directory.\n'))
  }
  
  
  # ----- Create output -------------------------------------------------------
  
  if (getRunOptions('verbose')) cat('\tCreating output csv and images ...\n')
  
  # Generate filenames from timestamps
  # NOTE:  Filenames assume hours < 1000 AND timestep > 0.01 hours
  hours <- opt$startTime + ((0:(length(imageList[[1]])-1))*opt$timestep)
  filenames <- sprintf("%06.2f",hours)
  
  # Apply timesteps to row names of timeseries
  rownames(timeseriesList$timeseries) <- filenames
  # Apply timesteps to overlap row names
  for (channel in names(dyeOverlap)) {
    rownames(dyeOverlap[[channel]]) <- filenames
  }
  
  # NOTE:  At this point, the timeseriesList$timeseries dataframe has timesteps 
  # NOTE:  as rownames but does not have a column that can be interpreted as hours.
  # NOTE:  When the data are written out as a .csv file, these timestep rownames
  # NOTE:  will be saved as the first column.
  # NOTE:
  # NOTE:  So the analysis_~ functions cannot be used on the timeseriesList$timeseries
  # NOTE:  dataframe directly. The data must instead be read in from the .csv files
  # NOTE:  because the analysis_~ functions get the timestep from the first column.
  
  # Create csv files ------------------
  
  if (getRunOptions('verbose')) cat("\tCreating csv files ...\n")
  
  # phase channel
  # TODO:  Should we use opt$channelNames[1] here instead of "phase"?
  csvFile <- writeExcel(timeseriesList$timeseries, chamberOutputDir, "phase", filenames, chamber)
  
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


