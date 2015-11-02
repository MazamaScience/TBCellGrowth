#!/depot/R/3.2.1/bin/Rscript
#
# Executable script for processing flow images


###############################################################################
# Initialize
###############################################################################

# Required packages
library(methods)
library(TBCellGrowth)

# Utility functions (for parsing and validating arguments)
source('utils_exec.R')

# Obtain and validate command line arguments
opt <- parseCommandLineArguments()

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
  
  # Make directories and open file
  dir.create(chamberOutputDir, showWarnings=FALSE)
  
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
  
  profilePoint('loadImages','seconds to load images')
  
  
  # ----- Equalise phase images -----------------------------------------------
  
  if (getRunOptions('verbose')) cat('\tEqualizing images ...\n')
  
  # Assume that we always have phase
  imageList[['phase']] <- lapply(imageList[['phase']], flow_equalizePhase, opt$phaseMedian)
  
  profilePoint('flow_equalizePhase','seconds to equalize phase images')
  
  if (getRunOptions('debug_images')) {
    saveImageList(imageList,chamberOutputDir,chamber,'A_equalized')    
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
#     saveImageList(imageList,chamberOutputDir,chamber,'C_aligned')    
#     profilePoint('saveImages','seconds to save images')
#   }
#   
  
  # ----- Label colonies --------------------------------------------------------
  
  if (getRunOptions('verbose')) cat('\tLabeling images ...\n')
  
  # At this point we no longer need backgrounds
  for (channel in names(imageList)) {
    imageList[[channel]][[1]] <- NULL
  }
  
  labeledImageList <- list()
  labeledImageList[['phase']] <- lapply(imageList[['phase']], solid_labelPhase)
  
  profilePoint('flow_labelPhase','seconds to create labeled images')   
  
  if (getRunOptions('debug_images')) {
    saveImageList(labeledImageList,chamberOutputDir,chamber,'D_labeled')    
    profilePoint('saveImages','seconds to save images')
  }
  
  
  # ----- Generate timeseries ---------------------------------------------------
  
  if (getRunOptions('verbose')) cat('\tGenerating timeseries ...\n')
  
  output <- generateBlobTimeseries(labeledImageList[['phase']], 
                                   minTimespan=opt$minTimespan)
  
  
  # ----- Equalize and label non-phase images -----------------------------------
  
  # NOTE:  This is done here because labeling depends on the 'phase' labels.
  
  if (getRunOptions('verbose')) cat('\tEqualizing and labeling non-phase images ...\n')
  
  for (channel in names(imageList)[-(names(imageList) == "phase")]) { # TODO:  Improve this logic
    if (getRunOptions('verbose')) cat(paste0("\tEqualizing ",channel," ...\n"))
    for (i in 1:length(imageList[[channel]])) {
      imageList[[channel]][[i]] <- flow_equalizeDye(imageList[[channel]][[i]], artifactMask)
      # Profiling handled inside flow_equalizeDye
      if (getRunOptions('verbose')) cat(paste0("\tLabeling ",channel," ...\n"))
      labeledImageList[[channel]][[i]] <- flow_labelDye(imageList[[channel]][[i]], labeledImageList[['phase']][[i]])
      # Profiling handled inside flow_equalizeDye
    }
  }
  
  profilePoint('non_phase','seconds to equalize and label non-phase images')   
  
  if (getRunOptions('verbose')) cat('\tFinding overlaps for non-\'phase\' images ...\n')
  
  # TODO:  Should this be inside the previous loop?
  dyeOverlap <- list()
  for (channel in names(imageList)[-(names(imageList) == "phase")]) { # TODO:  Improve this logic
    if (getRunOptions('verbose')) cat(paste0("\tFinding ",channel, " overlap ...\n"))
    dyeOverlap[[channel]] <- findDyeOverlap(labeledImageList[[channel]], labeledImageList[['phase']], output)
    profilePoint('overlap','seconds to findn dye overlaps')   
  }
  
  profilePoint('non_phase','seconds to find dye image overlaps')   
  
  
  # ----- Create output -------------------------------------------------------
  
  if (getRunOptions('verbose')) cat('\tCreating output csv and images ...\n')
  
  # Generate filenames from timestamps
  # Assuming hours < 1000
  hours <- opt$startTime + ((0:(length(imageList[['phase']])-1))*opt$timestep)
  filenames <- stringr::str_sub(paste0('000',hours),-3)
  
  # Apply timesteps to row names of timeseries
  rownames(output$timeseries) <- filenames
  # Apply timesteps to overlap row names
  for (channel in names(dyeOverlap)) {
    rownames(dyeOverlap[[channel]]) <- filenames
  }
  
  buildDirectoryStructure(output, 
                          phase=imageList[['phase']], 
                          labeled=labeledImageList,
                          dyeOverlap=dyeOverlap,
                          filenames=filenames,
                          outputDir=chamberOutputDir,
                          distanceScale=opt$distanceScale)
  
  # Profiling handled inside buildDirectoryStructure()
  ###profilePoint('output','seconds to build directory structure')   
  
  
  # ----- Cleanup -------------------------------------------------------------
  
  rm(imageList)
  rm(labeledImageList)
  rm(dyeOverlap)
  rm(output)
  
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


