### #!/usr/bin/env Rscript

# Jon's test script

library(TBCellGrowth)
### library(methods)

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
            outputDir='~/TBResults',
            verbose=TRUE,
            profile=TRUE,
            debug_image=TRUE,
            backgroundIndex=1,
            chambers=c('xy06'),
            channels='c1',
            channelNames='phase',
            startFrame=1,
            nFrames=5, # normally 30
            startTime=0,
            timestep=3,
            minTimespan=3, # normally 6
            distanceScale=0.21,
            help=FALSE,
            phaseMedian=0.4,
            numTargets=10,
            targetWidth=30,
            searchSpace=110)


# Divert all output to the transcript
transcriptFile <- file(paste0(opt$outputDir,'/TRANSCRIPT.txt'))
sink(transcriptFile,type='output')
sink(transcriptFile,type='message')

# Store run options internally
setRunOptions(opt)

# for output, handle each xy region at a time
for (chamber in opt$chambers) {
  
  # Begin profiling
  profileStart()
  
  if (getRunOptions('verbose')) {
    cat(paste0('\nProcessing chamber "',chamber,'" on ',Sys.time(),' ------------------------------\n\n'))
  }
  
  # ----- Create output directories -------------------------------------------
  
  outputDir <- paste0(opt$outputDir, "/", chamber)
  
  # Make directories and open file
  dir.create(outputDir, showWarnings=FALSE)
  
  
  # ----- Load images ---------------------------------------------------------
  
  if (getRunOptions('verbose')) cat('\tLoading images ...\n')
  
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
  
  if (getRunOptions('verbose')) cat('\tEqualizing images ...\n')
  
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
  
  if (getRunOptions('verbose')) cat('\tRotating images ...\n')
  
  imageList <- flow_rotateImages(imageList)
  
  ###profilePoint('flow_rotatePhase','seconds to rotate images')
  
  if (getRunOptions('debug_image')) {
    saveImageList(imageList,opt$outputDir,chamber,'B_rotated')    
    profilePoint('saveImages','seconds to save images')
  }
  
  
  # ----- Align images --------------------------------------------------------
  
  if (getRunOptions('verbose')) cat('\tAligning images ...\n')
  
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
  
  if (getRunOptions('verbose')) cat('\tCreating artifact mask ...\n')
  
  artifactMask <- flow_createArtifactMask(imageList[['phase']][[1]], TRUE)
  
  profilePoint('flow_createArtifactMask','seconds to create artifact mask')
  
  
  # ----- Ignore certain regions ----------------------------------------------
  
  if (getRunOptions('verbose')) cat('\tIgnoring regions ...\n')
  
  # Interpret ignore regions as pixels
  ignoredRegions <- flow_findIgnore(opt$ignore[[chamber]], dim(imageList[['phase']][[1]]))
  # Find dark line areas in the to ignore
  darkLines <- flow_findDarkLines(imageList[['phase']][[1]])
  # Combine these two into a final ignore list
  ignoredRegions <- rbind(ignoredRegions, darkLines)
  
  profilePoint('ignoreRegions','seconds to create ignored regions')   
  
  
  # ----- Label colonies --------------------------------------------------------
  
  if (getRunOptions('verbose')) cat('\tLabeling images ...\n')
  
  # At this point we no longer need backgrounds
  for (channel in names(imageList)) {
    imageList[[channel]][[1]] <- NULL
  }
  
  labeledImageList <- list()
  labeledImageList[['phase']] <- lapply(imageList[['phase']], flow_labelPhase, artifactMask, ignoredRegions)
  
  profilePoint('flow_labelPhase','seconds to create labeled images')   
  
  if (getRunOptions('debug_image')) {
    saveImageList(labeledImageList,opt$outputDir,chamber,'D_labeled')    
    profilePoint('saveImages','seconds to save images')
  }
  
  
  # ----- Generate timeseries ---------------------------------------------------
  
  if (getRunOptions('verbose')) cat('\tGenerating timeseries ...\n')
  
  output <- generateBlobTimeseries(labeledImageList[['phase']], 
                                   minTimespan=opt$minTimespan)
  
  
  # ----- Equalize and label non-phase images -----------------------------------
  
  if (getRunOptions('verbose')) cat('\tEqualizing and labeling non-phase images ...\n')
  
  for (channel in names(imageList)[-(names(imageList) == "phase")]) { # TODO:  Improve this logic
    if (getRunOptions('verbose')) cat(paste0("\tEqualizing ",channel," ...\n"))
    imageList[[channel]] <- lapply(imageList[[channel]], flow_equalizeDye, artifactMask)
    profilePoint('flow_equalizeDye','seconds to equalize dye images')
    if (getRunOptions('verbose')) cat(paste0("\tLabeling ",channel," ...\n"))
    labeledImageList[[channel]] <- mapply(flow_labelDye, imageList[[channel]], labeledImageList[['phase']], SIMPLIFY=FALSE)
    profilePoint('flow_labelPhase','seconds to create labeled dye images')   
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
                          outputDir=outputDir,
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
  }
  
}

# Restore normal output
sink(type='message')
sink()

###############################################################################
# END
###############################################################################


