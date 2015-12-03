#!/usr/bin/env Rscript
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
opt <- analysis_parseCommandLineArguments()

# Print out session information
cat(paste0('\nWorking directory: ',getwd(),'\n'))
print(sessionInfo())

# Read in "phase" data that has timestep as the first column
inputPhaseCsv <- paste0(opt$inputDir,'/',opt$phaseCsv)
cat(paste0('Reading in ',inputPhaseCsv,'\n'))
df <- read.csv(inputPhaseCsv)

# Perform winnowing
count <- ncol(df) - 1 # don't count the 'timestep' column
cat(paste0('Winnowing ',count,' colonies ...\n'))
dfList <- analysis_winnowColonies(df, opt$minExpFitHour, opt$maxExpFitHour,
                                  opt$minDoublingTime, opt$maxDoublingTime,
                                  opt$removeOutliers, opt$minStartTime)

# Create output file name
outputPhaseCsv <- paste0(opt$outputDir,'/',opt$phaseCsv)

# REMOVED
removedCount <- ncol(dfList$removed) - 1 # don't count the 'timestep' column
if ( removedCount > 0 ) {
  
  # Create "phase" csv file
  cat(paste0('Writing "removed" csv file with ',removedCount,' colonies ...\n'))
  removedFile <- stringr::str_replace(outputPhaseCsv,'\\.csv','_removed\\.csv')
  write.csv(dfList$removed, removedFile)
  
  # Create "phase" 2plot
  title <- paste0(removedCount,' Colonies Removed')
  pngFile <- stringr::str_replace(outputPhaseCsv,'\\.csv','_removed_2plot\\.png')
  cat(paste0('Creating ',pngFile,'\n'))
  analysis_twoPlot(dfList$removed, opt$minExpFitHour, opt$maxExpFitHour, title=title, filename=pngFile)
  
  # Create "phase" 4plot
  title <- paste0(removedCount,' Colonies Removed')
  pngFile <- stringr::str_replace(outputPhaseCsv,'\\.csv','_removed_4plot\\.png')
  cat(paste0('Creating ',pngFile,'\n'))
  analysis_fourPlot(dfList$removed, opt$minExpFitHour, opt$maxExpFitHour, title=title, filename=pngFile)
  
} else {
  cat(paste0('\tNOTE:  No colonies removed during generation of debug plots.'))
}

# RETAINED
retainedCount <- ncol(dfList$retained) - 1 # don't count the 'timestep' column
if ( ncol(dfList$retained ) > 0 ) {
  
  # Create "phase" csv file
  cat(paste0('Writing "retained" csv file with ',retainedCount,' colonies ...\n'))
  retainedFile <- stringr::str_replace(outputPhaseCsv,'\\.csv','_retained\\.csv')
  write.csv(dfList$retained, retainedFile)
  
  # Create "phase" 2plot
  title <- paste0(retainedCount,' Colonies Retained')
  pngFile <- stringr::str_replace(outputPhaseCsv,'\\.csv','_retained_2plot\\.png')
  cat(paste0('Creating ',pngFile,'\n'))
  analysis_twoPlot(dfList$retained, opt$minExpFitHour, opt$maxExpFitHour, title=title, filename=pngFile)
  
  # Create "phase 4plot
  title <- paste0(retainedCount,' Colonies Retained')
  pngFile <- stringr::str_replace(outputPhaseCsv,'\\.csv','_retained_4plot\\.png')
  cat(paste0('Creating ',pngFile,'\n'))
  analysis_fourPlot(dfList$retained, opt$minExpFitHour, opt$maxExpFitHour, title=title, filename=pngFile)
  
} else {
  cat(paste0('\tNOTE:  No colonies retained during generation of debug plots.'))
}

# Create retained and removed versions of the dye channel CSV files -----------

# Find any accompanying dye csv files
pattern <- stringr::str_replace(opt$phaseCsv,'phase','.*')    # pattern now looks like, e.g. ".*_xy01.csv"
pattern <- stringr::str_replace(pattern,'\\.csv','\\\\.csv')  # pattern now looks like, e.g. ".*_xy01\\.csv"
allCsvFiles <- list.files(opt$inputDir, pattern=pattern)
dyeCsvFiles <- setdiff(allCsvFiles,opt$phaseCsv)
cat(paste0('Processing dye channel files: ',paste(dyeCsvFiles,collapse=', ')))

for (dyeCsv in dyeCsvFiles) {
  
  inputDyeCsv <- paste0(opt$inputDir,'/',dyeCsv)
  outputDyeCsv <- paste0(opt$outputDir,'/',dyeCsv)
  
  cat(paste0('Reading in ',inputDyeCsv,'\n'))
  dyeDF <- read.csv(inputDyeCsv)
  
  retained <- dyeDF[,names(dfList$retained)]
  retainedFile <- stringr::str_replace(outputDyeCsv,'\\.csv','_retained\\.csv')
  write.csv(retained, retainedFile)
  
  removed <- dyeDF[,names(dfList$removed)]
  removedFile <- stringr::str_replace(outputDyeCsv,'\\.csv','_removed\\.csv')
  write.csv(removed, removedFile)
  
}


###############################################################################
# END
###############################################################################


