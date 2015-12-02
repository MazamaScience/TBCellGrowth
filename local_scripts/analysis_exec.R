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
inputFile <- paste0(opt$inputDir,'/',opt$inputCsv)
cat(paste0('Reading in ',inputFile,'\n'))
df <- read.csv(inputFile)

# Perform winnowing
# TODO:  Could include startRow, stopRow as user configurable parameters
count <- ncol(df) - 1 # don't count the 'timestep' column
cat(paste0('Winnowing ',count,' colonies ...\n'))
dfList <- analysis_winnowColonies(df, opt$maxDoublingTime,
                                  opt$removeOutliers, opt$minStartHour)

# Create new files
csvFile <- paste0(opt$outputDir,'/',opt$inputCsv)
removedFile <- stringr::str_replace(csvFile,'\\.csv','_removed\\.csv')
retainedFile <- stringr::str_replace(csvFile,'\\.csv','_retained\\.csv')

# create plots
removedCount <- ncol(dfList$removed) - 1 # don't count the 'timestep' column
if ( removedCount > 0 ) {
  cat(paste0('Writing "removed" csv file with ',removedCount,' colonies ...\n'))
  write.csv(dfList$removed, removedFile)
  title <- paste0(removedCount,' Colonies Removed')
  pngFile <- stringr::str_replace(csvFile,'\\.csv','_removed_2plot\\.png')
  cat(paste0('Creating ',pngFile,'\n'))
  analysis_twoPlot(dfList$removed, title=title, filename=pngFile)
  title <- paste0(removedCount,' Colonies Removed')
  pngFile <- stringr::str_replace(csvFile,'\\.csv','_removed_4plot\\.png')
  cat(paste0('Creating ',pngFile,'\n'))
  analysis_fourPlot(dfList$removed, title=title, filename=pngFile)
} else {
  cat(paste0('\tNOTE:  No colonies removed during generation of debug plots.'))
}

retainedCount <- ncol(dfList$retained) - 1 # don't count the 'timestep' column
if ( ncol(dfList$retained ) > 0 ) {
  cat(paste0('Writing "retained" csv file with ',retainedCount,' colonies ...\n'))
  write.csv(dfList$retained, retainedFile)
  title <- paste0(retainedCount,' Colonies Retained')
  pngFile <- stringr::str_replace(csvFile,'\\.csv','_retained_2plot\\.png')
  cat(paste0('Creating ',pngFile,'\n'))
  analysis_twoPlot(dfList$retained, title=title, filename=pngFile)
  title <- paste0(retainedCount,' Colonies Retained')
  pngFile <- stringr::str_replace(csvFile,'\\.csv','_retained_4plot\\.png')
  cat(paste0('Creating ',pngFile,'\n'))
  analysis_fourPlot(dfList$retained, title=title, filename=pngFile)
} else {
  cat(paste0('\tNOTE:  No colonies retained during generation of debug plots.'))
}


###############################################################################
# END
###############################################################################


