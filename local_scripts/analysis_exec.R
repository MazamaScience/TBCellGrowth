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

# Print out session information
cat(paste0('\nWorking directory: ',getwd(),'\n'))
print(sessionInfo())

# Obtain and validate command line arguments
opt <- flow_parseCommandLineArguments()

csvFile <- paste0(opt$inputDir,'/',opt$inputCsvFile)
  
# Read in "phase" data that has timestep as the first column
df <- read.csv(csvFile)

# Perform winnowing
# TODO:  Could include startRow, stopRow as user configurable parameters
dfList <- analysis_winnowColonies(df, opt$maxDoublingTime,
                                  opt$removeOutliers, opt$minStartHour)

# Create new files
removedFile <- stringr::str_replace(csvFile,'\\.csv','_removed\\.csv')
retainedFile <- stringr::str_replace(csvFile,'\\.csv','_retained\\.csv')

# Create csv files
write.csv(dfList$removed,removedFile)
write.csv(dfList$retained,retainedFile)

# create plots
if ( nrow(dfList$removed ) > 0) {
  write.csv(dfList$removed, removedFile)
  title <- paste0(chamber,opt$channelNames[1],' REMOVED')
  pngFile <- stringr::str_replace(csvFile,'\\.csv','_removed_2plot\\.png')
  analysis_twoPlot(dfList$removed, title=title, filename=pngFile)
  title <- paste0(chamber,opt$channelNames[1],' REMOVED')
  pngFile <- stringr::str_replace(csvFile,'\\.csv','_removed_4plot\\.png')
  analysis_fourPlot(dfList$removed, title=title, filename=pngFile)
} else {
  cat(paste0('\tNOTE:  No colonies removed during generation of debug plots.'))
}

if ( nrow(dfList$retained ) > 0) {
  write.csv(dfList$retained, retainedFile)
  title <- paste0(chamber,opt$channelNames[1],' RETAINED')
  pngFile <- stringr::str_replace(csvFile,'\\.csv','_retained_2plot\\.png')
  analysis_twoPlot(dfList$retained, title=title, filename=pngFile)
  title <- paste0(chamber,opt$channelNames[1],' RETAINED')
  pngFile <- stringr::str_replace(csvFile,'\\.csv','_retained_4plot\\.png')
  analysis_fourPlot(dfList$retained, title=title, filename=pngFile)
} else {
  cat(paste0('\tNOTE:  No colonies retained during generation of debug plots.'))
}


###############################################################################
# END
###############################################################################


