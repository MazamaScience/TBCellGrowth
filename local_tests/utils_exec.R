# utils_exec.R
#
# Utility functions for executable scripts.

# Example command line options for interactive debuggong
if (FALSE) {

  args <- c('--inputDir=/Volumes/MazamaData1/Data/TBData/CellAsic, RvC, RPL22, & pEXCF-0023, 6-29-15',
            '--outputDir=~/TBResults/June29',
            '--chambers=xy01',
            '--channels=c1',
            '--channelNames=phase,green',
            '--minTimespan=6',
            '--nFrames=8')

  # Rapid growth
  args <- c('--inputDir=/Volumes/MazamaData1/Data/TBData/CellAsic, RvC, limiting PI, 9-1-15',
            '--dataDir=Experimental images',
            '--outputDir=~/TBResults/Sep01',
            '--chambers=xy01,xy06',
            '--channels=c1,c3',
            '--channelNames=phase,green',
            '--minTimespan=2',
            '--nFrames=4',
            '--startFrame=8',
            '--backgroundIndex=2',
            '--verbose')
  
  opt <- flow_parseCommandLineArguments(args)
  
  # Solid run 
  args <- c('--inputDir=/Volumes/MAZAMAMOB/Data/Alginate well plate, 10-16-15',
            '--dataDir=Images',
            '--outputDir=~/Desktop/TBResults/Nov05Test1',
            '--chambers=xy03,xy04,xy05,xy06,xy07,xy08,xy09,xy10,xy11,xy12,xy13,xy14',
            '--channels=c1',
            '--channelNames=phase',
            '--minTimespan=14',
            '--nFrames=20',
            '--startFrame=1',
            '--verbose')
  
  opt <- solid_parseCommandLineArguments(args)
 
  
}


# Parse command line arguments ------------------------------------------------

# @title Parse Command Line Arguments
# @args vector of command line arguments

flow_parseCommandLineArguments <- function(args=commandArgs(trailingOnly=TRUE)) {
  
  option_list <- list(    
    # File paths, all required
    optparse::make_option(c("--inputDir"), default='', type='character', help="Absolute path of the input directory [default \"%default\"]"),
    optparse::make_option(c("--outputDir"), default='', type='character', help="Absolute path of the output directory [default \"%default\"]"),
    optparse::make_option(c("--dataDir"), default="Time Course", type='character', help="Relative path of the data directory within inputDir [default \"%default\"]"),
    optparse::make_option(c("--backgroundDir"), default="Background", type='character', help="Relative path of the background directory within inputDir [default \"%default\"]"),
    optparse::make_option(c("--backgroundIndex"), default=1, type='integer', help="Background index [default \"%default\"]"),
    # Image information
    optparse::make_option(c("--extension"), default="tif", type='character', help="File extension of input images [default \"%default\"]"),
    optparse::make_option(c("--startTime"), default=0, type='integer', help="Starting hour [default %default]"),
    optparse::make_option(c("--timestep"), default=3, type='integer', help="Hours between image acquisition [default %default]"),
    optparse::make_option(c("--distanceScale"), default=0.21, type='integer', help="pixels/micrometer [default %default]"),
    # Chambers and channels
    optparse::make_option(c("--chambers"), default='xy01,xy02', type='character', help="Comma separate string of chamber ids [default \"%default\"]"),
    optparse::make_option(c("--channels"), default="c1", type='character', help="Comma separate string of channel ids [default \"%default\"]"),
    optparse::make_option(c("--channelNames"), default="phase", type='character', help="Comma separate string of channel names [default \"%default\"]"),    
    # Optimization
    optparse::make_option(c("--startFrame"), default=1, type='integer', help="Which image frame to start from [default %default]"),
    optparse::make_option(c("--nFrames"), default="all", help="Number of frames to read [default \"%default\"]"),
    optparse::make_option(c("--minTimespan"), default=5, type='integer', help="Minimum number of frames in which a colony must be recognized [default %default]"),    
    # TODO:  What's the difference between backgroundIndex and startFrame    
#     # Adjustable parameters
#     optparse::make_option(c("--phaseMedian"), default=0.4, help="Median value after equalization [default %default]"),
#     optparse::make_option(c("--numTargets"), default=10, help="Number of target features used for alignment [default %default]"),
#     optparse::make_option(c("--targetWidth"), default=3, help="Size of target feature used for alignment [default %default]"),
#     optparse::make_option(c("--searchSpace"), default=110, help="Size of search area for alignment [default %default]"),    
#     
    optparse::make_option(c("--verbose"), action="store_true", default=FALSE, help="Print out verbose processing details [default %default]") 
#     optparse::make_option(c("--profile"), action="store_true", default=FALSE, help="Print out additional timing information [default %default]"),  
#     optparse::make_option(c("--debug_images"), action="store_true", default=FALSE, help="Generate intermediate images for evaluation [default %default]")    
  )
    
  # Parse arguments
  opt <- optparse::parse_args(optparse::OptionParser(option_list=option_list), args)
  # Validate options
  opt <- flow_validateRunOptions(opt)
  # Set internal package state
  setRunOptions(opt)
  
  return(opt)
  
}

###############################################################################

flow_validateRunOptions <- function(opt) {
  
  # ----- Add configurable parameters that shouldn't be command line args -----
  
  # How to scale phase and dye
  opt$phaseMedian <- 0.4 # What value phase images should be equalized to
  opt$dyeMedian <- 0.6 # What value dye images should be equalized to
  
  # Image alignment
  opt$numTargets <- 10 # How many target features to use for alignment
  opt$targetWidth <- 30 # How large of a region the targets should be
  opt$searchSpace <- 110 # How far left, top, right, down to search for alignment
  
  opt$profile <- TRUE
  opt$debug_images <- TRUE
  
  # Convert type
  
  # ----- Validate important parameters ---------------------------------------
  
  if (opt$nFrames != "all") {
    opt$nFrames <- as.numeric(opt$nFrames)
    if (opt$nFrames <= opt$minTimespan) {
      stop(paste0("nFrames[",opt$nFrames,"] must be greater than minTimespan[",opt$minTimespan,"]"))
    }
  }
  
  if (!is.numeric(opt$backgroundIndex)) stop("backgroundIndex must be an integer")
  
  opt$dataDir <- paste0(opt$inputDir, "/", opt$dataDir)
  opt$backgroundDir <- paste0(opt$inputDir, "/", opt$backgroundDir)
  
  if (!file.exists(opt$inputDir)) stop(paste0("inputDir: directory does not exist: '",opt$inputDir,"'"))
  if (!file.exists(opt$dataDir)) stop(paste0("dataDir: directory does not exist: '",opt$dataDir,"'"))
  if (!file.exists(opt$backgroundDir)) stop(paste0("backgroundDir: directory does not exist: '",opt$backgroundDir,"'"))
  
  # ----- Convert arguments passed as comma separated strings to vectors ------
  
  opt$chambers <- stringr::str_split(opt$chambers,",")[[1]]
  opt$channels <- stringr::str_split(opt$channels,",")[[1]]
  opt$channelNames <- stringr::str_split(opt$channelNames,",")[[1]]
  
  # Print to stdout in human readable form
  if (opt$verbose) {
    options(width=160)
    str(opt)
  }
  
  return(opt)
  
}






# Parse command line arguments ------------------------------------------------

# @title Parse Command Line Arguments
# @args vector of command line arguments

solid_parseCommandLineArguments <- function(args=commandArgs(trailingOnly=TRUE)) {
  
  option_list <- list(    
    # File paths, all required
    optparse::make_option(c("--inputDir"), default='', type='character', help="Absolute path of the input directory [default \"%default\"]"),
    optparse::make_option(c("--outputDir"), default='', type='character', help="Absolute path of the output directory [default \"%default\"]"),
    optparse::make_option(c("--dataDir"), default="Time Course", type='character', help="Relative path of the data directory within inputDir [default \"%default\"]"),
    # Image information
    optparse::make_option(c("--extension"), default="tif", type='character', help="File extension of input images [default \"%default\"]"),
    optparse::make_option(c("--startTime"), default=0, type='integer', help="Starting hour [default %default]"),
    optparse::make_option(c("--timestep"), default=3, type='integer', help="Hours between image acquisition [default %default]"),
    optparse::make_option(c("--distanceScale"), default=0.21, type='integer', help="pixels/micrometer [default %default]"),
    # Chambers and channels
    optparse::make_option(c("--chambers"), default='xy01,xy02', type='character', help="Comma separate string of chamber ids [default \"%default\"]"),
    optparse::make_option(c("--channels"), default="c1", type='character', help="Comma separate string of channel ids [default \"%default\"]"),
    optparse::make_option(c("--channelNames"), default="phase", type='character', help="Comma separate string of channel names [default \"%default\"]"),    
    # Optimization
    optparse::make_option(c("--startFrame"), default=1, type='integer', help="Which image frame to start from [default %default]"),
    optparse::make_option(c("--nFrames"), default="all", help="Number of frames to read [default \"%default\"]"),
    optparse::make_option(c("--minTimespan"), default=5, type='integer', help="Minimum number of frames in which a colony must be recognized [default %default]"),    
    # TODO:  What's the difference between backgroundIndex and startFrame    
    #     # Adjustable parameters
    #     optparse::make_option(c("--phaseMedian"), default=0.4, help="Median value after equalization [default %default]"),
    #     optparse::make_option(c("--numTargets"), default=10, help="Number of target features used for alignment [default %default]"),
    #     optparse::make_option(c("--targetWidth"), default=3, help="Size of target feature used for alignment [default %default]"),
    #     optparse::make_option(c("--searchSpace"), default=110, help="Size of search area for alignment [default %default]"),    
    #     
    optparse::make_option(c("--verbose"), action="store_true", default=FALSE, help="Print out verbose processing details [default %default]") 
    #     optparse::make_option(c("--profile"), action="store_true", default=FALSE, help="Print out additional timing information [default %default]"),  
    #     optparse::make_option(c("--debug_images"), action="store_true", default=FALSE, help="Generate intermediate images for evaluation [default %default]")    
  )
  
  # Parse arguments
  opt <- optparse::parse_args(optparse::OptionParser(option_list=option_list), args)
  # Validate options
  opt <- solid_validateRunOptions(opt)
  # Set internal package state
  setRunOptions(opt)
  
  return(opt)
  
}

###############################################################################

solid_validateRunOptions <- function(opt) {
  
  # ----- Add configurable parameters that shouldn't be command line args -----
  
  # How to scale phase and dye
  opt$phaseMedian <- 0.4 # What value phase images should be equalized to
  opt$dyeMedian <- 0.6 # What value dye images should be equalized to
  
  # Image alignment
  opt$numTargets <- 10 # How many target features to use for alignment
  opt$targetWidth <- 30 # How large of a region the targets should be
  opt$searchSpace <- 110 # How far left, top, right, down to search for alignment
  
  opt$profile <- TRUE
  opt$debug_images <- TRUE
  
  # Convert type
  
  # ----- Validate important parameters ---------------------------------------
  
  if (opt$nFrames != "all") {
    opt$nFrames <- as.numeric(opt$nFrames)
    if (opt$nFrames <= opt$minTimespan) {
      stop(paste0("nFrames[",opt$nFrames,"] must be greater than minTimespan[",opt$minTimespan,"]"))
    }
  }
  
  opt$dataDir <- paste0(opt$inputDir, "/", opt$dataDir)
  opt$backgroundDir <- paste0(opt$inputDir, "/", opt$backgroundDir)
  
  if (!file.exists(opt$inputDir)) stop(paste0("inputDir: directory does not exist: '",opt$inputDir,"'"))
  if (!file.exists(opt$dataDir)) stop(paste0("dataDir: directory does not exist: '",opt$dataDir,"'"))
  
  # ----- Convert arguments passed as comma separated strings to vectors ------
  
  opt$chambers <- stringr::str_split(opt$chambers,",")[[1]]
  opt$channels <- stringr::str_split(opt$channels,",")[[1]]
  opt$channelNames <- stringr::str_split(opt$channelNames,",")[[1]]
  
  # Print to stdout in human readable form
  if (opt$verbose) {
    options(width=160)
    str(opt)
  }
  
  return(opt)
  
}
