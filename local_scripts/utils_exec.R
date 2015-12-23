# utils_exec.R
#
# Utility functions for executable scripts.

# Example command line options for interactive debuggong
if (FALSE) {
  
  # Rapid growth
  args <- c('--inputDir=/Volumes/MazamaData1/Data/TBData/CellAsic, RvC, limiting PI, 9-1-15',
            '--dataDir=Experimental images',
            '--outputDir=~/TBResults/Sep01',
            '--chambers=xy05',
            '--channels=c1,c3',
            '--channelNames=phase,green',
            '--backgroundIndex=2',
            '--startFrame=8',
            '--minTimespan=3',
            '--nFrames=4',
            '--verbose')
  
  opt <- flow_parseCommandLineArguments(args)
  
  # Solid run 
  args <- c('--inputDir=/Volumes/MAZAMAMOB/Data/Alginate well plate, 10-16-15',
            '--dataDir=Images',
            '--outputDir=~/Desktop/TBResults/Nov10Test1',
            '--chambers=xy03,xy04,xy05,xy06,xy07,xy08,xy09,xy10,xy11,xy12,xy13,xy14',
            '--channels=c1,c3',
            '--channelNames=phase01,red',
            '--minTimespan=14',
            '--nFrames=20',
            '--startFrame=1',
            '--verbose')
  
  opt <- solid_parseCommandLineArguments(args)
  
  # Analysis example
  args <- c('--inputDir=/Volumes/sherman-ngs/JC_Results/TEST_solid.0.2.0/xy01/',
            '--phaseCsv=phase_xy01.csv',
            '--minExpFitHour=6',
            '--maxExpFitHour=30',
            '--minDoublingTime=0.1',
            '--maxDoublingTime=60',
            '--maxStartHour=36')
  
  opt <- analysis_parseCommandLineArguments(args)
}

###############################################################################
# For flow_exec.R
###############################################################################

# Parse command line arguments ------------------------------------------------

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
    optparse::make_option(c("--distanceScale"), default=0.21, type='double', help="pixels/micrometer [default %default]"),
    # Chambers and channels
    optparse::make_option(c("--chambers"), default='xy01,xy02', type='character', help="Comma separate string of chamber ids [default \"%default\"]"),
    optparse::make_option(c("--channels"), default="c1", type='character', help="Comma separate string of channel ids [default \"%default\"]"),
    optparse::make_option(c("--channelNames"), default="phase", type='character', help="Comma separate string of channel names [default \"%default\"]"), 
    # Labeling algorithm
    optparse::make_option(c("--minColonySize"), default=100, type='integer', help="identified groups of pixels below this size are discarded [default \"%default\"]"),
    # Optimization
    optparse::make_option(c("--startFrame"), default=1, type='integer', help="Which image frame to start from [default %default]"),
    optparse::make_option(c("--nFrames"), default="all", help="Number of frames to read [default \"%default\"]"),
    optparse::make_option(c("--minTimespan"), default=5, type='integer', help="Minimum number of frames in which a colony must be recognized [default %default]"),    
    #     # Adjustable parameters
    #     optparse::make_option(c("--phaseMedian"), default=0.4, help="Median value after equalization [default %default]"),
    #     optparse::make_option(c("--numTargets"), default=10, help="Number of target features used for alignment [default %default]"),
    #     optparse::make_option(c("--targetWidth"), default=30, help="Size of target feature used for alignment [default %default]"),
    #     optparse::make_option(c("--searchSpace"), default=110, help="Size of search area for alignment [default %default]"),    
    # Debugging    
    optparse::make_option(c("--verbose"), action="store_true", default=FALSE, help="Print out verbose processing details [default %default]"), 
    optparse::make_option(c("--noHyperlinks"), action="store_true", default=FALSE, help="Skip the creation of colony images and hyperlinks [default %default]") 
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

# Validate run options --------------------------------------------------------

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


###############################################################################
# For solid_exec.R
###############################################################################

# Parse command line arguments ------------------------------------------------

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
    optparse::make_option(c("--distanceScale"), default=0.21, type='double', help="pixels/micrometer [default %default]"),
    # Chambers and channels
    optparse::make_option(c("--chambers"), default='xy01,xy02', type='character', help="Comma separate string of chamber ids [default \"%default\"]"),
    optparse::make_option(c("--channels"), default="c1", type='character', help="Comma separate string of channel ids [default \"%default\"]"),
    optparse::make_option(c("--channelNames"), default="phase", type='character', help="Comma separate string of channel names [default \"%default\"]"),    
    # Labeling algorithm
    optparse::make_option(c("--minColonySize"), default=50, type='integer', help="identified groups of pixels below this size are discarded [default \"%default\"]"),
    # Optimization
    optparse::make_option(c("--startFrame"), default=1, type='integer', help="Which image frame to start from [default %default]"),
    optparse::make_option(c("--nFrames"), default="all", help="Number of frames to read [default \"%default\"]"),
    optparse::make_option(c("--minTimespan"), default=5, type='integer', help="Minimum number of frames in which a colony must be recognized [default %default]"),    
    #     # Adjustable parameters
    #     optparse::make_option(c("--phaseMedian"), default=0.4, help="Median value after equalization [default %default]"),
    #     optparse::make_option(c("--numTargets"), default=10, help="Number of target features used for alignment [default %default]"),
    #     optparse::make_option(c("--targetWidth"), default=30, help="Size of target feature used for alignment [default %default]"),
    #     optparse::make_option(c("--searchSpace"), default=110, help="Size of search area for alignment [default %default]"),    
    # Debugging   
    optparse::make_option(c("--verbose"), action="store_true", default=FALSE, help="Print out verbose processing details [default %default]"), 
    optparse::make_option(c("--noHyperlinks"), action="store_true", default=FALSE, help="Skip the creation of colony images and hyperlinks [default %default]") 
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

# Validate run options --------------------------------------------------------

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


###############################################################################
# For analysis_exec.R
###############################################################################

# Parse command line arguments ------------------------------------------------

# @args vector of command line arguments

analysis_parseCommandLineArguments <- function(args=commandArgs(trailingOnly=TRUE)) {
  
  option_list <- list(    
    # File paths, all required
    optparse::make_option(c("--inputDir"), default='', type='character', help="Absolute path of the input directory [default \"%default\"]"),
    optparse::make_option(c("--phaseCsv"), default='', type='character', help="Name of the 'phase' CSV file to process [default \"%default\"]"),
    optparse::make_option(c("--outputDir"), default=getwd(), type='character', help="Absolute path of the output directory [default \"%default\"]"),
    optparse::make_option(c("--minExpFitHour"), default=0, type='double', help="Hour of first datapoint to include in doubling time exponential fit [default \"%default\"]"),
    optparse::make_option(c("--maxExpFitHour"), default=1e9, type='double', help="Hour of last datapoint to include in doubling time exponential fit [default \"%default\"]"),
    optparse::make_option(c("--minDoublingTime"), default=0, type='double', help="Colonies with doubling times smaller than this many hours are removed [default \"%default\"]"),
    optparse::make_option(c("--maxDoublingTime"), default=1e9, type='double', help="Colonies with doubling times larger than this many hours are removed [default \"%default\"]"),
    optparse::make_option(c("--removeOutliers"), default=TRUE, type='logical', help="Flag indicating whether to remove colonies with doubling time outliers [default \"%default\"]"),
    optparse::make_option(c("--maxStartHour"), default=60, type='double', help="Colonies not identified by this hour are removed  [default \"%default\"]")
  )
  
  # Parse arguments
  opt <- optparse::parse_args(optparse::OptionParser(option_list=option_list), args)
  # Validate options
  opt <- analysis_validateRunOptions(opt)
  # Set internal package state
  setRunOptions(opt)
  
  return(opt)
  
}

# Validate run options --------------------------------------------------------

analysis_validateRunOptions <- function(opt) {
  
  opt$outputDir <- ifelse(opt$outputDir == '', getwd(), opt$outputDir)
  
  if (!file.exists(opt$inputDir)) stop(paste0("inputDir: directory does not exist: '",opt$inputDir,"'"))
  csvFile <- paste0(opt$inputDir,'/',opt$phaseCsv)
  if (!file.exists(csvFile)) stop(paste0("phaseCsv: file does not exist: '",csvFile,"'"))
  if (!file.exists(opt$outputDir)) stop(paste0("outputDir: directory does not exist: '",opt$outputDir,"'"))
  
  return(opt)
}

