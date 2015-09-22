#' @export
#' @title Save List of Images as Files
#' @param imageList list of images orgainzed as [[channel]][[timestep]]
#' @param outputDir output directory
#' @param chamber name of the chamber whose images are loaded e.g. "xy2"
#' @param tag identifier appended to file names
#' @description Images in \code{imageList} are written to disk with the 
#' following filename: \code{chamber_channel_timestep_tag.jpg}.
#' 
#' @return none

saveImageList <- function(imageList, outputDir, chamber, tag) {
  
  result <- try({
    for (channel in names(imageList)) {
      for (timestep in names(imageList[[channel]])) {
        file <- paste0(outputDir,'/',chamber,'_',channel,'_',timestep,'_',tag,'.jpg')
        EBImage::writeImage(imageList[[channel]][[timestep]], file)
      }
    }
  },
  silent=TRUE)
  
  if (class(result) == "try-error") {
    errmsg <- geterrmessage()
    warning(paste0('Unable to write imageList: ',errmsg))
  }
  
}