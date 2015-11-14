#' @export
#' @title Write Full Frame Images
#' @param timeseriesList a List with timeseries and centroids from
#' \link{generateBlobTimeseries}
#' @param phase a list of phase contrast image matrices
#' @param labeledImageList a list of labeled images from each channel
#' \link{flow_labelPhase}
#' @param dyeOverlap a \code{dataframe} of dye overlap values from
#' \link{findDyeOverlap}
#' @param filenames a vector of names equal to the length of the 
#' image lists. Usually timesteps.
#' @param outputDir the directory to build the file structure
#' @param distanceScale the distance conversion in um/pixel
#' @param chamber the current chamber id
#' @description Creates a "fullFrame" directory and populates it
#' with full-frame images with colony outlines and other labeling.
#' @return none

writeFullFrameImages <- function(timeseriesList, phase, labeledImageList, dyeOverlap, filenames, 
                                    outputDir="output", distanceScale=NULL, chamber="") { 
  
  # Add overlays to the phase images.
  # These overlays will also serve as a background to other channels
  
  full_overlay <- mapply(overlayOutlines, phase, labeledImageList$phase, col="yellow", SIMPLIFY=FALSE)
  ### full_overlay <- lapply(full_overlay, overlayScaleBar, distanceScale)
  writeImages(images=full_overlay, outputDir=outputDir, id="fullFrame", channel="phase", filenames)
  profilePoint('saveImages',paste('seconds to save outlined images'))
  
  # Write non phase channels
  for (cName in names(labeledImageList)[names(labeledImageList) != "phase"]) {
    if (getRunOptions('verbose')) cat(paste0('\tWriting ',cName,' ...\n'))
    channel <- labeledImageList[[cName]]
    overlay <- mapply(overlayColor, cName, phase, channel, full_overlay, SIMPLIFY=FALSE)
    writeImages(overlay, outputDir, id="fullFrame", cName, filenames)
    profilePoint('saveImages',paste('seconds to save dye colored',cName,'images'))
  }
  
  # All dyes combined if there are enough channels
  if (length(names(labeledImageList)) > 2) {
    dir.create(paste0(outputDir, "/fullFrame/all"), showWarnings=FALSE, recursive=TRUE)
    for (cName in names(labeledImageList)[names(labeledImageList) != "phase"]) {
      channel <- labeledImageList[[cName]]
      full_overlay <- mapply(overlayColor, cName, phase, channel, full_overlay, SIMPLIFY=FALSE)
    }
    writeImages(full_overlay, outputDir, "fullFrame", "all", filenames)
    profilePoint('saveImages',paste('seconds to save dye colored, combined images'))
  }
  
  rm(full_overlay)
  
}


