#' @export
#' @title Write Individual Images
#' @param timeseriesList a List with timeseries and centroids from
#' \link{generateBlobTimeseries}
#' @param phase a list of phase contrast image matrices
#' @param labeled a list of labeled images from each channel
#' \link{flow_labelPhase}
#' @param dyeOverlap a \code{dataframe} of dye overlap values from
#' \link{findDyeOverlap}
#' @param filenames a vector of names equal to the length of the 
#' image lists. Usually timesteps.
#' @param outputDir the directory to build the file structure
#' @param distanceScale the distance conversion in um/pixel
#' @param chamber the current chamber id
#' @description Creates individual directories and populates each
#' with colony thumbnail images with outlines and other labeling.
#' @return none

writeIndividualImages <- function(timeseriesList, phase, labeled, dyeOverlap, filenames, 
                                  outputDir="output", distanceScale=NULL, chamber="") { 
  
  dir.create(paste0(outputDir, "/individual"), showWarnings=FALSE, recursive=TRUE)
  
  counter <- 0
  colonyCount <- length(names(timeseriesList$timeseries))
  everyNth <- round(colonyCount/20)
  for (id in names(timeseriesList$timeseries)) {
    
    counter <- counter + 1
    if (getRunOptions('verbose')) {
      if ( (counter %% everyNth) == 0 ) cat(paste0('\t',counter,'/',colonyCount,'\n'))
    }
    
    dir.create(paste0(outputDir,"/individual/",id), showWarnings=FALSE, recursive=TRUE)
    
    sizes <- numeric(length(filenames))
    for (ii in 1:length(timeseriesList$centroids)) {
      centroids <- timeseriesList$centroids[[ii]]
      mask <- id == centroids$id
      if (sum(mask) > 0) {
        sizes[ii] <- centroids[mask,]$size
      } else {
        sizes[ii] <- 0
      }
    }
    
    ####################################################
    ############## ONLY PHASE
    ####################################################
    
    # Crop and color phase images
    cropped_phase <- cropImageByID(id, timeseriesList, phase, labeled[[1]])
    #     colored_phase <- mapply(overlayColor, "phase", cropped_phase$bg, cropped_phase$label, SIMPLIFY=FALSE)
    color_phase <- mapply(overlayOutlines, cropped_phase$bg, cropped_phase$labelSingle, col="yellow", thick=FALSE, SIMPLIFY=FALSE)
    color_phase <- lapply(color_phase, overlayScaleBar, distanceScale)
    color_phase <- mapply(overlayVitalStats, color_phase, id, filenames, sizes, distanceScale, SIMPLIFY=FALSE)
    color_phase <- mapply(overlayNeighborsIDs, color_phase, cropped_phase$labelFull, id, timeseriesList$centroids, SIMPLIFY=FALSE)
    
    writeImages(color_phase, paste0(outputDir,"/individual"), id, "phase", filenames)
    
    # Write non phase channels
    for (cName in names(labeled)[-1]) {
      channel <- labeled[[cName]]
      cropped_dye <- cropImageByID(id, timeseriesList, phase, channel)$labelFull
      color_dye <- mapply(overlayOutlines, color_phase, cropped_dye, col=cName, thick=FALSE, SIMPLIFY=FALSE)
      writeImages(color_dye, paste0(outputDir,"/individual"), id, cName, filenames)
    }
    
  }
  
  if (getRunOptions('verbose')) cat("\n")
  
  profilePoint('saveImages',paste('seconds to save individual images'))
  
}


