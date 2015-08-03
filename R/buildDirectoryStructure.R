#' @export
#' @title Builds A Directory of Image With Excel UI
#' @param output an output object with timeseries and centroids from
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
#' @description write a lot more here TODO
#' @return none

buildDirectoryStructure <- function(output, phase, labeled, dyeOverlap, filenames, 
                                    outputDir="output", distanceScale=NULL) {
  
  dir.create(outputDir)
  
  # Merge timeseries together
  excel <- lapply(dyeOverlap, function(x) x)
  excel$phase <- output$timeseries
  
  
  #################################################################
  #################################################################
  
  ### FULL FRAME ###
  dir.create(paste0(outputDir, "/fullFrame"))
  
  # Add overlays to phase
  # These overlays will also serve as a background to other channels
  #full_overlay <- mapply(overlayColor, "phase", phase, labeled$phase, SIMPLIFY=FALSE)
  full_overlay <- mapply(overlayOutlines, phase, labeled$phase, col="yellow", SIMPLIFY=FALSE)
  full_overlay <- lapply(full_overlay, overlayScaleBar, distanceScale, 200)
  
  writeImages(full_overlay, outputDir, "fullFrame", "phase", filenames)
  
  # Write non phase channels
  for (cName in names(labeled)[names(labeled) != "phase"]) {
    channel <- labeled[[cName]]
    overlay <- mapply(overlayColor, cName, phase, channel, full_overlay, SIMPLIFY=FALSE)
    writeImages(overlay, outputDir, "fullFrame", cName, filenames)
  }
  
  # All dyes combined of there are enough channels
  if (length(names(labeled)) > 2) {  
    dir.create(paste0(outputDir, "/fullFrame/all"))
    for (cName in names(labeled)[names(labeled) != "phase"]) {
      channel <- labeled[[cName]]
      full_overlay <- mapply(overlayColor, cName, phase, channel, full_overlay, SIMPLIFY=FALSE)
    }
    writeImages(full_overlay, outputDir, "fullFrame", "all", filenames)
  }
  
  rm(full_overlay)
  
  
  
  for (id in names(output$timeseries)) {
    
    dir.create(paste0(outputDir,"/",id))
    
    sizes <- numeric(length(filenames))
    for (ii in 1:length(output$centroids)) {
      centroids <- output$centroids[[ii]]
      mask <- id==centroids$id
      if (sum(mask) > 0) { sizes[ii] <- centroids[mask,]$size
      } else { sizes[ii] <- 0 }
    }
    
    ####################################################
    ############## ONLY PHASE
    ####################################################
    
    # Crop and color phase images
    cropped_phase <- cropImageByID(id, output, phase, labeled$phase)
#     colored_phase <- mapply(overlayColor, "phase", cropped_phase$bg, cropped_phase$label, SIMPLIFY=FALSE)
    color_phase <- mapply(overlayOutlines, cropped_phase$bg, cropped_phase$label, col="yellow", SIMPLIFY=FALSE)
    color_phase <- lapply(color_phase, overlayScaleBar, distanceScale, 80)
    color_phase <- mapply(overlayVitalStats, color_phase, id, filenames, sizes, distanceScale, SIMPLIFY=FALSE)

    writeImages(color_phase, outputDir, id, "phase", filenames)

#     # Write non phase channels
#     for (cName in names(labeled)[names(labeled) != "phase"]) {
#       channel <- labeled[[cName]]
#       overlay <- mapply(overlayColor, cName, phase, channel, full_overlay, SIMPLIFY=FALSE)
#       writeImages(overlay, outputDir, "fullFrame", cName, filenames)
#     }

  }
  
  ####################################################
  ############## CREATE EXCEL FILES
  ####################################################
  
  writeExcel(excel$phase, outputDir, "phase", filenames)
  for (dye in names(dyes.labeled)) {
    writeExcel(excel[[dye]], outputDir, dye, filenames)
  }
  
}

# Helper function, properly formats hyperlink function
excelHyperlink <- function(url, text) {
  return(paste0('=HYPERLINK("',url,'","',text,'")'))
}


# Accepts a table of values, the output directory, the current dye color,
# and a vector of times / filenames
writeExcel <- function(df, outputDir, channel, filenames) {
  
  write.csv(df, paste0(outputDir, "/", channel, "_noLinks.csv"))
  
  # Creates hyperlinks to specific images
  cellHyperlinks <- function(id) {
    oDir <- paste0(id, "/", channel)
    cols <- df[id]
    for (i in 1:length(cols[[1]])) {
      filename <- paste0(oDir, "/t_", filenames[[i]], ".jpg")
      cols[id][[1]][[i]] <- excelHyperlink(filename, cols[id][[1]][[i]])
    }
    return(cols)
  }
  
  # Create hyperlinks on blob names
  colHyperlinks <- function(id) {
    oDir <- paste0(id, "/", channel)
    return(excelHyperlink(paste0(oDir),id))
  }
  
  # Create hyperlinks for full frames at time points
  timeHyperlinks <- function(time) {
    link <- paste0("fullFrame/", channel, "/t_", time,".jpg")
    return(excelHyperlink(link,time))
  }
  
  df <- data.frame(lapply(names(df), cellHyperlinks))
  
  names(df) <- lapply(names(df), colHyperlinks)

  rownames(df) <- lapply(filenames, timeHyperlinks)
  
  write.csv(df, paste0(outputDir, "/", channel, ".csv"))
  
 
  
}



writeImages <- function(images, outputDir, id, channel, filenames) {
  dir.create(paste0(outputDir, "/", id, "/", channel))
  for (i in 1:length(images)) {
    file <- paste0(outputDir, "/", id, "/", channel, "/t_", filenames[[i]], ".jpg")
    EBImage::writeImage(images[[i]], file=file)
  }
  createGif(paste0(outputDir, "/", id, "/", channel, "/"), paste0("g_",id,".gif"))
}