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
  
  # Merge timeseries together
  excel <- lapply(dyeOverlap, function(x) x)
  excel$phase <- output$timeseries
  
  directoryTime <- proc.time()
  cat("\nBuilding directory structure")
  
  #################################################################
  #################################################################
  
  ptm <- proc.time()
  cat("\nfullFrame ")
  
  ### FULL FRAME ###
  dir.create(paste0(outputDir, "/fullFrame"))
  
  # Add overlays to phase
  # These overlays will also serve as a background to other channels
  #full_overlay <- mapply(overlayColor, "phase", phase, labeled$phase, SIMPLIFY=FALSE)
  full_overlay <- mapply(overlayOutlines, phase, labeled$phase, col="yellow", SIMPLIFY=FALSE)
  full_overlay <- lapply(full_overlay, overlayScaleBar, distanceScale)
  
  writeImages(full_overlay, outputDir, "fullFrame", "phase", filenames)
  
  cat(formatTime(ptm))
  
  # Write non phase channels
  for (cName in names(labeled)[names(labeled) != "phase"]) {
    ptm <- proc.time()
    cat(paste0("\n",cName," "))
    channel <- labeled[[cName]]
    overlay <- mapply(overlayColor, cName, phase, channel, full_overlay, SIMPLIFY=FALSE)
    writeImages(overlay, outputDir, "fullFrame", cName, filenames)
    cat(formatTime(ptm))
  }
  
  # All dyes combined of there are enough channels
  if (length(names(labeled)) > 2) {
    ptm <- proc.time()
    cat("\nallDyes ")
    dir.create(paste0(outputDir, "/fullFrame/all"))
    for (cName in names(labeled)[names(labeled) != "phase"]) {
      channel <- labeled[[cName]]
      full_overlay <- mapply(overlayColor, cName, phase, channel, full_overlay, SIMPLIFY=FALSE)
    }
    writeImages(full_overlay, outputDir, "fullFrame", "all", filenames)
    cat(formatTime(ptm))
  }
  
  rm(full_overlay)
  
  ptm <- proc.time()
  cat("\nIndividual ids")
  
  for (id in names(output$timeseries)) {
    
    cat(".")
    
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
    color_phase <- mapply(overlayOutlines, cropped_phase$bg, cropped_phase$labelSingle, col="yellow", thick=FALSE, SIMPLIFY=FALSE)
    color_phase <- lapply(color_phase, overlayScaleBar, distanceScale)
    color_phase <- mapply(overlayVitalStats, color_phase, id, filenames, sizes, distanceScale, SIMPLIFY=FALSE)
    color_phase <- mapply(overlayNeighborsIDs, color_phase, cropped_phase$labelFull, id, output$centroids, SIMPLIFY=FALSE)

    writeImages(color_phase, outputDir, id, "phase", filenames)

    # Write non phase channels
    for (cName in names(labeled)[names(labeled) != "phase"]) {
      channel <- labeled[[cName]]
      cropped_dye <- cropImageByID(id, output, phase, channel)$labelFull
      color_dye <- mapply(overlayOutlines, color_phase, cropped_dye, col=cName, thick=FALSE, SIMPLIFY=FALSE)
      writeImages(color_dye, outputDir, id, cName, filenames)
    }

  }

  cat(paste0("\nIndividual ids finished in ", formatTime(ptm)))
  
  ####################################################
  ############## CREATE EXCEL FILES
  ####################################################
  
#   writeExcel(excel$phase, outputDir, "phase", filenames)
  for (cName in names(labeled)) {
    writeExcel(excel[[cName]], outputDir, cName, filenames)
  }

  cat(paste0("\nFull directory built in ", formatTime(directoryTime)))
  
}

# Helper function, properly formats hyperlink function
excelHyperlink <- function(url, text) {
  return(paste0('=HYPERLINK("',url,'","',text,'")'))
}


# Accepts a table of values, the output directory, the current dye color,
# and a vector of times / filenames
writeExcel <- function(df, outputDir, channel, filenames) {
  
  if (dim(df)[[2]] < 1) return()
  
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

