#' @export
#' @title Create Excel File from Dataframe
#' @param df timeseries dataframe created by \link{generateBlobTimeseries}
#' @param outputDir output directory
#' @param channel name of the channel e.g. "phase" or "green"
#' @param filenames a vector of names equal to the number of rows of the dataframe.
#' Typically timesteps.
#' @param chamber name of the chamber whose images are loaded e.g. "xy2"
#' @description A specially formatted .csv file is generated from the contents
#' of the \code{df} dataframe. Individual cells will be linked to images associated 
#' with each TB colony (row) at each timestep (col). Full columns will be linked
#' to animaged gifs while full rows will be linked to full images for each timestep.
#' 
#' The images are generated in buildDirectoryStructure() which also calls this function.
#' @return Absolute path of the "no links" version of the .csv file.

writeExcel <- function(df, outputDir, channel, filenames, chamber) {
  
  # Sanity check 
  if ( ncol(df) < 1 ) {
    cat(paste0('\tWARNING: No csv file written for "',channel,'". Dataframe has ',nrow(df),' rows and ',ncol(df),' columns.\n'))
    return()
  }
  
  # Save the "no links" version of the .csv file
  file <- paste0(outputDir, "/", channel, "_", chamber, ".csv")
  result <- try( write.csv(df, file) )
  if ( class(result)[1] == "try-error" ) {
    err_msg <- geterrmessage()
    cat(paste0('\nWARNING:  Could not write "',file,'"/\n',err_msg,'\n'))   
  }
  
  returnValue <- file
  
  # Save the hyperlinked version if requested
  if ( !getRunOptions('noHyperlinks') ) {
    
    # Convert cells into hyperlinks
    for ( id in names(df) ) {
      df[[id]] <- cellHyperlinks(df[[id]], id, filenames, channel)
    }
    
    # Convert column headers into hyperlinks
    names(df) <- lapply(names(df), colHyperlinks, channel)
    
    # Convert row headers into hyperlinks
    rownames(df) <- lapply(filenames, timeHyperlinks, channel)
    
    # Save the hyperlinked .csv file
    file <- paste0(outputDir, "/", channel, "_", chamber, "_hyperlinks.csv")
    result <- try( write.csv(df, file) )
    if ( class(result)[1] == "try-error" ) {
      err_msg <- geterrmessage()
      cat(paste0('\nWARNING:  Could not write "',file,'"/\n',err_msg,'\n'))   
    }
    
  } # END of hyperlinked version
  
  return(returnValue)
  
}

###############################################################################

# Helper function to properly format hyperlink
excelHyperlink <- function(url, text) {
  return(paste0('=HYPERLINK("',url,'","',text,'")'))
}

# Creates hyperlinks to specific images
cellHyperlinks <- function(column, id, filenames, channel) {
  outputDir <- paste0("individual/", id, "/", channel)
  hyperlinks <- vector('character',length(column))
  for (i in 1:length(column)) {
    filename <- paste0(outputDir, "/t_", filenames[i], ".jpg")
    hyperlinks[i] <- excelHyperlink(filename, column[i])
  }
  return(hyperlinks)
}

# Create hyperlinks on blob names
colHyperlinks <- function(id, channel) {
  outputDir <- paste0("individual/", id, "/", channel)
  return(excelHyperlink(paste0(outputDir),id))
}

# Create hyperlinks for full frames at time points
timeHyperlinks <- function(time, channel) {
  link <- paste0("fullFrame/", channel, "/t_", time,".jpg")
  return(excelHyperlink(link,time))
}

