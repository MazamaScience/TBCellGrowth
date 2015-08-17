#' @export
#' @title Searches an image for dark cell colonies and incrementally labels each colony.
#' @param images a sequence of labeled images. See \link{flow_labelPhase}.
#' @param minTimespan remove blobs from output which aren't found in at least
#' n sequential images.
#' @param maxDistance the cutoff for distance between two blobs
#' @return A \code{list} with elements \code{timeseries}, a dataframe of blob IDs 
#' and blob sizes at each timestep (in pixels) and \code{centroids}, a \code{list}
#' of dataimages with centroids, blob ID's and original integer labels for mapping
#' output column names back to the original images.

generateBlobTimeseries <- function(images, minTimespan=8, maxDistance=50) {
  
  ptm <- proc.time()
  cat("\nMaking timeseries")
  
  # Get centroids for first frame (assuming empty background frame is in images[[1]])
  centroidsBefore <- getCentroids(images[[1]])
  
  # Initialize return timeseries output
  output <- data.frame(t(data.frame(centroidsBefore$size,row.names=centroidsBefore$id)))
  
  # Initialize list of centroid data.frame. We'll also be returning this so we can map
  # the timeseries back to the original images.
  centroids <- vector("list", length(images))
  centroids[[1]] <- centroidsBefore
  
  cat("\nTracking blobs")
  
  # Now track blobs between each pair of images
  for (i in 2:length(images)) {
    
    cat(".")
    
    centroidsAfter <- getCentroids(images[[i]])
    
    # Find groups that are determined to be the same between the two images
    groups <- findSimilarGroups(centroidsBefore,centroidsAfter,maxDistance)
    
    # For those continued group, give them the ID's from the previous frame
    centroidsAfter <- updateCentroidIDs(centroidsAfter, groups)
    
    # Add frame to output
    output <- appendOutput(centroidsAfter, output)
    
    # Save centroids
    centroids[[i]] <- centroidsAfter
    
    # Reassign "before" centroids to the current frame
    centroidsBefore <- centroidsAfter
    
  }
  
  # Apply minimum timespan, only saving groups that are identified for at least
  # n images
  output <- output[,apply(output, 2, function(x) sum(!is.na(x)) > minTimespan)]
  
  cat("\nTimeseries sorted by slope of ln(timeseries)")
  
  # Sort output by linear growth slope
  sorted <- apply(log(output), 2, function(x) { 
    x <- x[!is.na(x)]
    y <- 1:length(x)
    m <- lm(x ~ y)$coef[[1]]
    return(m)
  })
  sorted <- sort(sorted,TRUE)
  output <- output[,names(sorted)]
  
  # Make analysis dataframe
  analysis <- output[-c(1:dim(output)[1]),]
  analysis[1,] <- 0
  
  cat(paste0("\nTimeseries built in ", formatTime(ptm)))
  
  
  
  # HUMAN READABLE NAMES
  ######################
  ######################
  
  names <- as.list(read.csv("localData/names.csv", stringsAsFactors=F))[[1]]
  names <- unique(names[nchar(names) < 8])
  
  # Find all IDs that are in use
  ids <- unique(unlist(lapply(centroids, function(x) as.character(x$id))))
  
  # Make a dictionary mapping old ids to names
  if (length(ids) > length(names)) {
    tempNames <- names
    times <- ceiling(length(ids) / lenth(names))
    for (ii in 1:times) {
      names <- c(names, paste0(tempNames, "_", ii))
    }
  } 
  newNames <- names[sample(1:length(names),length(ids))]
  names(newNames) <- ids
  
  # Apply new names to centroids
  for (ii in 1:length(centroids)) {
    cen <- centroids[[ii]]
    ids <- as.character(cen$id)
    centroids[[ii]]$id <- unlist(newNames[ids])
  }
  
  # Apply new names to timeseries
  colnames(output) <- unlist(newNames[colnames(output)])
  
  ######################
  ######################
  
  return(list(
    timeseries = output,
    centroids = centroids
  ))
  
}






# Find the best fit between two images of centroids based on 
# distance and size. Returns a best guess of which blobs became which
findSimilarGroups <- function(c1, c2, maxDistance) {
  
  # Initialize dataframe with each combination of the two indices
  df <- expand.grid(index1=seq(1,dim(c1)[[1]]), index2=seq(1,dim(c2)[[1]]))
  df$id1 <- c1$id[df$index1]
  df$id2 <- c2$id[df$index2]
  
  # Get coordinate arrays for all combinations
  x1 <- c1$x[df$index1]
  y1 <- c1$y[df$index1]
  x2 <- c2$x[df$index2]
  y2 <- c2$y[df$index2]
  
  # Calculate the distances
  df$dist <- sqrt( (x1-x2)^2 + (y1-y2)^2 )
  
  size1 <- c1$size[df$index1]
  size2 <- c2$size[df$index2]
  
  # Calculate the growth
  df$growthPer <- size2 / size1
  df$growthRel <- unlist(lapply(df$growthPer, function(x) if(x > 1) { return(1/x)  } else { return(x) }))
  
  # Calculate "score" of group fit
  df$score <- (0.5^(df$dist/20)) * (df$growthRel^0.2)
  
  # Remove undefinted scores
  df <- df[!is.nan(df$score),]
  
  # Remove unrealistic growth
  df <- df[df$growthPer < 2.5 & df$growthPer > 0.5,]
  
  # Remove unrealistic distance travelled
  df <- df[df$dist < maxDistance,]
  
  # Sort scores
  df <- df[order(-df$score),]
  
  # Remove duplicated ids. This remove weaker connections
  df <- df[!duplicated(df$index1),]
  df <- df[!duplicated(df$index2),]
  
  return(df)
  
}



# update the output with a new timestep and 
appendOutput <- function(c1, output) {
  
  # find IDs that aren't already in output
  newIDs <- c1[!(c1$id %in% colnames(output)),]
  
  # Create empty dataframe for new IDs
  newIDs.df <- data.frame(t(data.frame(rep(NA,dim(newIDs)[[1]]))))
  colnames(newIDs.df) <- newIDs$id
  
  # Bind current output with new IDs
  allIDs.df <- cbind(output, newIDs.df)
  
  # Create a new row with all of the new values
  newRow <- data.frame(t(data.frame(c1$size, row.names=c1$id)))
  
  # Add the new row with rbind.fill, which replaces missing values with NA
  return(data.frame(dplyr::rbind_all(list(output, newRow))))
  
}



# Updates a centroid dataframe with IDs that are determined to be
# the same from the previous frame
updateCentroidIDs <- function(c1, g) {
  
  # select columns that continue between frame1 & 2
  idList <- as.character(c1$id)
  id1 <- as.character(g$id1)
  id2 <- as.character(g$id2)
  c1$id <- as.character(lapply(idList, function(x) if(x %in% id2) id1[which(id2==x)] else x))
  
  return(c1)
  
}
