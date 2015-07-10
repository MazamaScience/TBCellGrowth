#' @export
#' @title Searches an image for dark cell colonies and incrementally labels each colony.
#' @param frames a sequence of labeled frames. See \link{flow_labelPhase}.
#' @param ignore a list of y index pairs to ignore. Typically used to remove 
#' centroids near dark lines.
#' @param minTimespan remove blobs from output which aren't found in at least
#' n sequential frames.
#' @param maxDistance the cutoff for distance between two blobs
#' @return A \code{list} with elements \code{timeseries}, a dataframe of blob IDs 
#' and blob sizes at each timestep (in pixels) and \code{centroids}, a \code{list}
#' of dataframes with centroids, blob ID's and original integer labels for mapping
#' output column names back to the original images.

generateBlobTimeseries <- function(frames, ignore=list(), minTimespan=5, maxDistance=20) {
  
  # Get centroids for first frame (assuming empty background frame is in frames[[1]])
  centroidsBefore <- getCentroids(frames[[2]])
  
  # Remove ignored y indices
  for (ig in ignore) {
    centroidsBefore <- centroidsBefore[!(round(centroidsBefore$y) %in% seq(ig[[1]],ig[[2]])),]
  }
  
  # Initialize return timeseries output
  output <- data.frame(t(data.frame(centroidsBefore$size,row.names=centroidsBefore$id)))
  
  # Initialize list of centroid data.frames. We'll also be returning this so we can map
  # the timeseries back to the original images.
  centroids <- vector("list", length(frames))
  centroids[[2]] <- centroidsBefore
  
  # Now track blobs between each pair of frames
  for (i in 3:length(frames)) {
    
    # Record time
    ptm <- proc.time()
    print(paste0("Processing frame ", i, " of ", length(frames)))
    
    centroidsAfter <- getCentroids(frames[[i]])
    # Remove ignored y indices
    for (ig in ignore) {
      centroidsAfter <- centroidsAfter[!(round(centroidsAfter$y) %in% seq(ig[[1]],ig[[2]])),]
    }
    
    # Find groups that are determined to be the same between the two frames
    groups <- findSimilarGroups(centroidsBefore,centroidsAfter,maxDistance)
    
    # For those continued group, give them the ID's from the previous frame
    centroidsAfter <- updateCentroidIDs(centroidsAfter, groups)
    
    # Add frame to output
    output <- appendOutput(centroidsAfter, output)
    
    # Save centroids
    centroids[[i]] <- centroidsAfter
    
    # Reassign "before" centroids to the current frame
    centroidsBefore <- centroidsAfter
    
    print(proc.time() - ptm)
    
  }
  
  # Apply minimum timespan, only saving groups that are identified for at least
  # n frames
  output <- output[,apply(output, 2, function(x) sum(!is.na(x)) > minTimespan)]
  
  return(list(
    timeseries = output,
    centroids = centroids
  ))
  
}





# For each index in a blob labeled matrix, find the centroids, size, 
# and generate a unique id
getCentroids <- function(m) {
  
  # Initialize vectors
  x <- numeric(max(m))
  y <- numeric(max(m))
  ymin <- numeric(max(m))
  size <- numeric(max(m))
  id <- character(max(m))
  index <- numeric(max(m))
  
  # Function for generating an ID
  generateID <- function(x, y,z) {
    return(paste0("x", x, "y", y,"z",z))
  }
  
  # For each blob find centroids and size
  for (i in 1:max(m)) {
    m1 <- m == i
    ind <- which(m1, arr.ind=T)
    x[[i]] <- mean(ind[,1])
    y[[i]] <- mean(ind[,2])
    ymin[[i]] <- min(ind[,2])
    size[[i]] <- sum(m1)
    id[[i]] <- generateID(round(mean(ind[,1])),round(mean(ind[,2])),sum(m1))
    index[[i]] <- i
  }
  
  # Create a dataframe
  df <- data.frame(x=x,y=y,ymin=ymin,size=size,id=id,index=index)
  
  # Remove blobs that are of size 0
  df <- df[size>0,]
  
  return(df)
  
}




# Find the best fit between two frames of centroids based on 
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
  df <- df[df$growthPer < 1.5 & df$growthPer > 0.75,]
  
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
