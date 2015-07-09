% Generated by roxygen2 (4.1.0): do not edit by hand
% Please edit documentation in R/flow_labelPhase.R
\name{flow_labelPhase}
\alias{flow_labelPhase}
\title{Identify and Label Phase Microscopy Groups}
\usage{
flow_labelPhase(image, artifactMask)
}
\arguments{
\item{image}{an image matrix to search for cell colonies}

\item{artifactMask}{a mask of non biological features to ignore. See \link{createArtifactMask}.}
}
\value{
A \code{matrix} of integer labeled blobs.
}
\description{
Searches an image for dark cell colonies and incrementally labels each colony.
}
