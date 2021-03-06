% Generated by roxygen2 (4.1.0): do not edit by hand
% Please edit documentation in R/analysis_fourPlot.R
\name{analysis_fourPlot}
\alias{analysis_fourPlot}
\title{Four Plots of Growth Curves and Doubling Times}
\usage{
analysis_fourPlot(timeseries, minExpFitHour = 0, maxExpFitHour = 1e+09,
  title = "Title Goes Here", filename = NULL, pngSize = 800)
}
\arguments{
\item{timeseries}{timeseries dataframe}

\item{minExpFitHour}{hour of first datapoint to include in doubling time exponential fit}

\item{maxExpFitHour}{hour of last datapoint to include in doubling time exponential fit}

\item{title}{overall title}

\item{filename}{path of the file to which the plot is saved (NULL will plot to screen)}

\item{pngSize}{image width/height in pixels}
}
\value{
Vector of doubling times
}
\description{
Given a "timeseries" dataframe with hours in the
first column and colony sizes in all other columns, create the following plots:
\enumerate{
  \item{growth plot of colony sizes}
  \item{growth plot of log of colony sizes}
  \item{boxplot of doubling times}
  \item{histogram of doubling times with boxplot outliers removed}
}
}
\details{
The R boxplot() function is used to define outliers and has the following documentation
where IQR stands for Inter-Quartile Range:

The two 'hinges' are versions of the first and third quartile, i.e., close to
quantile(x, c(1,3)/4). The hinges equal the quartiles for odd n (where n <- length(x))
and differ for even n. Whereas the quartiles only equal observations for n %% 4 == 1 (n = 1 mod 4),
the hinges do so additionally for n %% 4 == 2 (n = 2 mod 4), and are in the middle
of two observations otherwise.

The notches (if requested) extend to +/-1.58 IQR/sqrt(n). This seems to be based on
the same calculations as the formula with 1.57 in Chambers et al (1983, p. 62), given
in McGill et al (1978, p. 16). They are based on asymptotic normality of the median
and roughly equal sample sizes for the two medians being compared, and are said to
be rather insensitive to the underlying distributions of the samples. The idea appears
to be to give roughly a 95% confidence interval for the difference in two medians.
}
\seealso{
\link{analysis_doublingTime}
}

