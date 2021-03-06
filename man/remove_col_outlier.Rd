% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/remove_col_outlier.R
\name{remove_col_outlier}
\alias{remove_col_outlier}
\title{Remove rows of a data frame based on column outliers}
\usage{
remove_col_outlier(df, col, threshold = 3, quiet = T)
}
\arguments{
\item{df}{The data frame to search through}

\item{col}{The name or index of the column in the data frame.}

\item{threshold}{Indices of values that are farther from the mean
than the \code{threshold} times the  deviation are removed. The
standard deviation is obtained using \verb{[sd()]}. The default value
is \code{threshold = 3}.}

\item{quiet}{Whether to print a message on the number of outliers
removed. The default is \code{quiet = F}.}
}
\value{
A data frame with rows containing outliers in a specified
column removed.
}
\description{
\code{remove_col_outlier} removes rows in a data frame based on the
outliers in a column.
It is a wrapper for \verb{[find_col_outlier()]}.
}
