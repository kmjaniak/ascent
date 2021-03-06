% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/clean_ascent_csvs.R
\name{clean_ascent_csvs}
\alias{clean_ascent_csvs}
\title{Clean Ascent CSVs}
\usage{
clean_ascent_csvs(batch_names, inst = TRUE, stream = TRUE, posix = TRUE,
  direct = FALSE)
}
\arguments{
\item{batch_names}{List of batch names}

\item{inst}{Defaults to TRUE. If FALSE will not extract the instrument from the batch name}

\item{stream}{Defaults to TRUE. If FALSE will not extract the stream from the batch name}

\item{posix}{Defaults to TRUE. If FALSE will not convert AcqTime to type POSIXct}

\item{direct}{Defaults to FALSE. If TRUE will read batch_names input as file path, without adding folder/extension to
input string}
}
\description{
Reads Ascent csvs, generates a data frame that is easy to manipulate.
}
\examples{
clean_ascent_csvs(batch_names)
}
\keyword{ascent}
