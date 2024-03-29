% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cache_dataframe.R
\name{cache_dataframe}
\alias{cache_dataframe}
\title{Cache datafra,e}
\usage{
cache_dataframe(data_vec, sel_dir, clean_name = FALSE)
}
\arguments{
\item{data_vec}{A named list containing the data.frames.}

\item{sel_dir}{A character string specifying the directory to save the
dataframes.}

\item{clean_name}{A logical value specifying whether to clean the names of
the dataframes before saving them. Default is \code{FALSE}.}
}
\description{
Takes a named list with dataframes and caches the results by writing
each dataframe into a separate text file.
}
