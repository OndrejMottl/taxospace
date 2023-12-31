% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_classification_bulk.R
\name{get_classification_buk}
\alias{get_classification_buk}
\title{Get classification for given vector of taxa}
\usage{
get_classification_buk(
  taxa_vec,
  sel_db_name = c("gnr", "iplant"),
  sel_db_class = c("gbif", "itis"),
  verbose = FALSE
)
}
\arguments{
\item{taxa_vec}{A character vector of taxa names to classify.}

\item{sel_db_name}{A string indicating which database to use to
resolve names. Default is "gnr".}

\item{sel_db_class}{A string indicating which database to use for
classification. Default is "gbif".}

\item{verbose}{Logical. If TRUE the additional messages are printed on the console.}
}
\value{
A data frame with classification information for each taxa name.
}
\description{
This function takes a vector of taxa names and returns the classification
for each of them using the specified database. The default is to use the
GBIF database, but it can be changed to ITIS with the "sel_db_class"
parameter.
}
\examples{
get_classification_buk(c("Homo sapiens", "Panthera tigris"))
}
\seealso{
\link{get_classification}, \link[taxize:resolve]{taxize::resolve}, \link[taxize:classification]{taxize::classification}
}
