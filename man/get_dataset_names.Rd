% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_dataset_names.R
\name{get_dataset_names}
\alias{get_dataset_names}
\title{get_dataset_names}
\usage{
get_dataset_names(
  vintage = NULL,
  filter_name_str = NULL,
  filter_title_str = NULL,
  ignore_case = TRUE,
  brief = TRUE
)
}
\arguments{
\item{vintage}{A numeric for the year to select the datasets. If NULL, then all the years are returned.}

\item{filter_name_str}{A character string by which to filter the resultant data.table using the
"name" column.}

\item{filter_title_str}{A character string by which to filter the resultant data.table using the
"title" column.}

\item{ignore_case}{A logical which if FALSE will not ignore case in filtering the "title" column.}

\item{brief}{A logical which if TRUE will return a resultant data.table with just columns
"name", "vintage", "title". The default is TRUE.}
}
\value{
A data.table
}
\description{
Get the acronym names and descriptions of the Census Bureau's datasets.
}
\details{
Function produces a data.table/dataframe of the
Census Bureau's dataset acronym names that can be used in other \code{RcensusPkg::}
functions calling for a dataset acronym name. See Census Bureau's publicly available
\href{https://www.census.gov/data/developers/data-sets.html}{datasets} for
descriptions.
}
\author{
Rick Dean
}
