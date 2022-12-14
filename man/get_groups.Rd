% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_groups.R
\name{get_groups}
\alias{get_groups}
\title{get_groups}
\usage{
get_groups(dataset = NULL, vintage = NULL)
}
\arguments{
\item{dataset}{A required string that sets the acronym name of the data set of interest (e.g. "acs/acs5")}

\item{vintage}{An required numeric that sets the year of interest.}
}
\value{
A data.table
}
\description{
Get the names of Census Bureau variable groups and their descriptive parameters
}
\details{
Function produces a data.table of variable groups/tables and their descriptions.
}
\author{
Rick Dean
}
