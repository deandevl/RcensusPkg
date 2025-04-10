% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_vintage_data.R
\name{get_vintage_data}
\alias{get_vintage_data}
\title{get_vintage_data}
\usage{
get_vintage_data(
  dataset = NULL,
  vintage = NULL,
  vars = NULL,
  NAME_GEOID = TRUE,
  predicates = NULL,
  group = NULL,
  wide_to_long = FALSE,
  region = NULL,
  regionin = NULL,
  na_cols = NULL,
  key = Sys.getenv("CENSUS_KEY")
)
}
\arguments{
\item{dataset}{A required string that sets the name of the data set of interest (e.g. "acs/acs5").
Descriptions/vintages for datasets can be found by running
\code{RcensusPkg::get_dataset_names()}.}

\item{vintage}{An optional numeric that sets the vintage of interest. Available vintages
for a specific dataset can be found by running \code{RcensusPkg::get_dataset_names()}.}

\item{vars}{A required string vector (if the parameter 'group' is \code{NULL}) of variable names to be acquired.
Available variable names can be determined by running \code{RcensusPkg::get_variable_names()}.}

\item{NAME_GEOID}{A logical which if \code{TRUE} will add "NAME" and "GEO_ID" variables to the 'vars' string vector.
The default is \code{TRUE}.}

\item{predicates}{An optional vector of strings that adds data filtering.
See \href{https://www.census.gov/data/developers/guidance.html}{Census Data API User Guide} for
forming predicates and filtering or limiting variables. As noted in the guide each predicate must
start with an ampersand sign.}

\item{group}{An optional string that names an entire group of similar variables to be retrieved.
For example the group value "B01001" would return values of all variables related to
"SEX BY AGE". To find available groups submit a dataset and vintage to \code{RcensusPkg::get_groups}.}

\item{wide_to_long}{If 'group' is defined then the returned \code{data.table} is normally in a wide format with all the group variables as columns.
If this logical parameter is \code{TRUE} then a long format is returned with group variable names in one column (named "estimate") and
their respective values in another column (named "value").}

\item{region}{An optional string that specifies the geography of the request. See
\href{https://www.census.gov/library/reference/code-lists/ansi.html}{Federal Information Processing Series (FIPS)}
for a listing of codes for this and the 'regionin' parameter. Not all regions such as counties,
blocks, or tracts are available for a specific dataset and vintage. Use
\code{RcensusPkg::get_geography()} to check on both 'region' and 'regionin'.}

\item{regionin}{An optional string that sets a qualifier for 'region'.}

\item{na_cols}{If \code{TRUE} will remove all rows with missing values. If a
vector of column names/integers will check only those columns for missing values.}

\item{key}{A required string that sets the access key. All Census Bureau API requests require an access key.
Sign-up for a key is free and can be obtained \href{https://api.census.gov/data/key_signup.html}{here}.
The function will check for a global setting of the key via \code{Sys.getenv("CENSUS_KEY")}.
Run \code{usethis::edit_r_environ()} and edit your .Renviron file with the line: CENSUS_KEY=\emph{your key}
to create the global association.}
}
\value{
A \code{data.table}
}
\description{
Get Census Bureau data for a specific dataset, variables, and region
in the form of a \code{data.table}.

Function produces a \code{data.table} with selected Census Bureau variables
as columns. The function requires an access key issued from the Bureau.
Variables of interest can be specified individually or by group/table name.
Predicate phrases can be specified for filtering the results.
}
\details{
See Census Bureau's publicly available
\href{https://www.census.gov/data/developers/data-sets.html}{datasets} for
dataset descriptions.

Some of the more common Census Bureau datasets include:
\itemize{
\item\href{https://www.census.gov/data/developers/data-sets/acs-1year.html}{American Community Survey 1-year Data}
\item\href{https://www.census.gov/data/developers/data-sets/ACS-supplemental-data.html}{American Community Survey 1-year Supplemental Data}
\item\href{https://www.census.gov/data/developers/data-sets/acs-5year.html}{American Community Survey 5-Year Data}
\item\href{https://www.census.gov/data/developers/data-sets/decennial-census.html}{Decennial Census}
\item\href{https://www.census.gov/data/developers/data-sets/economic-census.html}{Economic Census}
\item\href{https://www.census.gov/data/developers/data-sets/popest-popproj/popest.html}{Population Estimates and Projections}
\item\href{https://www.census.gov/data/developers/data-sets/abs.html}{Annual Business Survey (ABS)}
}
}
\examples{
\dontrun{
  # Requires Census Bureau API key
  # ------American Community Survey 5-Year Data (2009-2021)-----------
  # Description: https://www.census.gov/data/developers/data-sets/acs-5year.html

  library(data.table)
  library(httr2)
  library(jsonlite)
  library(stringr)
  library(RcensusPkg)

  # Get the data for the total number of white females ("B01001A_017E") and
  # total number of white individuals ("B01001A_001E") across the US

  white_females_dt <- RcensusPkg::get_vintage_data(
    dataset = "acs/acs5",
    vars = c("B01001A_017E", "B01001A_001E"),
    vintage = 2021,
    region = "state:*"
  )
}
}
