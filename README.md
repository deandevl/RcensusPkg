# RcensusPkg
Contains R script for obtaining data from the US Census API.

To install *RcensusPkg* enter the following command lines from an R console:

```R
install.packages("devtools")
```

```R
devtools::install_github("deandevl/RcensusPkg")
```

The repository contains an html document named `intro_to_RcensusPkg.html` that introduces the package along with numerous demonstrations of the individual functions. 

The following table list the functions from the package:

| Function                   | Description                                                  |
| -------------------------- | ------------------------------------------------------------ |
| get_category_strings       | For a Census Bureau categorical variable return a data.table with the variable's integer values and corresponding label strings. |
| get_dataset_names          | Get the acronym names and descriptions of the Census Bureau's datasets. |
| get_geography              | Get the list of geography entities available (state, county, tract, etc) for a specific dataset. |
| get_groups                 | Get the names of Census Bureau variable groups and their descriptive parameters. Function produces a data.table of variable groups/tables and their descriptions. |
| get_multi_vintage_data     | Get Census Bureau data for a specific dataset, variables, and region in the form of a dataframe for multiple vintages. |
| get_variable_names         | Get Census Bureau variable acronym names and their label descriptions. |
| get_vintage_data           | Key function that gets Census Bureau data for a specific dataset, variables, and region in the form of a data.table. |
| join_it                    | Outer join two dataframes that have a common column variable. Function uses fast data.table techniques to join two data.tables by their common key values. |
| long_to_wide               | Reshape a data.table from a "long" format to a "wide" format. Function calls `data.table::dcast()` to reshape a long single column and its values to multiple columns. |
| plot_us_data               | This function produces a ggplot2 based choropleth map of a discrete/continuous variable across all/selected US states including viewable placement of Alaska, Hawaii, and Puerto Rico. |
| remove_area_water          | Function removes water area geometries from downloaded Census Bureau shapefiles |
| tiger_block_groups_sf      | Returns simple feature (sf) of US Census block group boundary related geometric polygons, provided by the US Census Bureau's TIGER/Line Shapefiles database. |
| tiger_cbsa_sf              | Returns simple feature (sf) of core based statistical area (CBSA) boundary related geometric polygons, provided by the US Census Bureau's TIGER/Line Shapefiles database. |
| tiger_counties_sf          | Returns simple feature (sf) of the entire US county boundary related geometric polygons, provided by the US Census Bureau's TIGER/Line Shapefiles database. |
| tiger_county_subsection_sf | Returns simple feature (sf) of US Census county subsection boundary related geometric polygons, provided by the US Census Bureau's TIGER/Line Shapefiles database. |
| tiger_landmarks_sf         | Returns simple feature (sf) landmark related geometric polygons provided by the US Census Bureau's TIGER/Line Shapefiles database. |
| tiger_places_sf            | Returns simple feature (sf) places related geometric polygons provided by the US Census Bureau's TIGER/Line Shapefiles database. |
| tiger_roads_sf             | Returns simple feature (sf) of US Census roads boundary related geometric polygons, provided by the US Census Bureau's TIGER/Line Shapefiles database. |
| tiger_states_sf            | Returns simple feature (sf) of state boundary related geometric polygons, provided by the US Census Bureau's TIGER/Line Shapefiles database. |
| tiger_tracts_sf            | Returns simple feature (sf) of US Census tract boundary related geometric polygons, provided by the US Census Bureau's TIGER/Line Shapefiles database. |
| tiger_urban_area_sf        | Returns simple feature(sf) of the Census Bureau's defined "urban area" boundary related geometric polygons, provided by the US Census Bureau's TIGER/Line Shapefiles database. |
| tiger_water_sf             | Returns simple feature (sf) water related geometric polygons provided by the US Census Bureau's TIGER/Line Shapefiles database. |
| tiger_zctas_sf             | Returns simple feature (sf) shapefile Zip Code Tabulation Areas(ZCTA) shapefile of the entire US. |
| wide_to_long               | Reshape a data frame from a "wide" format to a "long" format. Function is a helper in calling data.table's melt() function to reshape a wide data frame to a long form. |

