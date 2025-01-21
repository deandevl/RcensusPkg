# remove_area_water()

    Code
      ny_state_county_fips <- usmap::fips(state = "New York", county = "New York")
      ny_state_fips <- substr(ny_state_county_fips, 1, 2)
      ny_county_fips <- substr(ny_state_county_fips, 3, 5)
      output_dir <- withr::local_tempdir()
      if (!dir.exists(output_dir)) {
        dir.create(output_dir)
      }
      express <- parse(text = paste0("COUNTYFP == ", "\"", ny_county_fips, "\""))
      ny_tracts_sf <- RcensusPkg::tiger_tracts_sf(state = ny_state_fips, vintage = 2020,
        general = FALSE, transform_crs = 6538, express = express, output_dir = output_dir,
        delete_files = TRUE)
      without_water_sf <- RcensusPkg::remove_area_water(ny_tracts_sf, vintage = 2020,
        output_dir = output_dir, delete_files = TRUE)
    Condition
      Warning:
      attribute variables are assumed to be spatially constant throughout all geometries
    Code
      a_plot <- RplotterPkg::create_sf_plot(sf = without_water_sf, title = "New York, New York without water")

