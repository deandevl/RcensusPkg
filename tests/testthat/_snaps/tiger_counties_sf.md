# tiger_counties_sf() Ohio

    Code
      ohio_fips <- usmap::fips(state = "ohio")
      express <- parse(text = paste0("STATEFP == ", "\"", ohio_fips, "\""))
      output_dir <- withr::local_tempdir()
      if (!dir.exists(output_dir)) {
        dir.create(output_dir)
      }
      ohio_counties_sf <- RcensusPkg::tiger_counties_sf(output_dir = output_dir,
        delete_files = FALSE, general = TRUE, express = express)
      a_plot <- RplotterPkg::create_sf_plot(sf = ohio_counties_sf)

