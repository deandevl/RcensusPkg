# tiger_water_sf() area

    Code
      state_county_fips <- usmap::fips(state = "Ohio", county = "Geauga")
      state_fips <- substr(state_county_fips, 1, 2)
      county_fips <- substr(state_county_fips, 3, 5)
      output_dir <- withr::local_tempdir()
      if (!dir.exists(output_dir)) {
        dir.create(output_dir)
      }
      geauga_area_water_sf <- RcensusPkg::tiger_water_sf(state = state_fips, county = county_fips,
        output_dir = output_dir, delete_files = FALSE)
      a_plot <- RplotterPkg::create_sf_plot(geauga_area_water_sf)

# tiger_water_sf() linear

    Code
      state_county_fips <- usmap::fips(state = "Ohio", county = "Geauga")
      state_fips <- substr(state_county_fips, 1, 2)
      county_fips <- substr(state_county_fips, 3, 5)
      output_dir <- withr::local_tempdir()
      if (!dir.exists(output_dir)) {
        dir.create(output_dir)
      }
      geauga_linear_water_sf <- RcensusPkg::tiger_water_sf(state = state_fips,
        county = county_fips, entity = "linear", output_dir = output_dir,
        delete_files = FALSE)
      a_plot <- RplotterPkg::create_sf_plot(geauga_linear_water_sf)
      expect_true(is.ggplot(a_plot))
      vdiffr::expect_doppelganger("tiger_water_sf() linear", a_plot)
      expect_no_error(ggplot_build(a_plot))

