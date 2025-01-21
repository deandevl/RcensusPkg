# get_dataset_names() acs5

    Code
      acs5_datasets_ls <- RcensusPkg::get_dataset_names(vintage = 2020,
        filter_name_str = "acs5/")

---

    Code
      acs5_datasets_ls$data
    Output
                      name vintage
                    <char>   <int>
      1: acs/acs5/cprofile    2020
      2:  acs/acs5/profile    2020
      3:     acs/acs5/pums    2020
      4:   acs/acs5/pumspr    2020
      5:  acs/acs5/subject    2020
                                                                                              title
                                                                                             <char>
      1:                    American Community Survey: 5-Year Estimates: Comparison Profiles 5-Year
      2:                          American Community Survey: 5-Year Estimates: Data Profiles 5-Year
      3:             2020 American Community Survey: 5-Year Estimates - Public Use Microdata Sample
      4: 2020 American Community Survey: 5-Year Estimates - Public Use Microdata Sample Puerto Rico
      5:                         American Community Survey: 5-Year Estimates: Subject Tables 5-Year

