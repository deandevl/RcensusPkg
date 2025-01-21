# get_multi_vintage_data()

    Code
      deschutes_fips <- usmap::fips("OR", "Deschutes")
      state <- substr(deschutes_fips, 1, 2)
      county <- substr(deschutes_fips, 3, 5)
      head(RcensusPkg::get_multi_vintage_data(dataset = "acs/acs1", vintage_v = 2005:
        2019, vars = c("B25077_001E", "B25077_001M"), region = paste0("county:",
        county), regionin = paste0("state:", state)))
    Output
                             NAME B25077_001E B25077_001M  state county  GEOID
                           <char>      <char>      <char> <char> <char> <char>
      1: Deschutes County, Oregon      236100       13444     41    017  41017
      2: Deschutes County, Oregon      336600       11101     41    017  41017
      3: Deschutes County, Oregon      356700       16765     41    017  41017
      4: Deschutes County, Oregon      331600       17104     41    017  41017
      5: Deschutes County, Oregon      284300       12652     41    017  41017
      6: Deschutes County, Oregon      260700       18197     41    017  41017
         vintage
           <int>
      1:    2005
      2:    2006
      3:    2007
      4:    2008
      5:    2009
      6:    2010

