library(dplyr)
source('functions/fwf_statcan_census.R')


file_paths <- 
  c(
    '2016' = 'data/raw/2016_Ind_98M0001X/data_donnees_2016_ind.dat',
    '2011' = 'data/raw/2011_NHS-ENM_Ind_99M0001X/Individual file/data_donnees.dat',
    '2006' = 'data/raw/2006/indiv_pumf06.dat',
    '2001' = 'data/raw/2001_Ind_96M0016XCB/indiv.dat'
  )

purrr::walk2(
  names(file_paths),
  file_paths,
  function(x, y) {
    
    tictoc::tic(
      paste0('Reading and saving data for: ', x)
    )
    
    df <-
      readr::read_fwf(
        y,
        get_fwf_statcan_census(x),
        col_types = 'n'
      ) %>% 
      mutate(
        file_year = x
      )
    
    arrow::write_parquet(
      df,
      paste0('data/processed/statcan_census_ind_', x, '.parquet')
      )
    
    tictoc::toc()
    
  }
)

