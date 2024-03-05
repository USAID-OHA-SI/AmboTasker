# PROJECT: Download pano data for FY24Q1
# PURPOSE: Automate downloads
# AUTHOR:  Tim Esssam | SI
# REF ID:  d03fd8af
# LICENSE: MIT
# DATE:   2024-02-26
# NOTES:

# LOCALS & SETUP ============================================================================

  # Libraries
    library(gagglr)
    library(grabr)
    library(tidyverse)
    library(glue)
    library(purrr)


  # SI specific paths/functions
  #store Pano creds (first time only)
  set_pano()

  #establish session
  sess <- pano_session(username = pano_user(),
                       password = pano_pwd())


# MUNGE -------------------------------------------------------------------

  # Download latest Malawi and Zambia Data
  map(c('Zambia', 'Malawi'), ~pano_extract_msds(operatingunit = .x,
                    archive = TRUE,
                    dest_path = si_path(),
                    username = pano_user(),
                    password = pano_pwd()),
      .progress = TRUE)



