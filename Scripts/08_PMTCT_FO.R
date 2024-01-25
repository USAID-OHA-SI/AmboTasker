# PROJECT: PMB requests
# PURPOSE: Munge and Analysis of PMTCT dat
# AUTHOR:  Tim Esssam | SI
# REF ID:  a40dbc57
# LICENSE: MIT
# DATE:   2024-01-25
# NOTES:

# LOCALS & SETUP ============================================================================

  # Libraries
    library(gagglr)
    library(tidyverse)
    library(scales)
    library(glue)
    library(ggtext)


  # SI specific paths/functions
    load_secrets()
    merdata <- file.path(glamr::si_path("path_msd"))
    file_path <- return_latest(folderpath = merdata,
      pattern = "PSNU_IM_.*Zambia")

    file_path_old <- return_latest(folderpath = merdata,
                                   pattern = "PSNU_IM_FY15-20.*txt")

  # Grab metadata
   get_metadata(file_path)

  # REF ID for plots
    ref_id <- "a40dbc57"

  # Functions


# LOAD DATA ============================================================================

  df_pmtct <- read_psd(file_path) %>% filter(indicator %in% c("PMTCT_FO"))

  df_pmtct_old <- read_psd(file_path_old) %>% filter(operatingunit == "Zambia",
                                                     indicator %in% c("PMTCT_FO"))

# MUNGE ============================================================================



# VIZ ============================================================================

  #

# SPINDOWN ============================================================================

