# PROJECT: MOH Treatment data analysis
# PURPOSE: Munge and Analysis of TX_CURR gaps
# AUTHOR:  Tim Esssam | SI
# REF ID:  acffab5c
# LICENSE: MIT
# DATE:   2024-01-18
# NOTES:

# LOCALS & SETUP ============================================================================

  # Libraries
    library(gagglr)
    library(tidyverse)
    library(scales)
    library(glue)
    library(ggtext)
    library(readxl)


  # SI specific paths/functions
    load_secrets()
    merdata <- file.path(glamr::si_path("path_msd"))
    site_path <- return_latest(folderpath = merdata,
                               pattern = "Site_IM_FY21-24.*_Zambia")
    file_path <- return_latest(folderpath = "Data",
                               pattern = "MoH")

    iit_path <- "Data/Zambia_Continuity in Treatment Dashboard_FY23 PreClean Q4.xlsb"

  # Grab metadata
   get_metadata(site_path)

  # REF ID for plots
    ref_id <- "acffab5c"

  # Functions


# LOAD DATA ============================================================================

    df_moh <- read_excel(file_path, skip = 1, sheet = 3) %>%
      janitor::clean_names() %>%
      select(-c(x15:x18))

    df_moh_long <-
      df_moh %>%
      pivot_longer(currently_on_art_female_5_9_y:x35_male,
                   names_to = "age_sex",
                   values_to = "tx_curr_moh")
  names(df_moh)

    df_iit <- read_excel(iit_path, )

# MUNGE ============================================================================

  #

# VIZ ============================================================================

  #

# SPINDOWN ============================================================================

