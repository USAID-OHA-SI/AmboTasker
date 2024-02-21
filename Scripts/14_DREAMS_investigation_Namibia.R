# PROJECT: Testing
# PURPOSE: Munge and Analysis of
# AUTHOR:  Tim Esssam | SI
# REF ID:  fbe1fa02
# LICENSE: MIT
# DATE:   2024-02-07
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
      pattern = "Genie-PSNUByIM.*Namibia")

  # Grab metadata
   get_metadata(file_path)

  # REF ID for plots
    ref_id <- "fbe1fa02"

  # genie files
    genie_list <- list.files("Data", pattern = "Genie", full.names = T)

  # Function to read data, munge and spit out table
    sum_agyw <- function(genie_file){

      quarters <- str_extract(genie_file, "Q\\d")

      df <- read_psd(genie_file) %>% filter(indicator == "AGYW_PREV")

      df %>%
        count(numeratordenom, indicator, standardizeddisaggregate,
              fiscal_year, wt = targets) %>%
        spread(fiscal_year, n) %>%
        mutate(genie_quarter = quarters)
    }

    genie_list

# LOAD DATA ============================================================================

  df_genie <- read_psd(file_path)
  map(genie_list, ~sum_agyw(.x))

# MUNGE ============================================================================

  df_genie %>%
      count(numeratordenom, indicator, standardizeddisaggregate, fiscal_year, wt = targets) %>%
      spread(fiscal_year, n)

# VIZ ============================================================================

  #

# SPINDOWN ============================================================================

