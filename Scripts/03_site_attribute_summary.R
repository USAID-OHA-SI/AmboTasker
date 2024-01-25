# PROJECT: Ad Hoc Analyses for Site level attributes
# PURPOSE: Munge and Analysis of USAID site level data
# AUTHOR:  Tim Esssam | SI
# REF ID:  c5730b8d
# LICENSE: MIT
# DATE:   2024-01-09
# NOTES:

# LOCALS & SETUP ============================================================================

  # Libraries
  library(gagglr)
  library(tidyverse)
  library(scales)
  library(glue)
  library(ggtext)
  library(gt)
  library(gtExtras)

  # SI specific paths/functions
  load_secrets()
  file_path <- return_latest(folderpath = "Data/",
                             pattern = "Data Exchange")


# LOAD AND SUMMARIZE ------------------------------------------------------

  df_atrb <- read_csv(file_path) %>% janitor::clean_names()

  df_atrb %>% names()

  df_atrb %>% count(facility_type, faith_based)

  df_atrb %>% count(facility_type, ownership_type)
