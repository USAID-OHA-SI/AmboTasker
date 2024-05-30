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


  site_path <- si_path() %>%
    return_latest("Site.*Zambia")

# LOAD AND SUMMARIZE ------------------------------------------------------

  df_atrb <- read_csv(file_path) %>% janitor::clean_names()
  df_site <- read_psd(site_path) %>% distinct(sitename, psnu, psnuuid, orgunituid, snu1)

  df_site_tx <-
    read_psd(site_path) %>%
    filter(indicator == "TX_CURR",
           standardizeddisaggregate == "Total Numerator") %>%
    group_by(sitename, psnu, psnuuid, orgunituid, snu1, fiscal_year) %>%
    summarize(TX_CURR = sum(cumulative, na.rm = T)) %>%
    ungroup() %>%
    spread(TX_CURR, fiscal_year)

  df_site_atrb <- df_atrb %>%
    left_join(df_site , by = c("datim_uid" = "orgunituid"))


  df_atrb %>% names()

  df_atrb %>% count(facility_type, faith_based)

  df_atrb %>% count(facility_type, ownership_type)



# SITES for NW province ---------------------------------------------------

  df_site_atrb %>%
    filter(str_detect(snu1, "NorthWestern")) %>% View()

