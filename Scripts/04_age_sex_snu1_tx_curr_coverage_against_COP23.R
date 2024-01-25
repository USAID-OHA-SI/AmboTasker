# PROJECT: Ad Hoc Analyses
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
  library(mmtable2)
  library(ggbeeswarm)
  library(ggdist)

  # SI specific paths/functions
  load_secrets()
  merdata <- file.path(glamr::si_path("path_msd"))
  file_path <- return_latest(folderpath = merdata,
                             pattern = "Site_IM_FY21-24.*_Zambia")

  # Grab metadata
  get_metadata(file_path)


# LOAD DATA ============================================================================

  df_site <- read_psd(file_path)

  # How many mechanisms per province?
  df_site %>%
    filter(fiscal_year == metadata$curr_fy,
           prime_partner_name != "Dedup") %>%
    distinct(mech_name, mech_code, prime_partner_name) %>%
    arrange(mech_name) %>%
    prinf()


# Need a table of provincial target achievement for TX_CURR using FY23Q4 TX_CURR / COP23 targets
# Seems that we need to do 2024 separate from 2023
  df_snu1_age_sex_24 <-
    df_site %>%
    filter(standardizeddisaggregate == "Age/Sex/HIVStatus",
           indicator == "TX_CURR",
           ageasentered %in% c("01-09", "10-14", "15-24", "25-34", "35-49", "50+"),
           fiscal_year == 2024) %>%
    group_by(fiscal_year, ageasentered, snu1, sex) %>%
    summarise(across(c(targets), \(x) sum(x, na.rm = T))) %>%
    ungroup()

  df_snu1_age_sex_23 <-
    df_site %>%
    filter(standardizeddisaggregate == "Age/Sex/HIVStatus",
           indicator == "TX_CURR",
           ageasentered %in% c("01-04", "05-09", "10-14", "15-19", "20-24",
                               "25-29", "30-34", "35-39", "40-44", "45-49",
                               "50-54", "55-59", "60-64", "65+"),
           fiscal_year == 2023) %>%
    mutate(ageasentered = case_when(
      ageasentered %in% c("01-04", "05-09") ~ "01-09",
      ageasentered %in% c("15-19", "19-24") ~ "15-24",
      ageasentered %in% c("25-29", "30-34") ~ "25-34",
      ageasentered %in% c("35-39", "40-44", "45-49") ~ "35-49",
      ageasentered %in% c("50-54", "55-59", "60-64", "65+") ~ "50+",
      TRUE ~ ageasentered
    )) %>%
    group_by(fiscal_year, ageasentered, snu1, sex) %>%
    summarise(fy23_cumulative = sum(cumulative, na.rm = T)) %>%
    mutate(fiscal_year = 2024)

  df_snu1_age_sex_24 %>% left_join(df_snu1_age_sex_23) %>%
    mutate(cop23_cov = fy23_cumulative / targets,
           fill_color = ifelse(cop23_cov >= .90, hw_hunter, hw_orchid_bloom)) %>%
    ggplot(aes(x = ageasentered, y = snu1)) +
    geom_tile(aes(fill = fill_color), color = "white", alpha = 0.75) +
    facet_wrap(~sex) +
    geom_text(aes(label = percent(cop23_cov, 1.0)),
              size = 7/.pt) +
      scale_fill_identity() +
    si_style_nolines(facet_space = 0.5)
