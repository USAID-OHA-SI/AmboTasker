# PURPOSE: AmboTasker
# AUTHOR:  Baboyma Kagniniwa | USAID/OHA/SIEI/SI
# PURPOSE: PrEP Trend by Agency and SNU
# REF ID:  585a7cff
# LICENSE: MIT
# DATE:    2024-01-25
# UPDATE:  2024-01-25
# NOTES:   Zambia Clinical Scenarioes

# Libraries ====

  library(tidyverse)
  library(glamr)
  library(gophr)
  library(glitr)
  library(gisr)
  library(sf)
  library(scales)
  library(extrafont)
  library(tidytext)
  library(patchwork)
  library(ggtext)
  library(glue)


# LOCALS & SETUP ====

  # Download files

  #grabr::pano_extract_msds("Zambia", add_global = F)

  # Set paths

  dir_data   <- "Data"
  dir_dataout <- "Dataout"
  dir_images  <- "Images"
  dir_graphics  <- "Graphics"

  dir_mer <- glamr::si_path("path_msd")
  dir_ras <- glamr::si_path("path_raster")
  dir_shp <- glamr::si_path("path_vector")
  dir_cntry <- file.path("../../PEPFAR/COUNTRIES/Zambia")

  # Files

  file_nat <- si_path() %>% return_latest("NAT_SUBNAT")
  file_psnu <- si_path() %>% return_latest("PSNU_IM_FY21-.*_Zambia")
  file_site <- si_path() %>% return_latest("Site_IM_FY21-.*_Zambia")

  get_metadata(file_psnu)

  meta <- metadata

  # Set Params

  ref_id <- "585a7cff"
  agency <- "USAID"
  cntry <- "Zambia"

  curr_fy <-source_info(return = "fiscal_year")
  curr_pd <- source_info(return = "period")

  msd_source <- source_info()

  authors <- c("Baboyma Kagniniwa", "Tim Essam")

# Functions  =====

# LOAD DATA =====

  df_msd <- read_psd(file_psnu)


# MUNGE =====

  df_prep <- df_msd %>%
    filter(str_detect(indicator, "PrEP"),
           standardizeddisaggregate == "Total Numerator") %>%
    clean_agency() %>%
    filter(funding_agency %in% c(agency, "CDC"))

  #df_prep %>% distinct(indicator, standardizeddisaggregate)

  df_prep_sum_agency <- df_prep %>%
    summarise(across(where(is.numeric), \(.x) sum(.x, na.rm = T)),
              .by = c(fiscal_year, funding_agency, indicator)) %>%
    mutate(fyear = fiscal_year) %>%
    reshape_msd() %>%
    group_by(funding_agency, snu1, indicator) %>%
    arrange(period) %>%
    mutate(
      value_cum = case_when(
        indicator == "PrEP_NEW" ~ cumsum(value),
        TRUE ~ value)
    )

  df_prep_sum <- df_prep %>%
    summarise(across(where(is.numeric), \(.x) sum(.x, na.rm = T)),
              .by = c(fiscal_year, funding_agency, snu1, indicator)) %>%
    mutate(fyear = fiscal_year) %>%
    reshape_msd() %>%
    group_by(period_type, funding_agency, snu1, indicator) %>%
    arrange(period) %>%
    mutate(
      value_cum = case_when(
        period_type == "results" & indicator == "PrEP_NEW" ~ cumsum(value),
        TRUE ~ value
      ),
      targets = case
    ) %>%
    ungroup() %>%



# VIZ =====

  fy_start <- 2021
  fy_end <- 2023

  prep_cum <- df_prep %>%
    filter(fiscal_year == curr_fy) %>%
    count(wt = cumulative) %>%
    pull()

  pd_breaks <- df_prep_sum %>%
    filter(period_type == "results") %>%
    distinct(period) %>%
    arrange(period) %>%
    pull()

  df_prep_sum %>%
    filter(indicator %in% c("PrEP_NEW"),
           period_type == "results",
           fyear != fy_start) %>%
    ggplot(aes(period, value_cum, group = funding_agency)) +
    geom_area(fill = scooter, color = scooter, alpha = .2, linewidth = 1, na.rm = TRUE) +
    geom_vline(xintercept = fy_start, color = "white",
               linewidth = .9, linetype = "dotted") +
    geom_point(shape = 21, fill = "white", color = scooter, stroke = 1.5, na.rm = TRUE) +
    scale_y_continuous(position = "right", expand = c(.01, .01), labels = label_number(scale = .001, suffix = "K")) +
    scale_x_discrete(breaks = pd_breaks, labels = str_remove(pd_breaks, "FY[:digit:]{2}(?!Q1)")) +
    coord_cartesian(clip = "off") +
    facet_wrap(funding_agency ~ snu1, ncol = 3) +
    labs(x = NULL, y = NULL,
         title = glue("Zambia has initiated {comma(prep_cum)} \\
                      onto PrEP in {curr_fy}") %>% toupper,
         subtitle = "Pre-Exposure Prophylaxis (PrEP) Quarterly Results",
         caption = glue("Source: {msd_source}
                        SI analytics: {paste(authors, collapse = '/')} | {ref_id}
                     US Agency for International Development")) +
    si_style_ygrid()

# OUTPUTS =====

