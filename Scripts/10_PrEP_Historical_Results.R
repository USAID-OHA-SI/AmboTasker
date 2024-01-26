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
  file_psnu1 <- si_path() %>% return_latest("PSNU_IM_FY15")
  file_psnu2 <- si_path() %>% return_latest("PSNU_IM_FY21")
  file_site1 <- si_path() %>% return_latest("Site_IM_FY15-.*_Zambia")
  file_site2 <- si_path() %>% return_latest("Site_IM_FY21-.*_Zambia")

  get_metadata(file_psnu1)

  meta <- metadata

  # Set Params

  ref_id <- "585a7cff"
  agency <- "USAID"
  cntry <- "Zambia"

  curr_fy <- source_info(return = "fiscal_year")
  curr_pd <- source_info(return = "period")

  msd_source <- source_info()

  authors <- c("Baboyma Kagniniwa", "Tim Essam")

# Functions  =====

# LOAD DATA =====

  df_msd <- c(file_psnu1, file_psnu2) %>%
    map(function(.x) {
      read_psd(.x) %>%
        filter(operatingunit == cntry)
    }) %>%
    bind_rows()

# MUNGE =====

  df_prep <- df_msd %>%
    filter(operatingunit == cntry,
           str_detect(indicator, "PrEP"),
           standardizeddisaggregate == "Total Numerator") %>%
    clean_agency() %>%
    filter(funding_agency %in% c(agency, "CDC"))

  # Agency summary

  df_prep_sum_agency <- df_prep %>%
    summarise(across(.cols = all_of(c("cumulative", "targets")), \(.x) sum(.x, na.rm = T)),
              .by = c(fiscal_year, funding_agency, indicator)) %>%
    mutate(fyear = paste("FY", str_sub(fiscal_year, 3,4)),
           achv = cumulative / targets) %>%
    group_by(funding_agency, indicator) %>%
    arrange(fiscal_year) %>%
    mutate(
      cum_sum = case_when(
        indicator == "PrEP_NEW" ~ cumsum(cumulative),
        TRUE ~ cumulative)
    ) %>%
    ungroup() %>%
    relocate(cum_sum, .after = cumulative)

  df_prep_sum_snu <- df_prep %>%
    summarise(across(all_of(c("cumulative", "targets")), \(.x) sum(.x, na.rm = T)),
              .by = c(fiscal_year, funding_agency, snu1, indicator)) %>%
    mutate(fyear = paste("FY", str_sub(fiscal_year, 3,4)),
           achv = cumulative / targets) %>%
    group_by(funding_agency, snu1, indicator) %>%
    arrange(fiscal_year) %>%
    mutate(
      cum_sum = case_when(
        indicator == "PrEP_NEW" ~ cumsum(cumulative),
        TRUE ~ cumulative
      )
    ) %>%
    ungroup() %>%
    relocate(cum_sum, .after = cumulative)

  df_prep_sum_snu2 <- df_prep %>%
    summarise(across(all_of(c("cumulative", "targets")), \(.x) sum(.x, na.rm = T)),
              .by = c(fiscal_year, snu1, indicator)) %>%
    mutate(fyear = paste0("FY", str_sub(fiscal_year, 3,4)),
           achv = cumulative / targets) %>%
    group_by(snu1, indicator) %>%
    arrange(fiscal_year) %>%
    mutate(
      snu1 = str_remove(snu1, " Province"),
      cum_sum = case_when(
        indicator == "PrEP_NEW" ~ cumsum(cumulative),
        TRUE ~ cumulative
      )
    ) %>%
    ungroup() %>%
    relocate(cum_sum, .after = cumulative)

# VIZ =====

  fy_start <- 2019 #df_prep_sum_agency %>% pull(fiscal_year) %>% min()
  fy_end <- curr_fy

  prep_cum <- df_prep %>%
    filter(fiscal_year == curr_fy,
           indicator == "PrEP_NEW") %>%
    count(wt = cumulative) %>%
    pull()

  pd_breaks <- df_prep_sum_snu2 %>%
    filter(fiscal_year >= fy_start, fiscal_year <= fy_end) %>%
    distinct(fyear) %>%
    arrange(fyear) %>%
    pull()

  df_prep_sum_agency %>%
    filter(indicator %in% c("PrEP_NEW"),
           fiscal_year >= fy_start, fiscal_year <= fy_end) %>%
    ggplot(aes(fyear, cum_sum, group = funding_agency)) +
    geom_area(fill = scooter, color = scooter, alpha = .2, linewidth = 1, na.rm = TRUE) +
    geom_vline(xintercept = fy_start, color = "white",
               linewidth = .9, linetype = "dotted") +
    geom_point(shape = 21, fill = "white", color = scooter, stroke = 1.5, na.rm = TRUE) +
    geom_text(data = df_prep_sum_agency %>%
                filter(indicator %in% c("PrEP_NEW"),
                       fiscal_year %in% c(fy_start, fy_end)),
              aes(label = paste0(comma(cum_sum), "\n(", comma(cumulative), ")")),
              hjust = 1, vjust = -.3, fontface = "bold") +
    geom_text(data = df_prep_sum_agency %>%
                filter(indicator %in% c("PrEP_NEW"),
                       fiscal_year %ni% c(fy_start, fy_end),
                       fiscal_year >= fy_start,
                       fiscal_year <= fy_end),
              aes(label = paste0("(", comma(cumulative), ")")),
              size = 3, hjust = 1, vjust = -1) +
    scale_y_continuous(position = "right", expand = c(.01, .01), labels = label_number(scale = .001, suffix = "K")) +
    coord_cartesian(clip = "off") +
    facet_wrap(~funding_agency) +
    labs(x = NULL, y = NULL,
         title = glue("Zambia has initiated {comma(prep_cum)} \\
                      onto PrEP in {curr_fy}") %>% toupper,
         subtitle = glue("**{fy_start}** to **{fy_end}** Pre-Exposure Prophylaxis (PrEP) Cumulative Annual Program Results (APR)"),
         caption = glue("Source: {msd_source}
                        SI analytics: {paste(authors, collapse = '/')} | {ref_id}
                     US Agency for International Development")) +
    si_style_ygrid() +
    theme(plot.subtitle = element_markdown(),
          strip.text = element_text(face = "bold"))

  si_save("Graphics/prep_agency_trends.png")
  si_save("Graphics/prep_agency_trends.svg")

  # SNU1

  snu_order <- df_prep_sum_snu2 %>%
    filter(fiscal_year == curr_fy,
           indicator == "PrEP_NEW") %>%
    arrange(desc(cum_sum)) %>%
    pull(snu1)


  df_prep_sum_snu2 %>%
    filter(indicator %in% c("PrEP_NEW"),
           fiscal_year >= fy_start, fiscal_year <= fy_end) %>%
    ggplot(aes(fyear, cum_sum, group = snu1)) +
    geom_area(fill = scooter, color = scooter, alpha = .2, linewidth = 1, na.rm = TRUE) +
    geom_vline(xintercept = fy_start, color = "white",
               linewidth = .9, linetype = "dotted") +
    geom_point(shape = 21, fill = "white", color = scooter, stroke = 1.1, na.rm = TRUE) +
    geom_text(data = df_prep_sum_snu2 %>%
                filter(indicator %in% c("PrEP_NEW"),
                       fiscal_year %in% c(fy_start, fy_end)),
              aes(label = paste0(comma(cum_sum), "\n(", comma(cumulative), ")")),
              #aes(label = comma(cumulative)),
              size = 2, hjust = 1, vjust = -.3, fontface = "bold") +
    scale_y_continuous(position = "right", expand = c(.01, .01), labels = label_number(scale = .001, suffix = "K")) +
    scale_x_discrete(breaks = pd_breaks, labels = str_replace(pd_breaks, "FY", "`")) +
    coord_cartesian(clip = "off") +
    facet_wrap(~ factor(snu1, snu_order), nrow = 2, scales = "free_y") +
    labs(x = NULL, y = NULL,
         title = glue("Zambia has initiated {comma(prep_cum)} \\
                      onto PrEP in {curr_fy}") %>% toupper,
         subtitle = glue("**{fy_start}** to **{fy_end}** Pre-Exposure Prophylaxis (PrEP) Cumulative Annual Program Results (APR)"),
         caption = glue("Source: {msd_source}
                        SI analytics: {paste(authors, collapse = '/')} | {ref_id}
                     US Agency for International Development")) +
    si_style_ygrid() +
    theme(plot.subtitle = element_markdown(),
          strip.text = element_text(face = "bold"))

  si_save("Graphics/prep_snu1_trends.png",
          width = 10, height = 6)
  si_save("Graphics/prep_snu1_trends.svg")

# OUTPUTS =====

  df_prepp %>%
    filter(fiscal_year == 2023) %>%
    ggplot(aes(x = funding_agency)) +
    geom_col(aes(y = targets), fill = grey20k, position = position_nudge(x = 0.1), width = 0.5) +
    geom_col(aes(y = cumulative), fill = hw_hunter, width = 0.5, alpha = 0.95) +
    geom_text(aes(y = cumulative, label = percent(achv, 1)), hjust = 1) +
    geom_text(aes(y = cumulative, label = comma(cumulative)), hjust = -0.5) +
    coord_flip() +
    si_style_xgrid() +
    labs(x = NULL, y = NULL, title = "CDC & USAID FY23 PrEP_NEW RESULTS",
         caption = metadata$caption) +
    scale_y_continuous(labels = comma)
  si_preview()
