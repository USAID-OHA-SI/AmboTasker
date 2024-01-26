# PROJECT: PrEP_NEW Graph
# PURPOSE: Munge and Analysis of
# AUTHOR:  Tim Esssam | SI
# REF ID:  3881a034
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
      pattern = "PSNU_IM.*Zambia")

  # Grab metadata
   get_metadata(file_path)

  # REF ID for plots
    ref_id <- "3881a034"

  # Functions


# LOAD DATA ============================================================================

  msd <- read_psd(file_path) %>%
      filter(indicator == "PrEP_NEW",
             standardizeddisaggregate == "Total Numerator") %>%
      clean_agency()

# MUNGE ============================================================================

  df_prep <-
      msd %>%
      group_by(funding_agency, fiscal_year, indicator) %>%
      summarize(across(.cols = c(targets, cumulative), \(x) sum(x, na.rm = T))) %>%
      mutate(achv = cumulative / targets) %>%
      filter(funding_agency %in% c("CDC", "USAID"))

# VIZ ============================================================================

  df_prep %>%
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

    si_save("Graphics/PrEP_NEW_by_agency_fy23.svg", height = 5, width = 6 )
# SPINDOWN ============================================================================

