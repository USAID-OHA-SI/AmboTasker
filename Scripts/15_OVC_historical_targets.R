# PROJECT: Analysis of Historical OVC Targets
# PURPOSE: Munge and Analysis of OVC Targets
# AUTHOR:  Tim Esssam | SI
# REF ID:  1f087c9b
# LICENSE: MIT
# DATE:   2024-02-20
# NOTES:

# LOCALS & SETUP ============================================================================

  # Librari es
    library(gagglr)
    library(tidyverse)
    library(scales)
    library(glue)
    library(ggtext)


  # SI specific paths/functions
    load_secrets()
    merdata <- file.path(glamr::si_path("path_msd"))
    file_path <- return_latest(folderpath = merdata,
      pattern = "OU_IM_FY15-20")

    file_path2 <- return_latest(folderpath = merdata,
                                pattern = "OU_IM_F")

  # Grab metadata
   get_metadata(file_path)

  # REF ID for plots
    ref_id <- "1f087c9b"

  # Functions


# LOAD DATA ============================================================================

  df_msd <- read_msd(file_path) %>% filter(operatingunit == "Zambia")
  df_msd2 <- read_msd(file_path2) %>% filter(operatingunit == "Zambia")

# FOCUS ON OVC TARGETS BY AGENCY========================================================

  df_ovc <- df_msd %>%
    bind_rows(df_msd2) %>%
      filter(indicator == "OVC_SERV",
             standardizeddisaggregate == "Total Numerator",
             operatingunit == "Zambia") %>%
      clean_agency() %>%
      group_by(fiscal_year, funding_agency) %>%
      summarise(targets = sum(targets, na.rm = T), .groups = "drop") %>%
    mutate(agency = fct_relevel(funding_agency,
                                c("USAID", "CDC", "PC",
                                  "DOD", "STATE", "DEDUP")))

  df_ovc %>%
      ggplot(aes(y = targets, x = factor(fiscal_year))) +
      geom_col(fill = hw_tango, width = 0.75) +
      facet_wrap(~agency, scales = "free_y") +
    si_style_ygrid() +
    scale_y_continuous(labels = comma) +
    labs(title = "OVC TARGETS BY AGENCY 2016 - 2024",
         caption = metadata$caption,
         x = NULL, y = NULL)

# DISAGGS ============================================================================

df_ovc_disag <- df_msd %>%
    bind_rows(df_msd2) %>%
    filter(indicator == "OVC_SERV",
           str_detect(standardizeddisaggregate, "Age/Sex"),
           operatingunit == "Zambia") %>%
    clean_agency() %>%
    group_by(fiscal_year, funding_agency, standardizeddisaggregate) %>%
    summarise(targets = sum(targets, na.rm = T), .groups = "drop") %>%
    mutate(agency = fct_relevel(funding_agency,
                                c("USAID", "CDC", "PC",
                                  "DOD", "STATE", "DEDUP")))

  df_ovc_disag %>%
    filter(fiscal_year > 2019) %>%
    mutate(total = sum(targets), .by = c(funding_agency, fiscal_year)) %>%
    mutate(share = targets / total) %>%
    filter(targets > 0) %>%
    mutate(disag_color = case_when(
      str_detect(standardizeddisaggregate, "DREAMS") ~ hw_lavender_haze,
      str_detect(standardizeddisaggregate, "Prevent") ~ hw_viking,
      str_detect(standardizeddisaggregate, "ProgramStatus$") ~ hw_tango,
      str_detect(standardizeddisaggregate, "Caregiver") ~ hw_hunter,
    )) %>%
    ggplot(aes(y = targets, x = factor(fiscal_year))) +
    geom_col(aes(fill = disag_color), width = 0.75) +
    geom_text(aes(label = percent(share, 1), group = standardizeddisaggregate),
              position = position_stack(vjust = .5),
              size = 10/.pt,
              family = "Source Sans Pro",
              color = grey90k) +
    facet_wrap(~agency, scales = "free_y") +
    si_style_ygrid() +
    scale_y_continuous(labels = comma) +
    labs(title = "OVC TARGETS BY AGENCY 2020 - 2024",
         caption = metadata$caption,
         x = NULL, y = NULL) +
    scale_fill_identity()

  # Can also flip these around to facet across disags
  df_ovc_disag %>%
    filter(fiscal_year > 2019) %>%
    mutate(total = sum(targets), .by = c(funding_agency, fiscal_year)) %>%
    mutate(share = targets / total) %>%
    filter(targets > 0) %>%
    mutate(disag_color = case_when(
      str_detect(standardizeddisaggregate, "DREAMS") ~ hw_lavender_haze,
      str_detect(standardizeddisaggregate, "Prevent") ~ hw_viking,
      str_detect(standardizeddisaggregate, "ProgramStatus$") ~ hw_tango,
      str_detect(standardizeddisaggregate, "Caregiver") ~ hw_hunter,
    )) %>%
    # count(standardizeddisaggregate, disag_color) %>%
    ggplot(aes(y = targets, x = factor(fiscal_year))) +
    geom_col(aes(fill = disag_color), width = 0.75) +
    facet_wrap(~standardizeddisaggregate) +
    si_style_ygrid() +
    scale_y_continuous(labels = comma) +
    labs(title = "OVC TARGETS BY AGENCY 2020 - 2024",
         caption = metadata$caption,
         x = NULL, y = NULL) +
    scale_fill_identity()

# SPINDOWN ============================================================================

