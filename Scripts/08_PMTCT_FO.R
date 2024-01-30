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

  df_pmtct_fo <- bind_rows(df_pmtct, df_pmtct_old)

# MUNGE ROUND 1 - Fo FINAL STATUS / DIED
  num_pmtct <- df_pmtct_fo %>%
    filter(
      indicator %in% c("PMTCT_FO"),
      standardizeddisaggregate %in% c(
        "Outcome"),
      otherdisaggregate %in% c(
        "HIV-final status unknown",
        "Other Outcomes: Died"),
      fiscal_year %in% c(2017:2023)) %>%
    group_by(fiscal_year, standardizeddisaggregate, otherdisaggregate, snu1) %>%  #include operatingunit
    summarise(across(c("cumulative"), \(x) sum(x, na.rm = T)),
              .groups = "drop") %>%
    pivot_wider(
      names_from = otherdisaggregate,
      names_glue = "{tolower(otherdisaggregate)}",
      values_from = cumulative
    ) %>%
    mutate_all(~replace_na(., 0)) %>% #replace NA's with zero
    rowwise() %>%
    mutate(
      num_sum = `hiv-final status unknown` + `other outcomes: died`)


  num2_pmtct <- df_pmtct_fo %>%
    filter(
      indicator %in% c("PMTCT_FO"),
      standardizeddisaggregate %in% c(
        "Outcome"),
      otherdisaggregate %in% c(
        #"HIV-final status unknown",
        "Other Outcomes: Died"),
      fiscal_year %in% c(2017:2023)) %>%
    group_by(fiscal_year, standardizeddisaggregate, otherdisaggregate, snu1) %>%  #include operatingunit
    summarise(across(c("cumulative"), \(x) sum(x, na.rm = T)),
              .groups = "drop") %>%
    pivot_wider(
      names_from = otherdisaggregate,
      names_glue = "{tolower(otherdisaggregate)}",
      values_from = cumulative
    ) %>%
    rowwise() %>%
    mutate(
      num_sum = `other outcomes: died`)


  denom_pmtct <- df_pmtct_fo %>%
    filter(
      indicator %in% c("PMTCT_FO"),
      standardizeddisaggregate %in% c(
        "Total Numerator"),
      fiscal_year %in% c(2017:2023)) %>%
    group_by(fiscal_year, standardizeddisaggregate, snu1) %>% #include operating unit
    summarise(across(c("cumulative"), \(x) sum(x, na.rm = T)),
              .groups = "drop")

  #join Viz 1 (Uknown + Died Otherdisaggs as Numerator)
  merged_df <- left_join(num_pmtct, denom_pmtct %>% select(fiscal_year, cumulative, snu1),
                         by = c("fiscal_year", "snu1")) %>% #select(-standardizeddisaggregate) %>%
    mutate(percent_fo = num_sum/cumulative)


  #join Viz 2 (Died Otherdisaggs as Numerator)
  merged_df2 <- left_join(num2_pmtct, denom_pmtct %>% select(fiscal_year, snu1, cumulative),
                          by = c("fiscal_year", "snu1")) %>%
    mutate(percent_fo = num_sum/cumulative)

# VIZ ============================================================================

  df_viz <-
    merged_df %>%
    mutate(sort_var = case_when(
      fiscal_year == metadata$curr_fy ~ percent_fo,
      TRUE ~ NA_real_
      )
    ) %>%
    group_by(snu1) %>%
    fill(sort_var, .direction = "updown") %>%
    ungroup() %>%
    mutate(snu1 = str_remove_all(snu1, " Province")) %>%
    mutate(snu1_order = fct_reorder(snu1, sort_var, .desc = T)) %>%
    filter(str_detect(snu1, "Military", negate = T))

  df_viz %>%
    ggplot(aes(x = fiscal_year,
               y = percent_fo,
               group = snu1_order
    )) +
    geom_area(fill = hw_hunter , alpha = 0.15) +
    geom_line(color = hw_hunter ,linewidth = 1) +
    geom_point(aes(y = percent_fo), fill = hw_hunter, size = 2.5, shape = 21, color = "white") +
    si_style_ygrid(facet_space = 0.5) +
    geom_text(data = df_viz %>% group_by(snu1_order) %>%
                filter(percent_fo %in% c(min(percent_fo),max(percent_fo))),
              aes(label = percent(percent_fo, 1)),
              size = 8/.pt,
              vjust = -0.25,
              hjust = -.25
    ) +
    facet_wrap(~snu1_order, nrow = 2) +
    scale_y_continuous(labels = scales::percent, lim = c(0, 1)) +
    scale_x_continuous(expand = c(0,1.1), #add buffer room on right
                       breaks = c(2017, 2019, 2021, 2023),
                       label = c(paste(rep("FY", 4), 17:19, sep = ""))) +
    #coord_cartesian(expand = TRUE) +
    theme(plot.subtitle = ggtext::element_markdown()) +
    labs(x = NULL, y = NULL,
         subtitle = glue::glue("<span style = 'color:#419164'>Percent Infants with reported Final Outcome who have an Unknown Status and/or Died </span>"),
         caption = "Source: FY23Q4c MSD & FY15-20 MSD",
         title = "USAID PMTCT_FO TRENDS FY17 - FY23")
  si_preview()

  si_save("Graphics/ZMB_PMCTC_FO.svg")

# DIFFERENT KNOWN STATUS REQUEST ============================================================================

  df_pmtct_fo %>%
    filter(standardizeddisaggregate == "Outcome") %>%
    count(fiscal_year, otherdisaggregate) %>%
    spread(fiscal_year, n)



  num_pmtct_tk2 <- df_pmtct_fo %>%
    filter(
      indicator %in% c("PMTCT_FO"),
      standardizeddisaggregate %in% c(
        "Outcome"),
      otherdisaggregate %in% c(
        "HIV-uninfected",
        "HIV-infected"),
      fiscal_year %in% c(2017:2023)) %>%
    group_by(fiscal_year, standardizeddisaggregate, otherdisaggregate, snu1) %>%  #include operatingunit
    summarise(across(c("cumulative"), \(x) sum(x, na.rm = T)),
              .groups = "drop") %>%
    pivot_wider(
      names_from = otherdisaggregate,
      names_glue = "{tolower(otherdisaggregate)}",
      values_from = cumulative
    ) %>%
    mutate_all(~replace_na(., 0)) %>% #replace NA's with zero
    rowwise() %>%
    mutate(
      num_sum = `hiv-infected` + `hiv-uninfected`)

  #denominator
  denom_pmtct_tk2 <- df_pmtct_fo %>%
    filter(standardizeddisaggregate %in% c(
        "Total Numerator"),
      fiscal_year %in% c(2017:2023)) %>%
    group_by(fiscal_year, standardizeddisaggregate, snu1) %>% #include operating unit
    summarise(across(c("cumulative"), sum, na.rm = TRUE),
              .groups = "drop")

  merged_df_tk2 <- left_join(num_pmtct_tk2, denom_pmtct_tk2 %>% select(fiscal_year, snu1, cumulative),
                         by = c("fiscal_year", "snu1")) %>%
    mutate(percent_fo = num_sum/cumulative)

  # Ask:  proportion of infants with known status in PMTCT_FO = (sum of HIV-infected plus HIV uninfected) / PMTCT_FO total N.
  # Great if you can also figure out how to add a bar for each FY data point showing PMTCT_FO total N.

  # First, let's show the total N (cumulative)
  # Asked to order by largest changes
  df_viz2 <-  merged_df_tk2 %>%
    mutate(sort_var = case_when(
      fiscal_year == metadata$curr_fy ~ percent_fo,
      TRUE ~ NA_real_
      )
    ) %>%
    group_by(snu1) %>%
    fill(sort_var, .direction = "updown") %>%
    ungroup() %>%
    mutate(snu1_order = fct_reorder(snu1, sort_var, .desc = T))

  df_viz2 %>%
    ggplot(aes(x = fiscal_year)) +
    geom_col(aes(y = cumulative, fill = hw_hunter), alpha = 0.75, width = 0.5) +
    facet_wrap(~snu1_order, nrow = 1)

  df_viz2 <-
    df_viz2 %>%
    mutate(snu1 = str_remove_all(snu1, " Province"),
           snu1_agency = case_when(
             str_detect(snu1, "West|South|East|Lusaka") ~ "CDC",
             TRUE ~ "USAID",
           )) %>%
    group_by(snu1) %>%
    mutate(percent_fo_min = percent_fo[which.min(fiscal_year)],
           percent_fo_max = percent_fo[which.max(fiscal_year)],
           fo_diff = percent_fo_max - percent_fo_min) %>%
    ungroup() %>%
    mutate(snu1_order = fct_relevel(snu1, c("NorthWestern", "Central", "Muchinga", "Copperbelt",
                                            "Northern", "Luapula",
                                            "Eastern", "Western", "Lusaka", "Southern"))) %>%
    filter(str_detect(snu1, "Mil", negate = T))



  # Define the central color
  fill_color <- c("#A8DFC0","#93D4AF","#7EC99F","#69BE8E","#54B37E","#3FA86D","#2A9D5C","#15924C","#00873B")

  # Requested SNU1 order
  # USAID highest achievers, then CDC


  df_viz2 %>%
    ggplot(aes(x = fiscal_year,
               y = percent_fo,
               group = snu1_order
    )) +
    geom_area(fill = hw_hunter , alpha = 0.15) +
    geom_line(color = hw_hunter ,linewidth = 1) +
    geom_point(aes(y = -0.1, fill = cumulative), size = 5, shape = 21, color = grey90k) +
    geom_point(aes(y = percent_fo), fill = hw_hunter, size = 2.5, shape = 21, color = "white") +
    si_style_ygrid(facet_space = 0.5) +
    geom_text(data = df_viz2 %>% group_by(snu1_order) %>%
                filter(percent_fo %in% c(min(percent_fo),max(percent_fo))),
              aes(label = percent(percent_fo, 1)),
              size = 8/.pt,
              vjust = -0.25,
              hjust = -.25
    ) +
    facet_wrap(~snu1_order, nrow = 2) +
    scale_y_continuous(labels = scales::percent, lim = c(-.1, 1)) +
    scale_x_continuous(expand = c(0,1.1), #add buffer room on right
                       breaks = c(2017, 2019, 2021, 2023),
                       label = c(paste(rep("FY", 4), 17:19, sep = "")))  +
    scale_fill_viridis_c(direction = -1) +

    #coord_cartesian(expand = TRUE) +
    theme(plot.subtitle = ggtext::element_markdown()) +
    labs(x = NULL, y = NULL,
         subtitle = glue::glue("<span style = 'color:#419164'>Percent Infants with reported Final Outcome (FO = HIV-infected + HIV uninfected / PMTCT_FO numerator)) </span>"),
         caption = "Source: FY23Q4c MSD & FY15-20 MSD",
         title = "USAID PMTCT_FO TRENDS FY17 - FY23",
         fill = "Total Number of Children with a Documented Outcome") +
      si_legend_fill()

  si_save("Graphics/ZMB_PMTCT_FO_infect.svg")
  si_preview()


