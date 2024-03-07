# PROJECT: Zambia EIP curves
# PURPOSE: Munge and Analysis of
# AUTHOR:  Tim Esssam | SI
# REF ID:  6d6f5120
# LICENSE: MIT
# DATE:   2024-03-07
# NOTES:

# LOCALS & SETUP ============================================================================

  # Libraries
    library(gagglr)
    library(tidyverse)
    library(scales)
    library(glue)
    library(ggtext)
    library(mindthegap)


  # SI specific paths/functions
    load_secrets()

  # REF ID for plots
    ref_id <- "6d6f5120"

  # Functions


# LOAD DATA ============================================================================

   p <-  epi_plot(sel_cntry = "Eswatini")


    df_epi <- mindthegap::pull_unaids(data_type = "HIV Estimates", pepfar_only = TRUE)

    df_epi_pepfar <- df_epi %>%
      dplyr::filter(age == "All", sex == "All",
                    indicator %in% c("Total deaths to HIV Population", "Number New HIV Infections"),
                    country %in% c("Eswatini", "Zambia")) %>% #grab indicators
      dplyr::select(year, country,indicator, estimate) %>%
      dplyr::arrange(country, indicator, year) #order rows by these variables

    df_epi_pepfar <- df_epi_pepfar %>%
      tidyr::pivot_wider(names_from = "indicator",
                         values_from = "estimate", #pivots data wide into infections column
                         names_glue = "{indicator %>% stringr::str_extract_all('deaths|Infections') %>% tolower}")


    df_epi_pepfar <-
      df_epi_pepfar %>%
      dplyr::mutate(declining_deaths = deaths - dplyr::lag(deaths, order_by = year) <= 0, by = c(country)) %>% #TRUE/FALSE declining
      dplyr::mutate(infections_below_deaths = infections < deaths,
                    ratio = infections / deaths,
                    direction_streak = sequence(rle(declining_deaths)$lengths),
                    epi_control = declining_deaths == TRUE & infections_below_deaths == TRUE)

    #Add colors to indicators and flip axis
    df_epi_pepfar <- df_epi_pepfar %>%
      tidyr::pivot_longer(c(infections, deaths), names_to = "indicator") %>% #put back indicators in column
      dplyr::arrange(country, indicator, year) %>%
      dplyr::mutate(val_mod = ifelse(indicator == "deaths", -value, value), #create dual-axis
                    fill_color = ifelse(indicator == "deaths", glitr::old_rose, glitr::denim))


    #COUNTRY
    df_viz_cntry <- df_epi_pepfar %>%
      dplyr::filter(country %in% "Eswatini") %>%
      dplyr::mutate(val_lab = dplyr::case_when(year == max(year) ~
                                                 scales::number(value, 1, scale = 1e-3, suffix = "k")), #standardize number format
                    max_plot_pt = max(value),
                    min_plot_pt = min(val_mod),
                    lab_pt = dplyr::case_when(year == max(year) ~ val_mod),
                    indicator = ifelse(indicator == "deaths", "Total Deaths to HIV Population", "New HIV Infections"), #creating labels
                    new_hiv_label = dplyr::case_when(value == max_plot_pt ~ indicator),  #assigning label location to min/max
                    tot_death_label = dplyr::case_when(val_mod == min_plot_pt ~ indicator)) %>%
      dplyr::mutate(cntry_order = max(value, na.rm = T), .by = country) %>%
      dplyr::mutate(country = forcats::fct_reorder(country, cntry_order, .desc = T))

    df_viz_zmb <- df_epi_pepfar %>%
      dplyr::filter(country %in% "Zambia") %>%
      dplyr::mutate(val_lab = dplyr::case_when(year == max(year) ~
                                                 scales::number(value, 1, scale = 1e-3, suffix = "k")), #standardize number format
                    max_plot_pt = max(value),
                    min_plot_pt = min(val_mod),
                    lab_pt = dplyr::case_when(year == max(year) ~ val_mod),
                    indicator = ifelse(indicator == "deaths", "Total Deaths to HIV Population", "New HIV Infections"), #creating labels
                    new_hiv_label = dplyr::case_when(value == max_plot_pt ~ indicator),  #assigning label location to min/max
                    tot_death_label = dplyr::case_when(val_mod == min_plot_pt ~ indicator)) %>%
      dplyr::mutate(cntry_order = max(value, na.rm = T), .by = country) %>%
      dplyr::mutate(country = forcats::fct_reorder(country, cntry_order, .desc = T))




# MUNGE ============================================================================

  p + labs(title = "SINCE 2010 IN ZAMBIA, NEW HIV INFECTIONS HAVE DROPPED BY 53%")
  si_save("Images/epi_plot.png")


# VIZ ============================================================================

  df_viz_cntry %>%
    ggplot(aes(year, value, group = indicator, fill = fill_color, color = fill_color)) +
    geom_area(position = "identity") +
    geom_point(aes(y = lab_pt), na.rm = TRUE, shape = 21, color = "white", size = 3) +
    geom_text(aes(label = val_lab), na.rm = TRUE,
              hjust = -0.3,
              family = "Source Sans Pro Light") + #value label text
    scale_y_continuous(labels = ~ (scales::label_number(scale_cut = scales::cut_short_scale())(abs(.))),
                       expand = c(0, 0), limits = c(0, 25000)) +
    scale_x_continuous(breaks = seq(min(df_epi_pepfar$year), max(df_epi_pepfar$year),5)) + #automatic x-axis min/max +
    scale_fill_identity(aesthetics = c("fill", "color")) +
    labs(x = NULL, y = NULL) + coord_cartesian(expand = T, clip = "off") +
    glitr::si_style_ygrid(facet_space = 0.75) + #adjusted y-axis grid spacing
    theme(axis.text.y = ggtext::element_markdown()) +
    labs(caption = "Source: UNAIDS Data 2022 Release",
         title = "ESWATINI REACHED EPIDEMIC CONTROL IN 2021") +
    geom_text(aes(label = new_hiv_label, x = 2005, y = (max_plot_pt)), na.rm = TRUE,
              hjust = -0.3, family = "Source Sans Pro Light") +
    geom_text(aes(label = tot_death_label, x = 2005, y = (max_plot_pt)), na.rm = TRUE,
              hjust = -0.3, family = "Source Sans Pro Light")

  si_save("Graphics/Eswatini_epi_control.svg")


# SPINDOWN ============================================================================

