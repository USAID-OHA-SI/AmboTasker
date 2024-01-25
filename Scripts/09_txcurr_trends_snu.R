# PROJECT:  AmboTasker
# AUTHOR:   A.Chafetz | USAID
# PURPOSE:  TX_CURR targets / results across time by SNU1
# REF ID:   71d5580e
# LICENSE:  MIT
# DATE:     2024-01-25
# UPDATED:

# DEPENDENCIES ------------------------------------------------------------

  library(tidyverse)
  library(gagglr)
  library(glue)
  library(scales)
  library(systemfonts)
  library(tidytext)
  library(patchwork)
  library(ggtext)


# GLOBAL VARIABLES --------------------------------------------------------

  ref_id <- "71d5580e"

  get_metadata(type = "PSNU_IM.*Zambia")

# IMPORT ------------------------------------------------------------------

  df_zmb <- si_path() %>%
    return_latest("PSNU_IM.*Zambia") %>%
    read_psd()

  df_zmb_hist <- si_path() %>%
    return_latest("PSNU_IM_FY15") %>%
    read_psd() %>%
    filter(country == "Zambia")

# MUNGE -------------------------------------------------------------------

  #aggregate TX total by snu1
  df_tx <- df_zmb %>%
    bind_rows(df_zmb_hist) %>%
    filter(indicator == "TX_CURR",
           standardizeddisaggregate == "Total Numerator",
           fiscal_year <= metadata$curr_fy,
           snu1 != "_Military Zambia") %>%
    group_by(snu1, #funding_agency,
             fiscal_year, indicator) %>%
    summarise(across(c(cumulative, targets), \(x) sum(x, na.rm = TRUE)),
              .groups = "drop")

  #apply agency breakdown
  df_tx <- df_tx %>%
    mutate(snu_agency = ifelse(str_detect(snu1, "South|East|Lusa|^Western"), "CDC", "USAID"),
           .after = snu1)

  #add achievement and growht
  df_viz <- df_tx %>%
    adorn_achievement() %>%
    group_by(snu1) %>%
    mutate(comp_year = case_when(fiscal_year == 2019 ~ cumulative),
           tx_growth = (cumulative/lag(cumulative, order_by = fiscal_year))-1,
           target_growth = (targets/lag(targets, order_by = fiscal_year))-1) %>%
    fill(comp_year) %>%
    mutate(tx_growth_alt = (cumulative/comp_year)) %>%
    ungroup() %>%
    mutate(fill_color = ifelse(snu_agency == "USAID", hw_midnight_blue, hw_lavender_haze),
           fill_growth = ifelse(tx_growth < 0, hw_tango, hw_hunter),
           y_achv= 3.5e5)

# VIZ ---------------------------------------------------------------------

  v1 <- df_viz %>%
    filter(fiscal_year >= metadata$curr_fy -4) %>%
    ggplot(aes(fiscal_year, targets)) +
    geom_col(fill = "#626672", alpha = .7) +
    geom_col(aes(y = cumulative, fill = fill_color)) +
    geom_errorbar(aes(ymin = targets, ymax = targets), color = "#626672",  alpha = .7) +
    geom_point(aes(y = y_achv, color = achv_color)) +
    facet_grid(~fct_reorder2(snu1, fiscal_year, targets)) +
    scale_y_continuous(labels = label_number(scale_cut = cut_short_scale()),
                       expand = c(.005, .005)) +
    scale_fill_identity() +
    scale_color_identity() +
    labs(x = NULL, y = NULL,
         caption = metadata$caption) +
    si_style_ygrid() +
    theme(panel.spacing.x = unit(.5, "lines"))

    v2 <- df_viz %>%
      filter(fiscal_year >= metadata$curr_fy -4) %>%
      ggplot(aes(fiscal_year, tx_growth, fill = fill_growth)) +
      geom_col(na.rm = TRUE) +
      facet_grid(~fct_reorder2(snu1, fiscal_year, targets)) +
      scale_y_continuous(labels = label_percent()) +
      scale_fill_identity() +
      labs(x = NULL, y = NULL,
           caption = metadata$caption) +
      si_style_ygrid() +
      theme(panel.spacing.x = unit(.5, "lines"))

    v1 / v2 +
      plot_layout(heights = c(4, 1))

    df_viz %>%
      filter(fiscal_year >= metadata$curr_fy -4) %>%
      ggplot(aes(fiscal_year, tx_growth_alt)) +
      geom_line(na.rm = TRUE) +
      facet_grid(~fct_reorder2(snu1, fiscal_year, targets)) +
      # scale_y_continuous(labels = label_percent()) +
      labs(x = NULL, y = NULL,
           caption = metadata$caption) +
      si_style_ygrid()

