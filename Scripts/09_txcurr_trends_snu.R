# PROJECT:  AmboTasker
# AUTHOR:   A.Chafetz | USAID
# PURPOSE:  TX_CURR targets / results across time by SNU1
# REF ID:   71d5580e
# LICENSE:  MIT
# DATE:     2024-01-25
# UPDATED:  2024-01-26

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
    mutate(comp_year = case_when(fiscal_year == 2021 ~ cumulative),
           tx_growth = (cumulative/lag(cumulative, order_by = fiscal_year))-1,
           target_growth = (targets/lag(targets, order_by = fiscal_year))-1) %>%
    fill(comp_year) %>%
    mutate(tx_growth_alt = (cumulative/comp_year)) %>%
    ungroup() %>%
    mutate(snu1 = str_remove(snu1, " Province"),
           fy_trunc = str_sub(fiscal_year, start = 3) %>% paste0("'", .),
           fill_color = ifelse(snu_agency == "USAID", hw_midnight_blue, hw_lavender_haze),
           fill_growth = ifelse(tx_growth < 0, hw_tango, hw_hunter),
           y_achv= 3.5e5)

  #limit years
  df_viz <- df_viz %>%
    filter(fiscal_year >= metadata$curr_fy -4)


# VIZ ---------------------------------------------------------------------

  v1 <- df_viz %>%
    ggplot(aes(fy_trunc, targets)) +
    geom_col(fill = "#626672", alpha = .3) +
    geom_col(aes(y = cumulative), fill = hw_electric_indigo, width = .5) +
    # geom_col(aes(y = cumulative, fill = fill_color), width = .5) +
    geom_point(aes(y = y_achv, color = achv_color)) +
    facet_grid(~fct_reorder2(snu1, fiscal_year, targets)) +
    scale_y_continuous(labels = label_number(scale_cut = cut_short_scale()),
                       expand = c(.005, .005)) +
    scale_fill_identity() +
    scale_color_identity() +
    coord_cartesian(clip = "off") +
    labs(x = NULL, y = "TX_CURR") +
    si_style_ygrid() +
    theme(panel.spacing.x = unit(.5, "lines"),
          strip.text = element_blank())

    v2 <- df_viz %>%
      ggplot(aes(fy_trunc, tx_growth, fill = fill_growth)) +
      geom_col(width = .5, na.rm = TRUE) +
      facet_grid(~fct_reorder2(snu1, fiscal_year, targets)) +
      scale_y_continuous(labels = label_percent(),
                         expand = c(.005, .005)) +
      scale_fill_identity() +
      labs(x = NULL, y = "YoY Growth") +
      si_style_ygrid() +
      theme(panel.spacing.x = unit(.5, "lines"))

    #alt growth
    # df_viz %>%
    #   ggplot(aes(fy_trunc, tx_growth_alt, group = snu1)) +
    #   geom_line(color = hw_orchid_bloom, na.rm = TRUE) +
    #   geom_point(shape = 21,  na.rm = TRUE, stroke = 1,
    #              fill = "white", color = hw_orchid_bloom,
    #              data = df_viz %>% filter(fiscal_year == 2021)) +
    #   geom_point(shape = 21,  na.rm = TRUE, stroke = 1,
    #              fill = hw_orchid_bloom, color = hw_orchid_bloom,
    #              data = df_viz %>% filter(fiscal_year == metadata$curr_fy)) +
    #   facet_grid(~fct_reorder2(snu1, fiscal_year, targets)) +
    #   scale_y_continuous(#labels = label_percent(),
    #                      expand = c(.005, .005)) +
    #   labs(x = NULL, y = "Growth",
    #        #title = "Treatment cohort growth compared to 2021 baseline",
    #        caption = metadata$caption) +
    #   si_style_ygrid() +
    #   theme(panel.spacing.x = unit(.5, "lines"))



    v1 / v2 +
      plot_layout(heights = c(4, 1)) +
      plot_annotation(caption = metadata$caption,
                      title = "ZAMBIA TREATMENT TRENDS FY19-23 BY PROVINCE",
                      subtitle = "Cumulative/Target Achievement + Year Over Year Cumulative Growth",
                      theme = si_style_ygrid())

    si_save("Graphics/tx_trends.svg")




    df_viz %>%
      filter(fiscal_year >= metadata$curr_fy - 2) %>%
      group_by(snu1) %>%
      summarise(avg_tx_growth = mean(tx_growth, na.rm = TRUE),
                .groups = "drop") %>%
      arrange(desc(avg_tx_growth))

    df_viz %>%
      select(snu1, fiscal_year, cumulative) %>%
      group_by(snu1) %>%
      mutate(growth_3yr = (cumulative/lag(cumulative, n = 2, order_by = fiscal_year))-1,
             delta_3yr = cumulative - lag(cumulative, n = 2, order_by = fiscal_year)) %>%
      filter(fiscal_year == metadata$curr_fy) %>%
      arrange(desc(growth_3yr))
