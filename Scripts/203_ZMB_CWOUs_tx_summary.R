# PROJECT:  C:/Users/tessam/Documents/Github/AmboTasker
# PURPOSE:  Call with OUs review
# AUTHOR:   T. Essam | USAID
# REF ID:   2a6548dc
# LICENSE:  MIT
# DATE:     2024-05-29
# UPDATED:

# DEPENDENCIES ------------------------------------------------------------

  #general
  library(tidyverse)
  library(glue)
  #oha
  library(gagglr) ##install.packages('gagglr', repos = c('https://usaid-oha-si.r-universe.dev', 'https://cloud.r-project.org'))
  #viz extensions
  library(scales, warn.conflicts = FALSE)
  library(systemfonts)
  library(tidytext)
  library(patchwork)
  library(ggtext)


# GLOBAL VARIABLES --------------------------------------------------------

  ref_id <- "2a6548dc"  #a reference to be places in viz captions

  path_msd <- si_path() %>% return_latest("Site_IM.*Zambia")

  metadata <- get_metadata(path_msd)  #extract MSD metadata


# IMPORT ------------------------------------------------------------------

  df_msd <- read_psd(path_msd)


# MUNGE -------------------------------------------------------------------

 df_tx <-  df_msd %>%
    filter(indicator %in% c("TX_NEW", "TX_CURR", "TX_NET_NEW"),
           standardizeddisaggregate == "Total Numerator",
           funding_agency == "USAID") %>%
    group_by(fiscal_year, indicator) %>%
    summarize(across(matches("qtr"), \(x) sum(x, na.rm = T))) %>%
    reshape_msd() %>%
    mutate(fill_color = case_when(
      indicator == "TX_CURR" ~ hw_viking,
      indicator == "TX_NEW" ~ hw_lavender_haze,
      TRUE ~ hw_hunter
    ))

  a <-  df_tx %>%
    mutate(indic_order = fct_relevel(indicator, c("TX_NET_NEW", "TX_NEW", "TX_CURR"))) %>%
    ggplot(aes(x = period, y = value)) +
    geom_col(width = 0.5, aes(fill = fill_color)) +
    geom_text(aes(label = comma(value)),
              family = "Source Sans Pro",
              size = 9/.pt,
              color = grey90k, vjust = -0.5) +
    facet_wrap(~indic_order, nrow = 3, scales = "free_y") +
    si_style_ygrid() +
    scale_fill_identity() +
    scale_y_continuous(labels = comma, position = "right") +
    labs(x = NULL, y = NULL)
  si_save("Images/ZMB_fy24q2_tx_summary.svg", scale = 1.3, width = 5.75, height = 3.1)

# IIT VIZ ---------------------------------------------------------------------

  # Not reproducing IIT from Panorama
  df_iit <- df_msd %>%
    filter(funding_agency == "USAID",
           indicator %in% c("TX_CURR",
                            "TX_ML_IIT_less_three_mo",
                            "TX_ML_IIT_six_more_mo",
                            "TX_ML_IIT_three_five_mo",
                            "TX_NET_NEW", "TX_NEW", "TX_RTT"),
           use_for_age == "Y") %>%
           #safe_for_net_new == "Y") %>%
    mutate(indicator = case_when(
      str_detect(indicator, "TX_ML_IIT*") ~ "TX_ML_IIT",
      TRUE ~ indicator
    )) %>%
    group_by(fiscal_year, indicator) %>%
    summarise(across(starts_with("qtr"), \(x) sum(x, na.rm = TRUE)), .groups = "drop") %>%
    reshape_msd(include_type = FALSE) %>%
    pivot_wider(names_from = "indicator",
                names_glue = "{tolower(indicator)}") %>%
    arrange(period) %>%
    mutate(tx_curr_lag1 = lag(tx_curr, n = 1)) %>%
    ungroup() %>%
    rowwise() %>%
    mutate(iit = tx_ml_iit / sum(tx_curr_lag1, tx_new, na.rm = TRUE)) %>%
    ungroup()


  df_iit_site <- df_msd %>%
    filter(funding_agency == "USAID",
           indicator %in% c("TX_CURR",
                            "TX_ML_IIT_less_three_mo",
                            "TX_ML_IIT_six_more_mo",
                            "TX_ML_IIT_three_five_mo",
                            "TX_NET_NEW", "TX_NEW", "TX_RTT"),
           use_for_age == "Y") %>%
    #safe_for_net_new == "Y") %>%
    mutate(indicator = case_when(
      str_detect(indicator, "TX_ML_IIT*") ~ "TX_ML_IIT",
      TRUE ~ indicator
    )) %>%
    group_by(fiscal_year, trendscoarse, facility, facilityuid, indicator) %>%
    summarise(across(starts_with("qtr"), sum, na.rm = TRUE), .groups = "drop") %>%
    reshape_msd(include_type = FALSE) %>%
    pivot_wider(names_from = "indicator",
                names_glue = "{tolower(indicator)}") %>%
    arrange(period) %>%
    group_by(trendscoarse, facilityuid) %>%
    mutate(tx_curr_lag1 = lag(tx_curr, n = 1)) %>%
    ungroup()%>%
    rowwise() %>%
    mutate(iit = tx_ml_iit / sum(tx_curr_lag1, tx_new, na.rm = TRUE)) %>%
    ungroup()


 b <-  df_iit_site %>%
    ggplot(aes(period, iit, size = tx_curr_lag1/100)) +
    geom_point(position = position_jitter(width = .25, seed = 42),
               na.rm = TRUE, color = hw_viking,
               alpha = .16) +
    # ggbeeswarm::geom_quasirandom(
    #   color = hw_viking, alpha = 0.1,
    #   width = .4
    # ) +
    geom_line(data = df_iit %>% filter(period != min(period)),
              aes(group = 1),
              size = 1.5,
              color = hw_sun_kissed) +
    geom_point(data = df_iit %>% filter(period != min(period)),
              size = 8,
              color = hw_sun_kissed) +
    geom_text(data = df_iit %>% filter(period != min(period)),
              aes(label = percent(iit)),
              family = "Source Sans Pro Bold",
              size = 10/.pt,
              color = grey90k) +
    scale_y_continuous(limits = c(0,.15),
                       breaks = c(0, .05, .1, .15),
                       label = percent_format(1),
                       oob = oob_squish, position = "right")  +
    facet_wrap(~"IIT") +
    si_style_ygrid() +
    labs(x = NULL, y = NULL) +
    theme(legend.position = "none",
          axis.text.x = element_blank())


  b / a +
    plot_layout(heights= c(1,6))

    si_save("Images/ZMB_FY24Q2_TX_CURR_summary.svg", scale = 1.3, width = 6.66, height = 4.55)

