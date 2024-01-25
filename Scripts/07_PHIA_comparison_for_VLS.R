# PROJECT: Delta in Zamphia VLS across time
# PURPOSE: Munge and Analysis of PHIA data
# AUTHOR:  Tim Esssam | SI
# REF ID:  990c3d20
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
    library(googlesheets4)


  # SI specific paths/functions
    load_secrets()
    gs_id <- "1pT53ohURDYrawWeC0rJ1omfd2_awMp-F1q0kxZlaHsY"

  # REF ID for plots
    ref_id <- "990c3d20"

  # Functions


# LOAD DATA ============================================================================

  df_phia <- read_sheet(ss = gs_id, sheet = "PHIA_GEO")

# MUNGE ============================================================================

  df_vls <- df_phia %>%
      filter(indicator == "VLS") %>%
      mutate(tag = ifelse(str_detect(snu1, "South|East|Lusa|^Western"), "CDC", "USAID")) %>%
      arrange(snu1, year) %>%
      group_by(snu1) %>%
      mutate(vls_gain = est - lag(est, n = 1),
             y_start = lag(est),
             y_end = lead(est)) %>%
      ungroup() %>%
      mutate(snu1 = fct_reorder2(snu1, year, -est)) %>%
      group_by(snu1) %>%
      fill(y_start, .direction = "up") %>%
      fill(y_end, .direction = "down")


# VIZ ============================================================================

  df_vls %>%
      ggplot(aes(y = est/100, x = snu1)) +
      geom_hline(aes(yintercept = .95^3), color = grey10k, linetype = "dashed") +
      geom_segment(aes(y = lb/100, yend = ub/100, x = snu1, xend = snu1),
                   color = grey10k, size = 4, alpha = 0.5) +
      geom_segment(aes(y = y_start/100, yend = y_end/100, x = snu1, xend = snu1), size = 1,
                   color = grey30k) +
      geom_point(size = 3, shape = 16, aes(color = factor(year), group = year)) +
      geom_point(size = 3, shape = 1, aes(group = year)) +
      geom_text(aes(label = round(vls_gain, 0)), size = 8/.pt, vjust = -1) +
      si_style_ygrid(facet_space = 0.75) +
      scale_y_continuous(labels = percent, lim = c(0.3, 1)) +
      scale_color_manual(values = c("2016" = hw_electric_indigo, "2021" = hw_orchid_bloom)) +
      facet_wrap(~tag, nrow = 1, scales = "free_x") +
      labs(title = "COMPARISON OF VIRAL LOAD SUPPRESSION FROM 2016 - 2021 ZAMPHIAs",
           caption = "Source: 2016 & 2021 ZAMPHIA",
           x = NULL, y = NULL)
    si_save("Graphics/Zamphia_VLS_comparison_across_time.svg")

# SPINDOWN ============================================================================

