# PROJECT:  AmboTasker
# PURPOSE:  Explore PLHIV SNU1 trends
# AUTHOR:   T. Essam | USAID
# REF ID:   a0964b89
# LICENSE:  MIT
# DATE:     2024-04-05
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
  library(gt)
  library(gtExtras)
  library(rcartocolor)


# GLOBAL VARIABLES --------------------------------------------------------

  ref_id <- "a0964b89"  #a reference to be places in viz captions

  path_msd <-  si_path() %>%
    return_latest("PSNUByIMs-Zambia-Frozen-2024-04-08")

  meta <- get_metadata(path_msd)

# Functions
  calc_tx_curr <- function(.data){
    .data %>%
      filter(indicator == "TX_CURR",
             standardizeddisaggregate == "Total Numerator") %>%
      group_by(fiscal_year, snu1) %>%
      summarize(across(.cols = c("cumulative", "targets"), \(x) sum(x, na.rm = T)), .groups = "drop") %>%
      calc_achievement()
  }

  carto_fill <- rcartocolor::carto_pal(name = "SunsetDark")


  # Darken columns
  drkn_clmn_hdr <- function(.data) {
    .data %>%
      tab_style(
        style = list(
          cell_text(color = grey90k)
        ),
        locations = cells_column_labels()
      )
  }

# IMPORT ------------------------------------------------------------------

  df_genie <- read_psd(path_msd) %>%
    filter(fiscal_year > 2015) %>%
    calc_tx_curr() %>%
    rowwise() %>%
    mutate(max_val = max(cumulative, targets)) %>%
    ungroup() %>%
    mutate(final_val = last(max_val), .by = "snu1") %>%
    mutate(snu1_sort = fct_reorder(snu1, final_val, .desc = T),
           snu1_limits = case_when(
             str_detect(snu1, "Lusaka|Copper|Southern|Central") ~ 4e5,
             TRUE ~ 1.15e5
           ))

# VIZ ---------------------------------------------------------------------

  df_genie %>%
    ggplot(aes(x = factor(fiscal_year))) +
    geom_blank(aes(y = snu1_limits)) +
    #geom_col(aes(y = targets), position = position_nudge(x = 0.1), fill = grey20k, width = 0.5) +
    geom_col(aes(y = targets), fill = hw_orchid_bloom, color = hw_orchid_bloom, width = 0.5) +
    geom_col(aes(y = cumulative), color = "white", fill = "white", width = 0.5, alpha = 0.9) +
    geom_text(aes(y = max_val, label = percent(achievement, 1)),
              size = 8/.pt,
              family = "Source Sans Pro",
              vjust = -0.5) +
    facet_wrap(~snu1_sort, scale = "free_y", axes = "all") +
    si_style_ygrid() +
    scale_y_continuous(label = comma) +
    labs(title = "HISTORICAL TX_CURR ACHIEVEMENT BY PROVINCE", x = NULL, y = NULL,
         caption = glue("{meta$caption} & {meta$caption} | Ref id: {ref_id} | Created by T. Essam")
         )

# Make a table showing the correct percent achievement
  min <- min(df_genie$achievement)
  max <- max(df_genie$achievement)

  df_genie %>%
    select(snu1, achievement, fiscal_year) %>%
    spread(fiscal_year, achievement) %>%
    arrange(desc(`2024`)) %>%
    gt(rowname_col = "snu1") %>%
    fmt_percent(columns = where(is.double), decimals = 0) %>%
    gt_theme_nytimes() %>%
    gt_color_rows(columns = where(is.double), palette = carto_fill, domain = c(min, max)) %>%
    drkn_clmn_hdr() %>%
    tab_header(title = "HISTORICAL TX_CURR ACHEIVEMENT BY PROVINCE") %>%
    tab_source_note(
      source_note = gt::md(glue::glue("{meta$caption} & {meta$caption} | Ref id: {ref_id} | Created by T. Essam"))) %>%
    tab_options(source_notes.font.size = px(10), data_row.padding = px(1))

