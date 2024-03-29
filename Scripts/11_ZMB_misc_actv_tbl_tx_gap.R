# PROJECT: Zambia Activity Table
# PURPOSE: Munge and Analysis of
# AUTHOR:  Tim Esssam | SI
# REF ID:  d437a548
# LICENSE: MIT
# DATE:   2024-01-28
# NOTES:

# LOCALS & SETUP ============================================================================

  # Libraries
    library(gagglr)
    library(tidyverse)
    library(scales)
    library(glue)
    library(ggtext)
    library(gt)
    library(gtExtras)
    library(googlesheets4)


  # SI specific paths/functions
    load_secrets()
    gs_id <-  "1ExYYMpJ0smR9ok8uBK7Yp7JOaiB_OFqn226DKDC91x0"


  # REF ID for plots
    ref_id <- "d437a548"

  # Functions
    # Darken columns
    drkn_clmn_hdr <- function(.data){
      .data %>%
        tab_style(
          style = list(
            cell_text(color = grey90k)
          ),
          locations = cells_column_labels(
          )
        )
    }


# LOAD DATA ============================================================================

  zmb_act <- read_sheet(gs_id)

# TABLE ============================================================================

  zmb_act %>%
      gt() %>%
      sub_missing(missing_text = "-") %>%
      gt_theme_nytimes() %>%
      drkn_clmn_hdr() %>%
      tab_header(
        title = glue("USAID SUPPORTED PROGRAMMING ACROSS ZAMBIA PROVINCES")) %>%
      gtsave_extra(filename = glue("Images/zmb_activities_summary.png"))

# VIZ ============================================================================

  #

# ZMB _COVERAGE ============================================================================

  gs_id2 <- "1eEnQ6Yr3aA-wvzbDjNCI7riqL9Ze3z68iTMw_Tigr8Y"

  file_cov <- "Data/ZMB_TX_GAP.csv"

  zmb_cov <- read_sheet(gs_id2)

  zmb_cov %>% select(-contains("Coverage")) %>%
    pivot_longer(where(is.double),
                 names_to = "")

  zmb_cov <- read_csv(file_cov) %>%
    janitor::clean_names()

  zmb_cov %>% names()

  zmb_cov %>%
    filter(province != "TOTAL") %>%
    mutate(groupings = case_when(
      str_detect(province, "Lus|Copp|South|Cent|East") ~ "top",
      TRUE ~ "bottom"
    ),
      ymax = ifelse(groupings == "top", 3.5e5, 1e5)) %>%
    mutate(snu1_order = fct_reorder(province, tx_curr, .desc = T)) %>%
    ggplot(aes(x = year)) +
    geom_blank(aes(y = ymax)) +
    geom_col(aes(y = tx_goal), color = hw_hunter, fill = "white", width = 0.5, ) +
    geom_col(aes(y = tx_curr), fill = "#a0c8b1",
             width = 0.5) +
    geom_errorbar(aes(ymin = tx_goal, ymax = tx_goal),
                  size = 0.5, width = 0.5,
                  colour = grey20k) +
    geom_text(aes(y = tx_curr, label = percent(coverage, 1.00)),
              size = 8/.pt,
              family = "Source Sans Pro",
              colour = grey50k) +
    geom_text(aes(y = tx_goal, label = comma(gap)),
              size = 8/.pt,
              family = "Source Sans Pro",
              colour = grey90k) +
    facet_wrap(~snu1_order, nrow = 2, scales = "free_y") +
    si_style_ygrid(facet_space = 0.75) +
    scale_y_continuous(labels = comma) +
    labs(x =  NULL, y = NULL,
         title = "Treatment Growth Over Last Three Years: Different Rates of Progress in the More Rural Provinces)",
         caption = "Source: DATIM MSD")
  si_preview()

  si_save("Graphics/ZMB_tx_gap.svg")


# AGE SEX COVERAGE --------------------------------------------------------

  df_agesex <- read_csv("Data/zmb_age_sex_cov.csv", skip = 2)

  df_agesex_long <-
    df_agesex %>%
    pivot_longer(cols = 2:13,
                 names_to = c("age", "sex"),
                 names_pattern = "^(.+)_(.+)$",
                 values_to = "coverage") %>%
    mutate(coverage = gsub("%", "", coverage) %>% as.numeric() / 100) %>%
    mutate(snu1 = fct_relevel(snu1, snu1_order),
           alpha_val = ifelse(coverage < .75, 1, 0.75))

  snu1_order <- df_agesex %>% distinct(snu1) %>% pull()

 a <-  df_agesex_long %>%
    mutate(across(.cols = c(2:3), \(x) gsub("%", "", x) %>% as.numeric()/100)) %>%
    mutate(snu1_order = fct_reorder(snu1, `_PEPFAR`)) %>%
    ggplot(aes(x = age, y = snu1_order)) +
    geom_tile(aes(fill = coverage, alpha = alpha_val), color = "white") +
    geom_text(aes(label = percent(coverage, 1),
                  color = ifelse(coverage < 0.75, "white", grey80k)),
              size = 8/.pt,
              family = "Source Sans Pro") +
    si_style_nolines() +
    facet_wrap(~sex) +
    scale_x_discrete(position = "top") +
    scale_y_discrete(lim = rev) +
    rcartocolor::scale_fill_carto_c(palette = 12, direction = -1, limits = c(0, 1)) +
    theme(strip.placement = "outside",
          legend.position = "none") +
    scale_alpha_continuous(range = c(0.5, 1)) +
    scale_color_identity() +
    labs(x = NULL, y = NULL,
         title = "Casefinding gaps remain among children under 10, adolescents and\nyoung people, and men 25-34 (based on gap to second 95") +
  coord_fixed()

  b <- df_agesex_long %>%
    distinct(snu1, `_PEPFAR`, `_MoH`) %>%
    mutate(across(.cols = c(2:3), \(x) gsub("%", "", x) %>% as.numeric()/100)) %>%
    mutate(snu1_order = fct_reorder(snu1, `_PEPFAR`)) %>%
    pivot_longer(2:3,
                 names_to = "type",
                 values_to = "coverage") %>%
    mutate(alpha_val = ifelse(coverage < .75, 1, 0.75)) %>%
    ggplot(aes(x = type, y = snu1_order)) +
    geom_tile(aes(fill = coverage, alpha = alpha_val), color = "white") +
    geom_text(aes(label = percent(coverage, 1),
                  color = ifelse(coverage < 0.75, "white", grey80k)),
              size = 8/.pt,
              family = "Source Sans Pro") +
    si_style_nolines() +
    scale_x_discrete(position = "top") +
    scale_y_discrete(lim = rev) +
    rcartocolor::scale_fill_carto_c(palette = 12, direction = -1, limits = c(0, 1)) +
    theme(strip.placement = "outside",
          legend.position = "none") +
    scale_alpha_continuous(range = c(0.5, 1)) +
    scale_color_identity() +
    labs(x = NULL, y = NULL,
         title = " \n ") +
    coord_fixed() +
    theme(axis.text.y = element_blank())


  library(patchwork)
  a + b
    si_save("Graphics/ZMB_age_sex_cov.svg")




