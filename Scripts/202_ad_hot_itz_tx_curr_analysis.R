# PROJECT:  C:/Users/tessam/Documents/Github/AmboTasker
# PURPOSE:  Itezhi-Tezhi review
# AUTHOR:   T. Essam | USAID
# REF ID:   53f84d8a
# LICENSE:  MIT
# DATE:     2024-05-06
# UPDATED:

# DEPENDENCIES ------------------------------------------------------------

  #general
  library(tidyverse)
  library(glue)
  #oha
  library(gagglr)
  library(scales, warn.conflicts = FALSE)
  library(systemfonts)
  library(tidytext)
  library(patchwork)
  library(ggtext)
  library(gt)
  library(gtExtras)


# GLOBAL VARIABLES --------------------------------------------------------

  ref_id <- "53f84d8a"  #a reference to be places in viz captions

  path_msd <-  si_path() %>% return_latest("Site_IM.*Zambia")

  meta <- get_metadata(path_msd)  #extract MSD metadata


  drkn_clmn_hdr <- function(.data) {
    .data %>%
      tab_style(
        style = list(
          cell_text(color = grey90k)
        ),
        locations = cells_column_labels()
      )
  }

  # Define a reasonable color palette to use to show variation in achievement
  carto_fill <- rcartocolor::carto_pal(name = "Earth")
  show_col(carto_fill, borders = F)

# IMPORT ------------------------------------------------------------------

  df_msd <- read_psd(path_msd) %>%
    filter(str_detect(psnu, "Itezhi"))

  df_site <-
    df_msd %>% filter(indicator %in% c("TX_CURR", "TX_NET_NEW", "VMMC_CIRC", "TX_NEW", "TX_PVLS", "PrEP_NEW"),
                    standardizeddisaggregate %in% c("Total Numerator", "Total Denominator"),
                    sitename != "Itezhi-tezhi District") %>%
    clean_indicator() %>%
    group_by(sitename, orgunituid, fiscal_year, funding_agency, indicator, numeratordenom) %>%
    summarize(across(matches("qtr"), \(x) sum(x, na.rm = T))) %>%
    ungroup() %>%
    reshape_msd(., direction = "quarters")

# MUNGE -------------------------------------------------------------------

# Start with TX_CURR focus
  df_tx_wide <- df_site %>%
    arrange(indicator, period) %>%
    filter(indicator == "TX_CURR") %>%
    select(-results_cumulative, -fiscal_year, -funding_agency) %>%
    mutate(spell = n() %>% as.character, .by = sitename) %>%
    mutate(spell = ifelse(spell == "9", "USAID-->CDC", "CDC Expansion")) %>%
    pivot_wider(names_from = period,
                values_from = results) %>%
    arrange(desc(spell), desc(FY24Q1)) %>%
    mutate(`Handoff Net Growth` = case_when(
      spell == "USAID-->CDC" ~ FY24Q1 - FY22Q4,
      TRUE ~ FY24Q1 - FY23Q1
    ))

  max <- max(df_tx_wide$`Handoff Net Growth`)

  df_tx_wide %>%
    gt(groupname_col= "spell") %>%
    cols_hide(columns = c(orgunituid, indicator, numeratordenom)) %>%
    fmt_number(where(is.numeric), decimals = 0) %>%
    summary_rows(groups = everything(),
                 columns = where(is.numeric),
                 fns = list(`Subtotal` = ~sum(., na.rm = TRUE)), ##na.rm = TRUE is important here
                 formatter = fmt_number,
                 decimals = 0) %>%
    fmt_missing(columns = everything(), missing_text = "") %>%
    tab_spanner(label = "CDC Supported",
                columns = FY23Q1:FY24Q1) %>%
    tab_spanner(label = "USAID Supported",
                columns = FY22Q1:FY22Q4) %>%
    grand_summary_rows(columns = where(is.numeric),
                       fns = list(
                        `Total TX_CURR` = ~sum(., na.rm = T)),
                        formatter = fmt_number,
                        decimals = 0) %>%
    gt_theme_nytimes() %>%
    gt_color_rows(columns = FY22Q1:FY22Q4, palette = c("#c43d4d"),  na.color = "white") %>%
  gt_color_rows(columns = FY23Q1:FY24Q1, palette = c("#2057a793"),  na.color = "white", alpha = TRUE) %>%
    gt_color_rows(columns = `Handoff Net Growth`, palette = carto_fill, domain = c(-max, max)) %>%
    drkn_clmn_hdr() %>%
    tab_header(title = "HISTORICAL SITE-LEVEL TX_CURR ACHIEVEMENT IN ITEZHI-TEZHI") %>%
    tab_source_note(
      source_note = md(glue("{meta$caption} | Created by T. Essam"))) %>%
    tab_options(source_notes.font.size = px(10), data_row.padding = px(1),
                grand_summary_row.padding = px(1),
                summary_row.padding = px(1),
                ) %>%
    opt_vertical_padding(scale = 0.25) %>%
    tab_style(style = list(
      cell_text(weight = 750)
    ),
    locations = cells_grand_summary()) %>%
  gtsave_extra(filename = "Images/ZMB_IZT_tx_curr_achv_table.png")


# VIZ ---------------------------------------------------------------------


