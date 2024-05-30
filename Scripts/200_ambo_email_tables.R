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
  library(gtExtras) # For the nytimes theme
  library(glue)
  library(rcartocolor) # For the sunsetDark color palette


# GLOBAL VARIABLES --------------------------------------------------------

  ref_id <- "a0964b89"  #a reference to be places in viz captions

  path_msd <-  si_path() %>%
    return_latest("PSNUByIMs-Zambia-Frozen-2024-04-08")

  meta <- get_metadata(path_msd)


# TABLE TO BE REPRODUCED --------------------------------------------------

  # Pull in the value from the image using Excel


  # amb_tbl <-  tibble::tribble(
  #                                  ~SNU1, ~FY20, ~FY21, ~FY22, ~FY23, ~FY24,
  #                      "Lusaka Province",  0.78,  0.81,  1.01,  1.05,  0.99,
  #                     "Western Province",  0.87,   0.9,  1.09,  1.05,  0.98,
  #                     "Eastern Province",  0.88,  0.86,     1,  1.02,  0.97,
  #                    "Southern Province",  0.81,  0.77,  0.91,  0.96,  0.96,
  #                    "Muchinga Province",  0.74,  0.83,  0.99,  1.11,  0.93,
  #                    "Northern Province",  0.77,  0.76,  0.96,  1.05,  0.88,
  #                     "Central Province",  0.88,  0.87,  0.79,  0.96,  0.86,
  #                     "Luapula Province",  0.84,  0.69,  0.89,  0.93,  0.85,
  #                  "Copperbelt Province",  0.81,  0.83,  0.81,  0.88,  0.83,
  #                     "_MiIitary Zambia",   0.8,  0.55,  0.74,  0.79,  0.82,
  #                "Northwestern Province",  1.43,  1.04,  0.86,  0.91,  0.79
  #                )


# Function to calculate TX_CURR achievement from cumulative / targets
  calc_tx_curr <- function(.data){
    .data %>%
      filter(indicator == "TX_CURR",
             standardizeddisaggregate == "Total Numerator") %>%
             #funding_agency == "USAID") %>%
      group_by(fiscal_year, snu1) %>%
      summarize(across(.cols = c("cumulative", "targets"), \(x) sum(x, na.rm = T)), .groups = "drop") %>%
      calc_achievement()
  }

  # Define a reasonable color palette to use to show variation in achievement
  carto_fill <- rcartocolor::carto_pal(name = "SunsetDark")
  show_col(carto_fill, borders = F)


# IMPORT ------------------------------------------------------------------

  df_genie <- read_psd(path_msd) %>%
    filter(fiscal_year > 2015) %>%
    calc_tx_curr()


# IMPORT FULL -------------------------------------------------------------


  # df_genie <- read_psd(path_msd) %>%
  #   filter(fiscal_year > 2015) %>%
  #   calc_tx_curr() %>%
  #   rowwise() %>%
  #   mutate(max_val = max(cumulative, targets)) %>% # Using this to reorder snu1s
  #   ungroup() %>%
  #   mutate(final_val = last(max_val), .by = "snu1") %>%
  #   mutate(snu1_sort = fct_reorder(snu1, final_val, .desc = T),
  #          snu1_limits = case_when(
  #            str_detect(snu1, "Lusaka|Copper|Southern|Central") ~ 4e5,
  #            TRUE ~ 1.15e5
  #          ))

  #df_genie %>%
  #  filter(str_detect(snu1, "NorthW"))

# VIZ ---------------------------------------------------------------------

  df_genie %>%
    ggplot(aes(x = factor(fiscal_year))) +
    geom_blank(aes(y = snu1_limits)) +
    geom_col(aes(y = targets), position = position_nudge(x = 0.1), fill = grey20k, width = 0.5) +
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


# GT TABLE TIME -----------------------------------------------------------

  amb_tbl %>%
    gt(rowname_col  = "SNU1") %>%
    # Format to percentages
    fmt_percent(columns = where(is.double), decimals = 0) %>%
    # Add Title
    tab_header(title = "HISTORICAL TX_CURR ACHEIVEMENT BY PROVINCE?") %>%
    # Add in a custom source note
    tab_source_note(source_note = "Source: Email Correspondence April 31, 2051") %>%
    # Add gtExtra::theme_nytimes
    gt_theme_nytimes() %>%
    # Footnote size and collapse space between rows
    tab_options(source_notes.font.size = px(10),
                data_row.padding = px(1)) %>%
    gt_color_rows(columns = where(is.double), palette = carto_fill)


# Write a function to darken column headers (nytimes theme is light)
  # Darken columns
  #
  drkn_clmn_hdr <- function(.data) {
    .data %>%
      tab_style(
        style = list(
          cell_text(color = grey90k)
        ),
        locations = cells_column_labels()
      )
  }

# Make a table showing the correct percent achievement
# Extract bounds for the color gradient from df
  min <- min(df_genie$achievement)
  max <- max(df_genie$achievement)

  # ^ matches the beginning of the string
  # .{2} matches any two characters
  text_sub <- "^.{2}"

  df_genie %>%
    select(snu1, achievement, fiscal_year) %>%
    mutate(fiscal_year = gsub(text_sub, "FY", fiscal_year)) %>%
    pivot_wider(names_from = fiscal_year,
                values_from = achievement) %>%
    arrange(desc(FY24)) %>%
    gt(rowname_col = "snu1") %>%
    fmt_percent(columns = where(is.double), decimals = 0) %>%
    gt_theme_nytimes() %>%
    gt_color_rows(columns = where(is.double), palette = carto_fill, domain = c(min, max)) %>%
    drkn_clmn_hdr() %>%
    tab_header(title = "HISTORICAL TX_CURR ACHEIVEMENT BY PROVINCE") %>%
    tab_source_note(
      source_note = md(glue("{meta$caption} | Created by T. Essam"))) %>%
    tab_options(source_notes.font.size = px(10), data_row.padding = px(1))


