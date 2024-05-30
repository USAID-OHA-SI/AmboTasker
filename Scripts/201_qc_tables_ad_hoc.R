# PROJECT:  C:/Users/tessam/Documents/Github/AmboTasker
# PURPOSE:  Target tally for QC
# AUTHOR:   T. Essam | USAID
# REF ID:   d56dc8ed
# LICENSE:  MIT
# DATE:     2024-04-12
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
  library(tameDP)
  library(gt)


# GLOBAL VARIABLES --------------------------------------------------------

  ref_id <- "d56dc8ed"  #a reference to be places in viz captions

  path_tst <- "../../../Downloads/2024-03-06 Target Setting Tool_148pm (3).xlsx"

  path_msd <-  si_path() %>%
    return_latest("PSNUByIMs-Zambia-Frozen-2024-04-08")

  get_metadata(path_msd)  #extract MSD metadata



# IMPORT ------------------------------------------------------------------

  df_msd <- tameDP::tame_dp(path_tst)

  snu1_to_psnu <- read_psd(path_msd) %>% distinct(snu1, psnuuid, psnu)


# MUNGE -------------------------------------------------------------------

  df_msd %>%
    filter(indicator == "TX_CURR",
           standardizeddisaggregate == "Age/Sex/HIVStatus",
           fiscal_year %in% c(2024, 2025)) %>%
    left_join(snu1_to_psnu, by = c("psnuuid")) %>%
    filter(str_detect(snu1, "Cent|Copp|Luap|Much|North")) %>%
    summarize(targets = sum(targets, na.rm = T), .by = c(snu1, fiscal_year)) %>%
    arrange(snu1) %>%
    spread(fiscal_year, targets) %>%
    gt(rowname_col = "snu1") %>%
    fmt_number(where(is.numeric), decimals = 0) %>%
    tab_header(title = "FY24 & FY25 TX_CURR TARGETS") %>%
    tab_source_note(source_note = "Source: 2024-03-06 TST 1:48pm Version") %>%
    gtExtras::gt_theme_nytimes() %>%
    gtExtras::gtsave_extra("Images/TX_CURR_target_table_fy24_25.png")

df_msd %>% filter(indicator == "TX_CURR") %>% count(standardizeddisaggregate, fiscal_year)

# VIZ ---------------------------------------------------------------------


