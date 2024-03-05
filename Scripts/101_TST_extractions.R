# PROJECT: Analysis of COP23 YR 2 TST
# PURPOSE: Munge and Analysis of Northwestern Targets
# AUTHOR:  Tim Esssam | SI
# REF ID:  68555c4c
# LICENSE: MIT
# DATE:   2024-03-05
# NOTES:

# LOCALS & SETUP ============================================================================

  # Libraries
    library(gagglr)
    library(tidyverse)
    library(scales)
    library(glue)
    library(ggtext)
    library(tameDP)



  # SI specific paths/functions
    load_secrets()
    merdata <- file.path(glamr::si_path("path_msd"))
    msd_path <- return_latest(merdata, pattern = "PSNU_IM.*Zambia")
    tst_file_path <- return_latest(folderpath = "Data",
      pattern = "Target Setting")

  # Grab metadata
   get_metadata(msd_path)

  # REF ID for plots
    ref_id <- "68555c4c"

  # Functions


# LOAD DATA ============================================================================

  dp <- tame_subnat(tst_file_path)
  df_msd <- read_psd(msd_path)

  df_msd %>% filter(str_detect(indicator, "PLHIV"))

  snu_psnu_cw <- df_msd %>% distinct(snu1, psnu, psnuuid)

  dp %>% count(standardizeddisaggregate)

  dp %>% left_join(snu_psnu_cw) %>%
    filter(snu1 == "NorthWestern Province",
           indicator %in% c("PLHIV", "TX_CURR_SUBNAT"),
           standardizeddisaggregate == "Age/Sex/HIVStatus") %>%
    group_by(fiscal_year, indicator, psnu, snu1, standardizeddisaggregate) %>%
    summarize(value = sum(targets, na.rm = T)) %>%
    spread(indicator, value) %>% prinf()

  dp %>% left_join(snu_psnu_cw) %>%
    filter(indicator %in% c("PLHIV"),
           standardizeddisaggregate == "Age/Sex/HIVStatus") %>%
    group_by(fiscal_year, indicator, psnu, snu1, ) %>%
    summarize(value = sum(targets, na.rm = T), .groups = "drop") %>%
    spread(indicator, value) %>%
    mutate(PLHIV_province = sum(PLHIV), .by = snu1) %>%
    mutate(district_share_province = PLHIV/PLHIV_province) %>%
    mutate(PLHIV_Zambia = sum(PLHIV)) %>%
    mutate(district_share_Zambia = PLHIV / PLHIV_Zambia) %>%
    mutate(province_share_Zambia = PLHIV_province / PLHIV_Zambia) %>%
    rename(PLHIV_district = PLHIV) %>%
    arrange(snu1) %>%
    write_csv(., "Dataout/PLHIV_TST_FY25.csv")


# MUNGE ============================================================================

  dp %>% left_join(snu_psnu_cw) %>%
    filter(indicator %in% c("PLHIV"),
           standardizeddisaggregate == "Age/Sex/HIVStatus") %>%
    group_by(fiscal_year, indicator, snu1) %>%
    summarize(value = sum(targets, na.rm = T)) %>%
    spread(indicator, value) %>% prinf()

# VIZ ============================================================================

  #read in with join function
  df_final <- join_dp_msd(tst_file_path, msd_path)


  #export to local drive
  today <- lubridate::today()

  readr::write_csv(df_final, glue::glue("Data/ZMB_COP24_v1_joined_{today}.csv"), na = "")

# SPINDOWN ============================================================================
names(df_final)
