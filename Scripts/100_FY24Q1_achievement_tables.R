# PROJECT: Create MDB summary tables
# PURPOSE: Munge and Analysis of FY24Q1 MSD initial
# AUTHOR:  Tim Esssam | SI
# REF ID:  57e13bfd
# LICENSE: MIT
# DATE:   2024-02-26
# NOTES:

# LOCALS & SETUP ============================================================================

  # Libraries
    library(gagglr)
    library(tidyverse)
    library(scales)
    library(glue)
    library(ggtext)
    library(selfdestructin5)
    library(gt)


  # SI specific paths/functions
    load_secrets()
    merdata <- file.path(glamr::si_path("path_msd"))
    file_path <- return_latest(folderpath = merdata,
      pattern = "PSNU_IM.*Zambia")

  # Grab metadata
   get_metadata(file_path)

  # REF ID for plots
    ref_id <- "57e13bfd"

    mk_ptr_tbl <- function(df, mech_id) {

      # Filter the data frame for the specified mech_id and get the mech_name
      filtered_df <- df %>%
        filter(mech_code == mech_id)

      mech_name <- filtered_df %>%
        distinct(mech_name) %>%
        pull(mech_name)

      # Run it through creation process
      filtered_df %>%
        make_mdb_df() %>%
        reshape_mdb_df(metadata$curr_pd) %>%
        create_mdb(ou = "Zambia", type = "main", metadata$curr_pd, metadata$source) %>%
        tab_header(title = glue::glue("{mech_name} PERFORMANCE SUMMARY")) %>%
        gtsave(path = "Images", filename = glue::glue("{mech_name}_mdb_main.png"))
    }

# LOAD DATA ====================================================================
  df_msd <- read_psd(file_path)

  df_msd %>% filter(standardizeddisaggregate == "Total Numerator",
                    indicator %in% c("TX_CURR", "HTS_TST_POS")) %>%
    group_by(funding_agency, indicator, fiscal_year) %>%
    summarise(tot = sum(cumulative, na.rm = T))


# MUNGE ========================================================================

    mdb_df   <- make_mdb_df(df_msd)
    mdb_tbl  <- reshape_mdb_df(mdb_df, metadata$curr_pd)

    # Create the treatment data frame needed for derived indicators
    mdb_df_tx    <- make_mdb_tx_df(df_msd)
    mdb_tbl_tx   <- reshape_mdb_tx_df(mdb_df_tx, metadata$curr_pd)

    mdb_tbl %>%
      # filter(indicator != "GEND_GBV") %>%
      create_mdb(ou = "Zambia", type = "main", metadata$curr_pd, metadata$source) %>%
      gtsave(path = "Images", filename = glue::glue("Zambia_{metadata$curr_pd}_mdb_main.png"))

    create_mdb(mdb_tbl_tx, ou = "Zambia", type = "treatment", metadata$curr_pd, metadata$source) %>%
      bold_column(., metadata$curr_pd %>% substr(., 5, 6)) %>%
      embiggen() %>%
      gtsave(., path = "Images", filename = glue::glue("{metadata$curr_pd}_Zambia_MMD_VL_MD.png"))


# PARTNER TABLES ===============================================================

    mech_list <- c(82075, 17399, 82086, 86412)
    map(mech_list, ~mk_ptr_tbl(df_msd, .x))

    df_msd %>%
      filter(funding_agency == "USAID",
             fiscal_year == 2024) %>%
      count(mech_name, mech_code)

    mk_ptr_tbl(df_msd, 86412)


# SPINDOWN =====================================================================

