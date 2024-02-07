# PROJECT: Analysis of IIT across sites and mech transition
# PURPOSE: Munge and Analysis of IIT
# AUTHOR:  Tim Esssam | SI
# REF ID:  5e198eac
# LICENSE: MIT
# DATE:   2024-02-06
# NOTES:

# LOCALS & SETUP ============================================================================

  # Libraries
    library(gagglr)
    library(tidyverse)
    library(scales)
    library(glue)
    library(ggtext)


  # SI specific paths/functions
    load_secrets()
    merdata <- file.path(glamr::si_path("path_msd"))
    site_path <- return_latest(folderpath = merdata,
      pattern = "SiteByIMs.*Zambia")

  # Grab metadata
   get_metadata(file_path)

  # REF ID for plots
    ref_id <- "5e198eac"

  # Functions


# LOAD DATA ============================================================================

    df_site <- read_psd(site_path) %>%
      mutate(snu1 = str_replace_all(snu1, " Province", ""))

    full_pds <- (min(df_site$fiscal_year) - 1) %>%
      paste0("-10-01") %>%
      as_date() %>%
      seq.Date(convert_qtr_to_date(metadata$curr_pd), by = "3 months") %>%
      convert_date_to_qtr()

    pd_brks <- str_replace(full_pds, "FY.*(1|3)$", "")

# MUNGE ============================================================================

    # Remap SAFE into ZIH
    df_site %>% filter(funding_agency == "USAID") %>%
      count(mech_name, mech_code)

    df_site <- df_site %>%
      mutate(mech_code = ifelse(mech_code == "17413", "82086", mech_code),
             mech_name = ifelse(mech_name == "SAFE", "ZIHA", mech_name))


    df_site %>%
      filter(indicator == "TX_CURR",
             standardizeddisaggregate == "Age/Sex/HIVStatus",
             trendscoarse != "Unknown Age") %>%
      group_by(indicator, fiscal_year, funding_agency) %>%
      summarise(across(c(targets, starts_with("qtr")), \(x) sum(x, na.rm = TRUE)), .groups = "drop") %>%
      reshape_msd("quarters") %>%
      select(-results_cumulative) %>% prinf()



    df_tx <- df_site %>%
      filter(funding_agency == "USAID",
             indicator == "TX_CURR",
             standardizeddisaggregate == "Age/Sex/HIVStatus",
             trendscoarse != "Unknown Age") %>%
      group_by(indicator, fiscal_year, trendscoarse, snu1) %>%
      group_by(indicator, fiscal_year, trendscoarse, snu1) %>%
      summarise(across(c(targets, starts_with("qtr")), \(x) sum(x, na.rm = TRUE)), .groups = "drop") %>%
      reshape_msd("quarters") %>%
      select(-results_cumulative)

    df_tx <- df_tx %>%
      group_by(snu1, trendscoarse) %>%
      mutate(decline = results < lag(results, 1),
             decline_shp = ifelse(decline == TRUE, "\u25Bc", "\u25B2"),
             fill_color = case_when(fiscal_year < metadata$curr_fy ~ trolley_grey,
                                    decline == TRUE ~ golden_sand,
                                    TRUE ~ scooter),
             fill_alpha = ifelse(fiscal_year < metadata$curr_fy, .6, .9),
             results_latest = case_when(period == max(period) ~ results),
             decline_latest = case_when(period == max(period) ~ decline_shp)) %>%
      fill(results_latest,decline_latest, .direction = "up") %>%
      mutate(disp_name = glue("{snu1} {decline_latest}")) %>%
      ungroup()

    v_tx_lrg <- df_tx %>%
      filter(period == max(period),
             trendscoarse == "<15") %>%
      arrange(desc(results)) %>%
      mutate(cumsum = cumsum(results)/sum(results)) %>%
      slice_head(n = 11) %>%
      pull(snu1)

    df_tx %>%
      filter(
        snu1 %ni% c("Eastern", "Southern"),
        trendscoarse == "<15") %>%
      ggplot(aes(period, results, fill = fill_color, alpha = fill_alpha)) +
      geom_col() +
      geom_text(data = . %>% filter(period == max(period)),
                aes(label = comma(results_latest)),
                vjust = -.7, color = matterhorn,
                family = "Source Sans Pro") +
      facet_wrap(~fct_reorder2(disp_name, period, results), scales = "free_y") +
      scale_fill_identity() +
      scale_alpha_identity() +
      scale_y_continuous(label = label_number(scale_cut = cut_short_scale())) +
      scale_x_discrete(labels = pd_brks) +
      coord_cartesian(expand = T, clip = "off") +
      labs(x = NULL, y = NULL,
           title = glue("OVERALL PEDIATRIC TX_CURR THROUGH {metadata$curr_pd}"),
           subtitle = glue("FY22 flagged <span style='color:{golden_sand}'>decline</span>/<span style='color:{scooter}'>growth</span>"),
           caption = glue("Source: {metadata$source}")) +
      si_style_ygrid() +
      theme(panel.spacing = unit(.5, "line"),
            plot.subtitle = element_markdown(),
            strip.text = element_markdown())

# IIT by USAID Sites===================================================================

    # Things look really bad. Let's divide the sites into clusters based on loss from Q1 of last year
    # Second, how many sites are not reporting in FY24Q1 that reported last year?

    # REMAPS SAFE TARGETS TO ZIH
    df_iit <- df_site %>%
      filter(funding_agency == "USAID",
             indicator %in% c("TX_ML", "TX_CURR", "TX_NEW", "TX_RTT"),
             standardizeddisaggregate %in% c("Age/Sex/HIVStatus", "Age/Sex/ARTNoContactReason/HIVStatus"),
             facility != "Data reported above Facility level") %>%
      group_by(fiscal_year, snu1, trendscoarse, facility, indicator, orgunituid, mech_name) %>%
      #group_by(fiscal_year, snu1, facility, facilityuid, indicator, mech_code, mech_name, psnu) %>%
      summarise(across(starts_with("qtr"), sum, na.rm = TRUE), .groups = "drop") %>%
      reshape_msd(include_type = FALSE) %>%
      pivot_wider(names_from = "indicator",
                  names_glue = "{tolower(indicator)}") %>%
      arrange(period) %>%
      group_by(trendscoarse, orgunituid) %>%
      mutate(tx_curr_lag1 = lag(tx_curr, n = 1)) %>%
      ungroup()%>%
      rowwise() %>%
      mutate(iit = tx_ml / sum(tx_curr_lag1, tx_new, na.rm = TRUE)) %>%
      ungroup()


    df_tmp <- df_iit %>%
      group_by(orgunituid) %>%
      complete(period, trendscoarse, snu1, facility) %>%
      ungroup() %>%
      mutate(tx_growth_yoy = ((tx_curr/lag(tx_curr, n = 4)) - 1), .by = c(orgunituid, trendscoarse)) %>%
      arrange(orgunituid, trendscoarse, period)

# SPINDOWN ============================================================================

