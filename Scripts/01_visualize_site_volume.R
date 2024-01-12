# PROJECT: Determine how many sites consistitue 80% of TX_CURR cohort for USAID
# PURPOSE: Munge and Analysis of USAID site level data
# AUTHOR:  Tim Esssam | SI
# REF ID:  c5730b8d
# LICENSE: MIT
# DATE:   2024-01-09
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
    library(mmtable2)
    library(ggbeeswarm)
    library(ggdist)



  # SI specific paths/functions
    load_secrets()
    merdata <- file.path(glamr::si_path("path_msd"))
    file_path <- return_latest(folderpath = merdata,
      pattern = "Site_IM_FY21-24.*_Zambia")

    pano_site_path <- "Data/ZMB_TX_CURR_Sites_Data Tables_ Single OU.xlsx"

  # Grab metadata
   get_metadata(file_path)

  # REF ID for plots
    ref_id <- "c5730b8d"

  # Functions
  # ChatGPT Prompt: User
  # In R, how can I list all objects and their corresponding values from a package that start with the letters "hw"
  # The objects are exported by the package and are not hidden.
  # Preview all hw_colors
  hw_colors <- ls(pos = "package:glitr", pattern = "^hw")
  hw_color_values <- mget(hw_colors, envir = asNamespace("glitr"))
  unlist(hw_color_values) %>% show_col()

  # Darken column header
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

  # Function to create gt table
  make_tx_tbl <- function(.data){
    gt_obj <- .data %>%
      gt(groupname_col = "snu1") %>%
      fmt_number(columns = c(cumulative, targets),
                 decimals = 0) %>%
      fmt_percent(c(4, 6:7),
                  decimals = 0) %>%
      sub_missing() %>%
      cols_label(
        cumulative = "TX_CURR",
        TX_CURR_delta = "% Change",
        targets = "Target",
        targets_delta = "% Change"
      ) %>%
      gt_theme_nytimes() %>%
      tab_options(
        table.font.size = px(10),
        row_group.font.weight = "bold",
        data_row.padding = px(0.5),
        column_labels.font.size = px(12),
        row_group.padding = px(1),) %>%
      drkn_clmn_hdr()
    return(gt_obj)
  }

# LOAD DATA ============================================================================

  df_site <- read_psd(file_path) %>%
      filter(indicator == "TX_CURR",
             standardizeddisaggregate == "Total Numerator")

  pano_site <- readxl::read_excel(pano_site_path)

# MUNGE ============================================================================

  # Create data frames with TX_CURR trends by Agency / Province / Targets
  df_tx_trend <- df_site %>%
      group_by(funding_agency, snu1, fiscal_year, indicator) %>%
      summarize(across(.cols = c(cumulative, targets), \(x) sum(x, na.rm = T)), .groups = 'drop') %>%
      adorn_achievement() %>%
    clean_agency() %>%
    mutate(agency_fill = case_when(
      funding_agency == "USAID" ~ "#002A6C",
      funding_agency == "CDC" ~ "#F9C555",
      TRUE ~ grey50k)
      )

  df_tx_trend_snu <- df_site %>%
    group_by(snu1, fiscal_year, indicator) %>%
    summarize(across(.cols = c(cumulative, targets), \(x) sum(x, na.rm = T)), .groups = 'drop') %>%
    adorn_achievement()


# VIZ ============================================================================

  #  Drop depuds, show provincial trends by partner
    df_tx_trend %>% filter(funding_agency != "DEDUP") %>%
    mutate(snu1_order = fct_reorder(snu1, cumulative, .desc = T)) %>%
      ggplot(aes(x = factor(fiscal_year))) +
      geom_col(aes(y = targets), fill = grey20k, position = position_nudge(x = -0.2), width = 0.5) +
      geom_col(aes(y = cumulative, fill = agency_fill), width = 0.5) +
        facet_wrap(funding_agency ~ snu1, scale = "free_y") +
    geom_label(data = . %>% filter(fiscal_year != 2024),
               aes(y = cumulative, label = percent(achievement, 1)),
              size = 7/.pt,
              family = "Source Sans Pro",
              vjust = 1) +
  geom_text(data = . %>% filter(fiscal_year != 2024),
            aes(y = cumulative, label = comma(cumulative)),
            size = 8/.pt,
            family = "Source Sans Pro",
            vjust = -0.5) +
  scale_fill_identity() +
  si_style_ygrid() +
  scale_y_continuous(labels = comma)


  # Just show the provinces
  df_tx_trend_snu %>%
    mutate(snu1_order = fct_reorder(snu1, targets, )) %>%
    ggplot(aes(x = factor(fiscal_year))) +
    geom_col(aes(y = targets), fill = grey20k, position = position_nudge(x = -0.2), width = 0.5) +
    geom_col(aes(y = cumulative, fill = hw_hunter), width = 0.5) +
    facet_wrap( ~ snu1, scale = "free_y") +
    geom_label(data = . %>% filter(fiscal_year != 2024),
               aes(y = cumulative, label = percent(achievement, 1)),
               size = 7/.pt,
               family = "Source Sans Pro",
               vjust = 1) +
    geom_text(data = . %>% filter(fiscal_year != 2024),
              aes(y = cumulative, label = comma(cumulative)),
              size = 8/.pt,
              family = "Source Sans Pro",
              vjust = -0.5) +
    scale_fill_identity() +
    si_style_ygrid(facet_space = 0.5) +
    scale_y_continuous(labels = comma)

# Really should organize the SNUs by size
 tb1 <-
   df_tx_trend_snu %>%
   select(1:6) %>%
    select(-indicator) %>%
    mutate(snu1 = fct_reorder(snu1, targets, .desc = T)) %>%
    arrange(snu1) %>%
    #filter(str_detect(snu1, "Lusaka|Copp|South|Cent|East|(^|\\W)Western")) %>%
    filter(str_detect(snu1, "Lusaka|Copp|South|Cent|East")) %>%
    mutate(TX_CURR_delta = (cumulative / lag(cumulative, n = 1)) -1, .by = "snu1", .after = cumulative) %>%
    mutate(targets_delta = (targets / lag(targets, n = 1)) -1, .by = "snu1", .after = targets) %>%
    mutate(TX_CURR_delta = case_when(
      fiscal_year == 2024 ~ NA_real_,
      TRUE ~ TX_CURR_delta
    )) %>%
   make_tx_tbl()

 tb2 <-
   df_tx_trend_snu %>%
   select(1:6) %>%
   select(-indicator) %>%
   mutate(snu1 = fct_reorder(snu1, targets, .desc = T)) %>%
   arrange(snu1) %>%
   filter(str_detect(snu1, "Lusaka|Copp|South|Cent|East|Military", negate = T)) %>%
   mutate(TX_CURR_delta = (cumulative / lag(cumulative, n = 1)) -1, .by = "snu1", .after = cumulative) %>%
   mutate(targets_delta = (targets / lag(targets, n = 1)) -1, .by = "snu1", .after = targets) %>%
   mutate(TX_CURR_delta = case_when(
     fiscal_year == 2024 ~ NA_real_,
     TRUE ~ TX_CURR_delta
   )) %>%
   make_tx_tbl()

 # Save tables as pngs to be read back in
 gtsave(tb1, "Images/tb1.png") #save gt table as png
 gtsave(tb2, "Images/tb2.png")
 table1_png <- png::readPNG("Images/tb1.png", native = TRUE) # read tmp png file
 table2_png <- png::readPNG("Images/tb2.png", native = TRUE)
 #
library(patchwork)
 layout <- c(
   area(t = 0, l = 0, r = 0, b = 0),
   area(t = 0, l = 1, r = 1, b = 0)
 )
 plot(layout)

 (ggplot() + theme_void()) + table1_png + table2_png
   plot_layout(design = layout)

# Combine plots
   library(magick)
   library(grid)

   # Read images
   image1 <- magick::image_read("Images/tb1.png")
   image2 <- magick::image_read("Images/tb2.png")

   # Convert to ggplot
   gg_image1 <- ggplot() +
     annotation_custom(rasterGrob(image1, width=unit(1,"npc"), height=unit(1,"npc"))) +
     theme_void() +
     theme(plot.margin = margin(0,0,0,0))

   gg_image2 <- ggplot() +
     annotation_custom(rasterGrob(image2, width=unit(1,"npc"), height=unit(1,"npc"))) +
     theme_void() +
     theme(plot.margin = margin(0,0,0,0))

   # Combine using patchwork
   combined_plot <- gg_image1 + gg_image2 +
     plot_layout(ncol = 2) &
     plot_annotation(theme = theme(plot.margin = unit(c(0, 0, 0, 0), "cm")))

   ggsave()
     si_save(plot = combined_plot, filename = "Images/ZMB_TX_CURR_Trends_Province.png")


# Site level analysis ============================================================================

    # Using pano sitelevel data
     pano_site %>%
       select(-c(`2020`, `2021`, `2022`)) %>%
       filter(!is.na(`2023`)) %>%
       count(snu1, agency) %>%
       spread(agency, n)

     # First remove PSNU sitenames (do this to get targets to carryalong)
     df_site_tx <-
       df_site %>%
       clean_agency() %>%
       # Remove sites where the psnu name is used as site name
       mutate(district = sitename == psnu) %>%
       filter(district == FALSE) %>%
       filter(fiscal_year == 2023) %>%
       group_by(orgunituid, sitename, funding_agency, snu1, psnu) %>%
       summarise(tx_curr = sum(cumulative, na.rm = T), .groups = "drop") %>%
       mutate(tx_curr_prov = sum(tx_curr), .by = "snu1") %>%
       arrange(snu1, desc(tx_curr))

    # For FY23Q4 TX_CURR Reporting -- which agency is reporting in which provinces?
     df_site_tx %>%
       clean_agency() %>%  View()
       count(funding_agency, snu1) %>%
       spread(funding_agency, n) %>%
       select(Province = snu1,
              CDC, USAID, DOD, DEDUP) %>%
       rowwise %>%
       mutate(TOTAL = max(c_across(CDC:DEDUP), na.rm = T))
       arrange(desc(TOTAL)) %>%
       gt() %>%
       fmt_number(decimals = 0) %>%
       sub_missing() %>%
       tab_header(title = "SITES REPORTING TX_CURR FY23Q4 BY FUNDING AGENCY")

    # Where is there discrepancy between pano_sites and MSD?
    # Appears that the discrepancy comes from the fact that values can be zero in MSD
    # But panorama only returns positive values.
      df_site_tx %>% filter(tx_curr == 0)


  # For now, we'll drop agency affiliation and focus on the sites themselves
    df_site_tx_runsum <-
      df_site_tx %>%
      group_by(orgunituid, sitename, snu1, psnu, tx_curr_prov) %>%
      summarise(tx_curr = sum(tx_curr, na.rm = T), .groups = "drop") %>%
      arrange(snu1, desc(tx_curr))  %>%
      mutate(tx_curr_runsum = cumsum(tx_curr), .by = "snu1") %>%
      mutate(tx_curr_share_prov = tx_curr/tx_curr_prov) %>%
      mutate(prov_runsum = cumsum(tx_curr_share_prov), .by = snu1) %>%
      mutate(top_80 = ifelse(prov_runsum <= .80, 1, 0)) %>%
      mutate(top_80_count = n(), .by = c(snu1, top_80)) %>%
      mutate(prov_sites = n(), .by = c(snu1)) %>%
      mutate(top_80_share = top_80_count/prov_sites) %>%
      clean_column() %>%
      mutate(snu1 = gsub(" Province", "", snu1))


# Viz Site shares ---------------------------------------------------------

    # What is breakdown of sites in 10 80% by province?
    df_site_tx_runsum %>%
      filter(top_80 == 1) %>%
      count(snu1, top_80_count)

    df_site_tx_runsum %>%
      count(snu1, tx_curr_prov)

    df_site_tx_runsum %>%
      filter(str_detect(snu1, "_Mil", negate = T)) %>%
      count(snu1, top_80_share, top_80) %>%
      ggplot(aes(x = snu1, y = top_80_share)) +
      geom_col(aes(fill = top_80)) +
      coord_flip()

  # What does the distribution site volume look like by SNU1?
    # How many facilities reporting per SNU1?
    snu_labels <- df_site_tx_runsum %>%
      count(snu1) %>%
      mutate(snu1_label = str_c(snu1, " (", n, ")"))





    df_site_bswrm <-
      df_site_tx_runsum%>%
      left_join(snu_labels) %>%
      filter(str_detect(snu1, "_Mil", negate = T)) %>%
      mutate(site_count = n(), .by = snu1) %>%
      group_by(snu1) %>%
      mutate(sitename_label = case_when(
        tx_curr == max(tx_curr) ~ paste0(sitename, "\n", comma(tx_curr)),
        tx_curr == min(tx_curr) ~ paste0(sitename, "\n", comma(tx_curr)),
        TRUE ~ NA_character_  # Assign NA for all other cases
      ),
        tx_curr_med_prov = median(tx_curr, na.rm = T)) %>%
      ungroup() %>%
      mutate(snu1_label = fct_reorder(snu1_label, site_count, .desc = T),
             color_fill = if_else(top_80 == 1, hw_orchid_bloom, grey30k))

    med_prov <- df_site_bswrm %>% distinct(median = tx_curr_med_prov, snu1_label)


    df_site_bswrm %>%
      filter(tx_curr > 0) %>%
      ggplot(aes(x = snu1_label)) +
      ggbeeswarm::geom_quasirandom(
        aes(y = ifelse(tx_curr > 1, log(tx_curr), tx_curr),
            color = color_fill),
        width = .4,
        size = 3
      ) +
      geom_text(aes(y = log(tx_curr),
                    label = sitename_label),
                size = 7/.pt,
                family = "Source Sans Pro") +
      geom_hline(data = med_prov, aes(yintercept = log(median))) +
      geom_text(data = med_prov, aes(y = log(median), label = round(median, 0)),
                size = 7/.pt,
                hjust = -1,
                family = "Source Sans Pro") +
      scale_color_identity() +
      facet_wrap(~snu1_label, scales = "free_x", nrow = 1) +
      si_style_ygrid(facet_space = 2) +
      scale_y_continuous(breaks = log(10^(0:5))) +
      labs(title = "TX_CURR SITE VOLUME FY23Q4 BY PROVINCE",
           x = NULL, y = NULL,
           caption = metadata$caption)

    si_save("Graphics/ZMB_beeswarm_site_volume_tx_curr.svg", scale = 1.5)


