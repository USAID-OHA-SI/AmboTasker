# PROJECT: Mapa Zambia
# PURPOSE: Create FY23 Coverage maps for All of Zambia
# AUTHOR: Tim Essam | SI
# REF ID:   702b8752a
# LICENSE: MIT
# DATE: 2022-10-17
# NOTES: Tim Essam | SI

# LOCALS & SETUP ============================================================================

  # Libraries
  library(gagglr)
  library(tidyverse)
  library(scales)
  library(sf)
  library(glue)
  library(extrafont)
  library(tidytext)
  library(patchwork)
  library(ggtext)
  library(selfdestructin5)
  library(gt)
  library(cascade) # Use dev version
  library(ggpattern)

  source("Scripts/extract_boundaries.R")

# SI specific paths/functions
  glamr::load_secrets()
  merdata <- file.path(glamr::si_path("path_msd"))
  file_path <- glamr::return_latest(folderpath = merdata,
                             pattern = "Site_IM.*Zambia.zip")

  pntdata <- file.path(glamr::si_path("path_vector"), "OU-Sites")

  rwi <- "Data/zmb_relative_wealth_index.csv"

  fac_coord_path <- return_latest(folderpath = pntdata, "Zambia - facilities")

  # REF ID for plots
  ref_id <- "702b8752a"

  # Grab metadata
  gophr::get_metadata(file_path)

# LOAD DATA ---------------------------------------------------------------

  df_rwi <- read_csv(rwi) %>%
    st_as_sf(., coords = c("longitude", "latitude"), crs = 4326)

  df_site <- gophr::read_psd(file_path)

  # Roll it up by mechanism at the PSNU-level
  df_site_cov <- df_site %>%
    gophr::clean_agency() %>%
    filter(fiscal_year == metadata$curr_fy,
           funding_agency %in% c("USAID", "CDC"),
           indicator == "TX_CURR",
           standardizeddisaggregate == "Total Numerator") %>%
    group_by(orgunituid, fiscal_year, funding_agency) %>%
    summarize(active = sum(cumulative, na.rm = T), .groups = "drop")

  df_site_cov %>% filter(active > 0) %>% count(funding_agency)

  # Map Infra
  fac_pts <- read_csv(fac_coord_path)

  cntry <- "Zambia"
  spdf_pepfar <- gisr::get_vcpolygons(name = "VcPepfarPolygons.shp")
  zmb_org <- grabr::datim_orgunits(cntry = cntry) %>% filter(orgunit_level == 5)

  # May need to rework below depending on gisr updates (use zmb_org)
  zmb_geo <- purrr::map(3:6, ~extract_boundaries(spdf_pepfar,
                                                 country = cntry,
                                                 level = .x,
                                                 username = datim_user(),
                                                 password = datim_pwd()))
  # Assign list names to the zmb geo list
  names(zmb_geo) <- list("adm0", "snu1", "psnu")

  # Using custom shapefile b/c of redistricting
  zmb_adm1 <- st_read("../Zambezi/GIS/snu1_fy22.shp") %>%
    st_transform(., crs = 4326)

  # Merge point data with results
  df_zmb_cov <-
    df_site_cov %>% left_join(fac_pts) %>%
    select(orgunituid, funding_agency, lon = longitude, lat = latitude) %>%
    filter(!is.na(orgunituid)) %>%
    mutate(agency_fill = ifelse(funding_agency == "USAID", hw_midnight_blue, hw_lavender_haze))


# MAPPY TIME --------------------------------------------------------------

  df_zmb_cov %>%
    add_tally() %>%
    mutate(agency_count = n(), .by = "funding_agency",
           pct_reach = percent(agency_count/n)) %>%
    mutate(facet_label = glue("{funding_agency}\n{comma(agency_count)} ({pct_reach}) Total Facilities Supported out of {comma(n)}")) %>%
    ggplot() +
    geom_sf(data = zmb_adm1, fill = grey10k) +
    geom_point(aes(x = lon, y = lat, color = agency_fill), alpha = 0.8, size = 1) +
    geom_point(aes(x = lon, y = lat), alpha = 0.1, shape = 1, color = "white", size = 1) +
    geom_sf(data = zmb_geo$adm0, fill = NA, color = grey90k, size = 2) +
    scale_color_identity() +
    facet_wrap(~facet_label) +
    si_style_map() +
    labs(x = NULL, y = NULL,
         title = "PEPFAR SUPPORTED FACILITIES BY FUNDING AGENCY",
         caption = metadata$caption)
  si_save("Graphics/ZMB_coverage_map_fy23.svg")

  df_rwi %>%
    ggplot() +
    geom_sf(aes(fill = rwi))




