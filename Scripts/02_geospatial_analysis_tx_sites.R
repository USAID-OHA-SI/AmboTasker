# PROJECT: Calculate the Square Kilometers of each province in Zambia & Convex Hull of Coverage
# PURPOSE: Munge and Analysis of
# AUTHOR:  Tim Esssam | SI
# REF ID:  6f0f880c
# LICENSE: MIT
# DATE:   2024-01-12
# NOTES:

# LOCALS & SETUP ============================================================================

  # Libraries
    library(gagglr)
    library(tidyverse)
    library(scales)
    library(tidytext)
    library(patchwork)
    library(sf)
    library(ggtext)
    library(glue)
    library(gt)
    library(gtExtras)


  # SI specific paths/functions
    load_secrets()
    gisdata <- file.path(glamr::si_path("path_vector"))

    gis_path <- return_latest(folderpath = file.path(gisdata, "OU-Sites"),
                              pattern = "Zambia")

    merdata <- file.path(glamr::si_path("path_msd"))
    file_path <- return_latest(folderpath = merdata,
                               pattern = "Site_IM_FY21-24.*_Zambia")

    shpdata <- file.path(glamr::si_path("path_vector"))



  # Grab metadata
   get_metadata(file_path)

  # REF ID for plots
    ref_id <- "6f0f880c"

  # Map Infra
    cntry <- "Zambia"
    spdf_pepfar <- gisr::get_vcpolygons(name = "VcPepfarPolygons.shp")
    zmb_org <- grabr::datim_orgunits(cntry = cntry) %>% filter(orgunit_level == 5)

    # May need to rework below depending on gisr updates (use zmb_org)
    zmb_geo <- purrr::map(3:5, ~extract_boundaries(spdf_pepfar,
                                        country = cntry,
                                        level = .x,
                                        username = datim_user(),
                                        password = datim_pwd()))


    names(zmb_geo) <- list("adm0", "snu1", "psnu")

    zmb_adm1 <- st_read("../Zambezi/GIS/snu1_fy22.shp") %>%
      st_transform(., crs = 4326)




# LOAD DATA ============================================================================

  site_gis <- read_csv(gis_path)
  df_site_roster <- read_psd(file_path) %>%
    filter(indicator == "TX_CURR") %>%
    filter(cumulative > 0, fiscal_year == 2023) %>%
    distinct(orgunituid, snu1, psnu, sitename)

# MUNGE ============================================================================

  df_geo <- right_join(site_gis, df_site_roster)

# CALACULATE CONVEX HULL BY PROVINCE ============================================================================

  # Assuming df is your data frame with 'lat' and 'lon' columns
  # Convert df to an sf object
  df_sf <- st_as_sf(df_geo %>% filter(!is.na(longitude)), coords = c("longitude", "latitude"), crs = 4326)

  # Some points appear to "out" of their Province. Let's filter these out.
  df_sf_joined <- st_join(df_sf, zmb_adm1) %>%
    mutate(snu1 = case_when(
      snu1.x != snu1.y ~ snu1.y,
      TRUE ~ snu1.x))

  df_sf_joined %>%
    ggplot() +
    geom_sf(data = zmb_adm1) +
    geom_sf_text(aes(label = snu1.x)) +
    geom_sf() +
    facet_wrap(~snu1.y)



  # Check shapefile
  # Assuming points_sf and polygons_sf are your two sf objects
  # Retrieve CRS for both
  crs_points = st_crs(df_sf)
  crs_polygons = st_crs(zmb_adm1)

  # Compare CRS
 crs_points == crs_polygons

  # Check that it worked
  df_sf %>%
    ggplot() +
    geom_sf(data = zmb_adm1) +
    geom_sf()

  # Now calculate the convex hull
  convex_hulls <- df_sf_joined %>%
    group_by(snu1) %>%
    summarise(geometry = st_convex_hull(st_union(geometry))) %>%
    st_cast("POLYGON") # Ensuring the geometry type is POLYGONv

  ggplot() +
    geom_sf(data = zmb_geo$adm0, fill = grey10k, alpha = 0.5)+
    #geom_sf(data = convex_hulls, color = "red") +
    geom_sf(data = zmb_adm1, fill = grey20k, color = "white") +
    geom_sf(data = df_sf_joined, colour = "blue") +
    facet_wrap(~snu1, nrow = 2) +
    theme_minimal()

  # Back to core task - What is the surface area of each snu1?
  zmb_adm1 %>%
    group_by(snu1) %>%
    summarise(area_sq_km = st_area(geometry)/10^6)


  # Grab wikipedia table
  library(rvest)
  # URL of the Wikipedia page
  url <- "https://en.wikipedia.org/wiki/Provinces_of_Zambia"

  # Read the webpage
  webpage <- read_html(url)

  # Scrape the first table
  area_table <- html_table(webpage, fill = TRUE)[[4]]

  # View the first few rows of the table
  area_table <-
    area_table %>%
    mutate(snu1 = gsub("\\[\\d+\\]", "", `Province[1]`),
           area = gsub(",", "", `Area (km2)`) %>% as.numeric()) %>%
    mutate(agency_cov = case_when(
     str_detect(snu1, "Cent|Copp|Luap|Much|North-W|Northern") ~ "USAID",
     TRUE ~ "CDC"
    )) %>%
    filter(snu1 != "Zambia") %>%
    mutate(agency_area = sum(area), .by = agency_cov)


# SPINDOWN ============================================================================

