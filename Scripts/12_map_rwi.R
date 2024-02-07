# PROJECT: Spatial kriging of RWI for Zambia
# PURPOSE: Munge and Analysis of RWI from Facebook
# AUTHOR:  Tim Esssam | SI
# REF ID:  c2af23d5
# LICENSE: MIT
# DATE:   2024-02-05
# NOTES: Creating an interpolated surface from data for mapping

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
    library(gstat)
    library(sp)

  # SI specific paths/functions
    load_secrets()
    Source("Scripts/extract_boundaries.R")
    pntdata <- file.path(glamr::si_path("path_vector"), "OU-Sites")

    rwi <- "Data/zmb_relative_wealth_index.csv"
    fac_coord_path <- return_latest(folderpath = pntdata, "Zambia - facilities")

  # REF ID for plots
    ref_id <- "c2af23d5"

  # Functions


# LOAD DATA ============================================================================

    # Loading wealth index and adding spatial properties to make it SF compatible
    df_rwi <- read_csv(rwi) %>%
      st_as_sf(., coords = c("longitude", "latitude"), crs = 4326)

    # Custom Admin1 shapefile per redistricting in 2023
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

# MUNGE ============================================================================

  # Let's check the distribution of the point data first histogram / QQ plot
  # Conclusion: Data is pretty much normally distributed with a bit of a fat tail on the right
    df_rwi %>%
      ggplot() +
      geom_histogram(aes(x = rwi)) +
      si_style()

    standardized_data <- (df_rwi$rwi - mean(df_rwi$rwi)) / sd(df_rwi$rwi)
    ks.test(standardized_data, "pnorm")

    # QQ PLOT
    qq_data <- qqnorm(df_rwi$rwi, plot.it = F)
    qq_df <- data.frame(Theoretical = qq_data$x, Sample = qq_data$y)

    ggplot(qq_df, aes(sample = Sample)) +
      stat_qq(color = hw_electric_indigo) +
      stat_qq_line(color = hw_sun_kissed) +
      ggtitle("QQ Plot") +
      xlab("Theoretical Quantiles") +
      ylab("Sample Quantiles") +
      si_style()


# KRIGING ESTIMATION ============================================================================

  # Hold up due to RStudio and R package versions. 'BrBG"
    # Get bounding box for Zambia
    bbox_zmb <- st_bbox(zmb_adm1)

    library(osmdata)

    # What attributes exist for highways
    available_features ()
    available_tags("highway") %>% prinf()
    ?available_tags()

    q <-  opq(bbox = bbox_zmb) %>%
      add_osm_feature(key = 'highway', value = c("motorway", "primary", "secondary", "corridor", "road")) %>%
      osmdata_sf()

    View(q[["osm_lines"]])

    ggplot() +
      geom_sf(data = q$osm_lines)



    df_rwi %>%
      ggplot() +
      geom_sf(aes(color = rwi), size = 0.5) +
      geom_sf(data = zmb_adm1, color = grey90k, fill = NA) +
      geom_sf(data = q$osm_lines) +
      scale_color_gradientn(colours = RColorBrewer::brewer.pal(11, 'Spectral'))


# SPINDOWN ============================================================================

