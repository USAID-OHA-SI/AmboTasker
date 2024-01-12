extract_boundaries <-
  function(spdf, country,
           level = 3,
           username,
           password,
           export = FALSE,
           name = NULL) {

    # Params
    cntry <- {{country}}

    lvl <- {{level}}

    accnt <- grabr::lazy_secrets("datim", username, password)

    user <- accnt$username

    pass <- accnt$password

    #ou/country orgunit uid
    uid <- grabr::get_ouuid(operatingunit = cntry,
                            username = user,
                            password = pass)

    # list of orgs at the specified level
    orgs <- grabr::get_ouorgs(
      ouuid = uid,
      level = lvl,
      username = user,
      password = pass
    )

    # Check for valid data
    if (base::is.null(orgs)) {
      base::cat(crayon::red("\nNo geodata found for this request.\n"))

      return(NULL)
    }

    orgs <- orgs %>% dplyr::mutate(org_level = lvl)

    # filter sp df
    spdf <- spdf %>%
      dplyr::left_join(orgs, by = "uid") %>%
      dplyr::filter(!is.na(orgunit))

    # Export
    if (export == TRUE & !is.null(name)) {
      # validate name
      name <- base::ifelse(!stringr::str_detect(name, ".shp$"),
                           base::paste0(name, ".shp"),
                           name)

      # Export shapefile
      sf::st_write(spdf,
                   delete_dsn = TRUE,
                   dsn = paste0(name, ".shp"))

    }

    return(spdf)
  }
