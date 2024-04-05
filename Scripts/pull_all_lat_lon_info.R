library(Wavelength)
library(gagglr)
library(tidyverse)
library(grabr)

load_secrets()
ctry_list <- datim_outable(datim_user(), datim_pwd()) %>%
  select(country, country_uid, facility_lvl)

df_orgs <- map_dfr(.x = unique(ctry_list$country_uid),
                   possibly(.f = ~ pull_hierarchy(.x, datim_user(), datim_pwd())))


