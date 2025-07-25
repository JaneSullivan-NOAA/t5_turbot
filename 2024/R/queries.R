# Queries for a Tier 5 version of the Greenland turbot assessment

# survey_definition_id
# 98 = EBS shelf
# 52 = AI
# 78 = EBS slope

# Set up ----

libs <- c("odbc", "keyring", "dplyr", "dbplyr", "tidyr", "lubridate", "readr", "here", "ggplot2")
if(length(libs[which(libs %in% rownames(installed.packages()) == FALSE )]) > 0) {install.packages(libs[which(libs %in% rownames(installed.packages()) == FALSE)])}
lapply(libs, library, character.only = TRUE)

yr <- 2024 # assessment yr
dir.create(here(yr))
dir.create(here(yr, "data"))
dir.create(here(yr, "data", "raw"))

race_spp <- 10115 # race species code for turbot

akfin <- DBI::dbConnect(odbc::odbc(), "akfin", uid = keyring::key_list("akfin")$username,
                        pwd = keyring::key_get("akfin", keyring::key_list("akfin")$username))

# BTS ----

lkup <- tbl(akfin, sql("gap_products.akfin_area")) %>%
  rename_all(tolower) %>% 
  filter(survey_definition_id %in% c(52, 98, 78) & area_type == "REGION") %>% 
  collect() 

# my_ids <- lkup %>% pull(area_id)
my_ids <- c(99901, # EBS shelf standard grid
            99904, # AI
            99905  # EBS slope
)


biom <- tbl(akfin, sql("gap_products.akfin_biomass")) %>%
  rename_all(tolower) %>%
  filter(species_code %in% race_spp & area_id %in% my_ids) %>%
  select(survey_definition_id, year, biomass_mt, biomass_var, n_haul, n_count) %>%
  collect() %>%
  mutate(common_name = "Greenland turbot",
         survey = ifelse(survey_definition_id == 98, "EBS Shelf",
                         ifelse(survey_definition_id == 52, "AI",
                                "EBS Slope")),
         cv = sqrt(biomass_var)/biomass_mt) %>%
  arrange(survey, year) %>%
  select(survey, survey_definition_id, common_name, year, 
         biomass = biomass_mt, cv, n_haul, n_count)

write_csv(biom, here(yr, "data", "raw", "bts_biomass_turbot.csv"))

# LLS ----

lls <- tbl(akfin, sql("afsc.lls_fmp_subarea_all_strata")) %>%
  rename_all(tolower) %>%
  filter(year >= 1996 &
           country == "United States" &
           species_code %in% race_spp & 
           council_management_area %in% c("Aleutians", "Bering Sea")) %>%
  collect() %>% 
  filter(c(council_management_area %in% c("Bering Sea") & year %% 2 == 1) |
           c(council_management_area %in% c("Aleutians") & year %% 2 == 0)) %>%
  mutate(strata = ifelse(council_management_area == "Bering Sea", "EBS Slope", "AI"),
         rpn_cv = sqrt(rpn_var)/rpn,
         rpw_cv = sqrt(rpw_var)/rpw) %>% 
  arrange(year, strata) %>% 
  select(strata, year, rpn, rpn_cv, rpw, rpw_cv)
lls %>% print(n=Inf)

write_csv(lls, here(yr, "data", "raw", "lls_turbot.csv"))
