
# cts --------------------------------------------------------------------------
rm(list = ls())
library(dplyr)
library(sf)

#' This script builds the cts table exported from this package
#' Caution: it takes a while to re-run

# start with 2010 census tract geographies linked to 2010 counties
cts = xwalks::cts2counties_time.series
cts = cts %>% select(1,3,4,5)


# pull data and initial cleans -------------------------------------------------

# opp insights
# source("data-raw/initial-pulls-and-cleans/Opp-insights-CTsCZs.R") # this one takes longest to run
opp_cts <- readRDS(file = "data-raw/Intermediate save/opp_cts.RDS")
source("data-raw/initial-pulls-and-cleans/CDC-neighborhood-indicators.R")
source("data-raw/initial-pulls-and-cleans/NHGIS-data.R")

#ddir <- "./R/initial-pulls-and-cleans/"
#source( paste0(ddir, "Opp-insights-CTsCZs.R"))
#source( paste0(ddir, "NHGIS-data.R"))
#source( paste0(ddir, "CDC-neighborhood-indicators.R"))

tibble(opp_cts)
tibble(allnh)
tibble(lifexp)

make_geoid <- function(df, state.col, county.col, tract.col) {
  # function to make concatenated unique geoid in the style of the NHGIS join code
  df %>%
    mutate_at(c(state.col, county.col, tract.col),
              ~format(., scientific = F, trim = T)) %>%
    mutate(gisjoin = paste0("G",
                          stringr::str_pad(pull(df, state.col)
                                           , width=2, side="left", pad="0"),
                          stringr::str_pad(pull(df, county.col)
                                           , width=4, side="left", pad="0"),
                          stringr::str_pad(pull(df, tract.col)
                                           , width=7, side="left", pad="0")))
  }

# concatenate everything to form single id column -------------------------
tibble(opp_cts)

opp_cts <- opp_cts.backup %>%
  make_geoid("state","county","tract")

opp_cts %>% select(c("state","county","tract", "gisjoin")) %>% head()

sum(opp_cts$geoid %in% allnh$NHGISCODE)
sum(!opp_cts$geoid %in% allnh$NHGISCODE)
#colnames(opp_cts)
opp_cts[!opp_cts$geoid %in% allnh$NHGISCODE, ] %>%
  filter_at(c(6:16),
            any_vars(!is.na(.)))
# the nine census tracts that don't match nhgis data...
# do they match the nhgis shapefile?
'ct_2010 <- st_read("/scratch/network/km31/2010 CTs/US_tract_2010.shp")
glimpse(ct_2010)
temp <- full_join(opp_cts,
                  mutate(ct_2010, GISJOIN = as.character(GISJOIN)),
                  by = c("geoid" = "GISJOIN"))
temp %>%
  st_as_sf() %>%
  filter(st_is_empty(geometry)) %>%
  filter_at(c(6:16),
            any_vars(!is.na(.)))'
# (correct; didn't match there either.)

# merge -------------------------------------------------------------------

opp_cts[1:2,1:5]
allnh[1:2,1:5]
cts <- full_join(allnh, opp_cts,
                 by = c("NHGISCODE" = "gisjoin"))

glimpse(lifexp)
lifexp <- lifexp %>%
  make_geoid("STATE2KX", "CNTY2KX", "TRACT2KX")

cts <- full_join(cts, lifexp,
                 by = c("NHGISCODE" = "gisjoin"))


# check Tract IDs match ---------------------------------------------------
cts %>% select( c(state, county, tract,
                  "STATE2KX", "CNTY2KX", "TRACT2KX", "Tract.ID", NHGISCODE))
# ..and drop duplicate id columns
cts <- cts %>%
  select(-c(state, county, tract,
            "STATE2KX", "CNTY2KX", "TRACT2KX", "Tract.ID"))

# drop other unneeded columns, rearrange, & rename ---------------------------------------------------
colnames(cts)
cts <- cts %>% select(-c(se.e.0.., Abridged.life.table.flag))

cts <- cts %>% select(statefp = 1,
                      countyfp = 2,
                      tractce = 3,
                      gisjoin = 4,
                      cz, czname,
                      6,7,5,8:12,
                      15:ncol(cts)) # :)
head(cts)
summary(cts)
# filter rows where all relevant cols are NAs -----------------------------
nrow(cts) # 111219

cts %>%
  filter_at(c(7:ncol(cts)),
            all_vars(is.na(.))) %>% nrow() # 37,000 rows only NAs for these vars

cts <- cts %>%
  filter_at(c(7:ncol(cts)),
            any_vars(!is.na(.))) # 73972


unique(nchar(cts$tractce))
cts %>% filter(is.na(tractce)) %>% summary() # 9
cts <- cts %>% filter(!is.na(tractce))

# final checks
nrow(cts)
summary(cts)

cts %>%
  filter(is.na(e.0.)) %>%
  count(statefp)
cts %>% count(statefp)


# ensure can be easily merged w/ spatial and xwalk cts -------------------------
cts <- tibble(cts)

cts[,1:5]
cts$geoid <- paste0(cts$statefp,
                    cts$countyfp,
                    cts$tractce)
cts <- cts %>% select(1:3,geoid, (4:(ncol(cts)-1)))
#cts[,c(1:5)]
#tibble(divDat::cts)
# save csv ----------------------------------------------------------------

#write.csv(cts, file = "./data/ct_outcomes.csv")
# next is adding variances

