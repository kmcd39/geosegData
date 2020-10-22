rm(list = ls())
dyn.load("/usr/local/gdal/2.2.4/lib64/libgdal.so.20.3.3") # necessary to do the spatial work thru server
suppressMessages(library(sf))
suppressMessages(library(dplyr))
suppressMessages(library(data.table))


# note this script takes forever to run -----------------------------------

# generated from scripts in "initial-pulls-and-clean" folder:
cts <- fread( "./data/ct_outcomes.csv" ) %>% select(-V1)

#generated from "extract_centroids" script:
ct_shp <- st_read("/scratch/network/km31/2010 CTs/US_tract_2010_prepped-w-centroids.shp")

ct_shp <- ct_shp %>% mutate(GISJOIN = as.character(GISJOIN))


cts[1:2,1:4]
ct_shp[1:2, ]
cts <- full_join(cts, ct_shp,
                 by = c("geoid" = "GISJOIN"))


# check merge -------------------------------------------------------------
head(select(cts, c(1:5,"lon", "lat", "geometry")))


# no mismatches:
cts[ , c("state", "STATEFP10", "county", "COUNTYFP10", "tract", "TRACTCE10", "geoid")] %>%
  mutate_at(c(1:6), ~as.numeric(as.character(.))) %>%
  filter(state != STATEFP10 |
           county != COUNTYFP10 |
           tract != TRACTCE10)
# drop extra state/county/tract cols
cts <- select(cts, -c("STATEFP10", "COUNTYFP10", "TRACTCE10"))
# check for empty geos:
cts[st_is_empty(cts$geometry), ]
# interestingly, there are a lot, but non that have non-zero, non-na populations
cts[st_is_empty(cts$geometry), ] %>% filter(population != 0 & !is.na(population))

# what's the deal with these rows?
pop_zero <- cts %>% filter(population == 0) %>%
  filter_at(c(15:25),
            any_vars(!is.na(.)))

library(leaflet)
temp <- cts[cts$cz == 24300, ] %>% # (chicago)
  filter(!is.na(geoid)) %>%
  mutate(pop.zero = (geoid %in% pop_zero$geoid)) %>%
  st_as_sf() %>%
  st_transform(4326)
# it seems like there are very small samples for acs 5-yr estimates and opp insights predictions for these rows even if population was 0 in census year. (I use census for population and hh)
pal <- colorNumeric(c("#1111FF", "#FF1111"), c(T,F))
#(highlights the pop_zero ct in chicago in red:)
'leaflet(temp) %>%
  addProviderTiles("OpenStreetMap.DE") %>%
  addPolygons(fillColor = ~pal(temp$pop.zero),
              color = ~pal(temp$pop.zero),
              label = temp$pop.zero,
              fillOpacity = .8,
              stroke = F)'

# get shpfiles to generate xwalks for -------------------------------------
# (gotten from census api - https://www.census.gov/cgi-bin/geo/shapefiles/index.php)
cbsas_2015  <- st_read("/scratch/network/km31/2015 CBSAs (from census bureau)/tl_2015_us_cbsa.shp")

counties_2018 <- st_read("/scratch/network/km31/2018-counties/with-water/tl_2018_us_county.shp")

# function to find intersections ------------------------------------------
# This function takes intersections and then trims out areas that *barely* intersect (i.e., boundary cases where only overlap is the border or one shpfile bleeds slighly over)
# as opposed to above, it returns only 1 row / CT. Note that the function is a little computationally expensive. It takes a couple minutes for the full country.
# i use this function (in separate package; i am also the author) to do this:
xwalks::get.spatial.overlap


# cbsas: -------------------------------------------------------------------
# prep for xwalk join:
head(cts)
st_crs(cts)
cts <- st_as_sf(cts) %>% st_transform(st_crs(cbsas_2015))

head(cbsas_2015)
cbsas_2015 <- cbsas_2015 %>% select(c("CBSAFP", cbsa_name = NAME, geometry))
#xwalk join: cts-cbsas
cbsa_joined <- xwalks::get.spatial.overlap(cts, cbsas_2015,
                                           "geoid", "CBSAFP")

cbsa_joined_backup <- cbsa_joined

colnames(cbsa_joined)
head(cbsa_joined %>% select(contains("geoid")))

# counties: ---------------------------------------------------------------
head(cts)
st_crs(cts)
cts <- st_as_sf(cts) %>% st_transform(st_crs(counties_2018))

head(counties_2018)
counties_2018 <- counties_2018 %>% select(c("GEOID", county_name = NAME, geometry))

county_joined <- xwalks::get.spatial.overlap(cts, counties_2018,
                                             "geoid", "GEOID")


# checking output ----------------------------------------------------------

county_joined %>%
  data.frame() %>%
  count(geoid, GEOID) %>% summary()

cbsa_joined %>%
  data.frame() %>%
  count(geoid, CBSAFP) %>% summary()


# convert both xwalks to sparse tables & save ------------------------------------
county_joined %>%
  data.frame() %>%
  select(ct_id = geoid,
         county_id = GEOID ) %>%
  write.csv(file = "./data/county_xwalk.csv")

cbsa_joined %>%
  data.frame() %>%
  select(ct_id = geoid,
         cbsa_id = CBSAFP ) %>%
  write.csv(file = "./data/cbsa_xwalk.csv")

# check saved
read.csv(file = "./data/county_xwalk.csv") %>% head()
read.csv(file = "./data/cbsa_xwalk.csv") %>% head()

# also save to xwalks package
'read.csv(file = "./data/county_xwalk.csv") %>%
  write.csv(file = "../xwalks/data/county_xwalk_.csv",
           row.names=FALSE)
read.csv(file = "./data/cbsa_xwalk.csv") %>%
  write.csv(file = "../xwalks/data/cbsa_xwalk_.csv",
            row.names = FALSE)
'
# (don't save as rds:)
'
write.csv(cbsa_joined, file = "./data/cbsa_xwalk.csv")
write.csv(county_joined, file = "./data/county_xwalk.csv")
'
