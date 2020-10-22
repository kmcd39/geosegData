
rm(list = ls())
# dyn.load("/usr/local/gdal/2.2.4/lib64/libgdal.so.20.3.3") # necessary to do the spatial work thru server
suppressMessages(library(sf))
suppressMessages(library(dplyr))
suppressMessages(library(data.table))


# add centroid geometries -------------------------------------------------------
#centroids <- st_read("/scratch/network/km31/2010 CTs/US_tract_2010_prepped-w-centroids.shp")

#centroids <- data.frame(centroids)
#head(centroids)

#cts <- full_join(cts, centroids,
#          by = c("geoid" = "GISJOIN"))

# checks ------------------------------------------------------------------
cts = readRDS(file = "data-raw/Intermediate save/cts.RDS")
cts %>% summary()

colnames(cts)

cts %>%
  filter_at(c(8:27),
            ~all(is.na(.)))

# add variances -----------------------------------------------------------

# define variables for which to gen variances
colnames(cts)
# variance variables :p
var.vars <- cts %>% 
  select(contains("_mean") &
           !contains("_se")) %>%
  colnames()
var.vars

# back-calc variances
for (v in var.vars ){
  #print(paste("adding variance for",v))
  cts <- geoseg::back_calculate_variance(cts, v)
}


# set row_id column -----------------------------------------------------
cts$row.id <- seq(1:nrow(cts))

# check for duplicates and weird issues before saving ---------------------
cts %>%
  count(geoid) %>%
  arrange(desc(n)) %>%
  tibble()

' it no longer does this, i think b/c i cleaned up how the geoids were managed.
Using census ones instead of nhgis maybe fixed?
# hm; it has two different county ids and is otherwise the same..
# alaska always being weird
cts[cts$geoid %in% c("G0201950000200", "G0201950000200"), ]
# one row matched with county 198; the other with 195. THe alaska county-equivalents were changed since the 2010 census is why. I just use opp-insights county
cts <- cts[!(cts$geoid == "G0201950000200" & cts$county_id == 2198), ]
'
# are there other places where opp insights and other county column don't match?
(cts<-tibble(cts))



# check if opp insights cz-xwalk matches mine/walkerke's -----------------------
ct2cz = xwalks::ctx %>% select(1:6)

# whoops i have an extra leading zero in a couple col's; fix that
cts[,c(1:4,6:7)] %>% arrange(geoid)
ct2cz %>% arrange(geoid)
cts$countyfp = as.numeric(cts$countyfp) %>% stringr::str_pad(3, "left", "0")
cts$tractce = as.numeric(cts$tractce) %>% stringr::str_pad(6, "left", "0")
cts$geoid = paste0(cts$statefp,
                   cts$countyfp,
                   cts$tractce)

cts$gisjoin %>% nchar() %>% unique()

abvcts = cts[,c(1:4,6:7)] %>% arrange(geoid)
ct2cz = ct2cz %>% arrange(geoid)

ct2cz[!ct2cz$geoid %in% abvcts$geoid, ] %>%
  filter(statefp == "36") %>%
  left_join(divDat::cts) %>% st_sf() %>%
  mapview::mapview(zcol="cz")


arsenal::comparedf( 
  (cts[,c(1:4,6:7)] %>% arrange(geoid)),
  (ct2cz%>% arrange(geoid))) %>%
  summary()

abvcts[(1297:1306),]
ct2cz[(1297:1306),]
# re-arrange and save -----------------------------------------------------
head(cts)
colnames(cts)

cts <- cts %>%
  select(c(1:6,
           27,28,row_id,
           7:26, 36:38,
           c("lon", "lat")))
summary(cts)

write.csv(cts,
          file = "data/fully-prepped.csv",
          row.names = FALSE)

#usethis::use_data(cts, overwrite = TRUE)
# what about the places with no shapefile match? --------------------------
appHelpers::state.index

cts %>%
  filter(is.na(id)) %>%
  #count(state,county)%>% #View()
  filter(state==15) %>% select(c(1:5))
state.index
# i checked ariizona & hawaii against census bureau reference maps: https://www.census.gov/geographies/reference-maps/2010/geo/2010-census-tract-maps.html
# For arizona, cts were totally missing; for hawaii they existed entirely over water.

# taking a look at tribal census tracts... it does look like generic census tracts also existed over tribal land
'library(appHelpers)
tribal_cts <- tigris::tribal_census_tracts(year = 2011, class = "sf")
states <- tigris::states(class = "sf")
library(ggplot2)
ggplot(tribal_cts) +
  geom_sf() +
  geom_sf(data = states[as.numeric(states$STATEFP) %in% lower_48, ],
          color = "#88AA55",
          fill = NA)'
