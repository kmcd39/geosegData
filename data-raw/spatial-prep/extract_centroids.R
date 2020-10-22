dyn.load("/usr/local/gdal/2.2.4/lib64/libgdal.so.20.3.3") # necessary to do the spatial work thru server
suppressMessages(library(sf))
suppressMessages(library(dplyr))
suppressMessages(library(data.table))
suppressMessages(library(tidyr))

# get CT shpfile -- downloaded through NHGIS:
# ( i don't put in project directory because so large )
ct_2010 <- st_read("/scratch/network/km31/2010 CTs/US_tract_2010.shp")

crs <- 4269

ct_2010 <- ct_2010 %>% st_transform(crs)

# check for empties & invalid geos
# sum(!st_is_valid(ct_2010$geometry)) # 44
# sum(st_is_empty(ct_2010$geometry)) # 0

# clean self-intersecting polygons ----------------------------------------
library(lwgeom)
ct_2010 <- st_make_valid(ct_2010)

# trim unwanted columns & add id column -----------------------------------
ct_2010 <- ct_2010 %>%
  mutate(id = 1:nrow(ct_2010)) %>%
  select(c(id, STATEFP10, COUNTYFP10,
           TRACTCE10, GISJOIN, geometry))

# Get polygon centroids - use geosphere package to deal with spherical polygons (i.e., polygons on globe)
centroids <- as.data.frame(geosphere::centroid(as(ct_2010, "Spatial")))
colnames(centroids) <- c("lon", "lat")
centroids$id <- 1:nrow(centroids)


# add centroids and save shapefile ----------------------------------------
ct_2010_shp <- left_join(ct_2010, centroids,
                         by = "id")

# st_write(ct_2010, "/scratch/network/km31/2010 CTs/US_tract_2010_prepped-w-centroids.shp")

