# this got complicated due to memory problems dealing w/ giant sp.dataframes
# it is moved here to consolidate
# called from script 3 "setup workspace"



# loop through states for centroid coords to make easier on the RAM --------
states <- unique(dat$state)
crs <- "+proj=longlat +datum=WGS84"

centroid.prep <- function(dat, i) {
  # turns sf geometry into sp format, dropping invalid and empty geometries
  dat %>%
    filter(state == i) %>%
    #filter(id %in% test.cz) %>% 
    st_as_sf() %>%
    st_transform(crs) %>%
    filter(st_is_valid(geometry)) %>%
    filter(!st_is_empty(geometry)) %>%
    select(c(1:5, "id", "geometry")) %>%
    #rmapshaper::ms_simplify() %>% # simplifies polygon boundaries
    as("Spatial") }

spatial.dat <- centroid.prep(dat, i = states[1])

for (i in states[2:length(states)]) {
  to.bind <- centroid.prep(dat, i)
  
  spatial.dat <-
    spatial.dat %>%
    rbind(to.bind)
}

# Get polygon centroids - use geosphere package to deal with spherical polygons (i.e., polygons on globe)
centroids <- as.data.frame(geosphere::centroid(spatial.dat))
colnames(centroids) <- c("lon", "lat") 
centroids$id <- 1:nrow(centroids)
# Create SpatialPointsDataFrame object
#sp::coordinates(centroids) <- c("lon", "lat") 
#sp::proj4string(centroids) <- sp::proj4string(as(spatial.dat, "Spatial"))

# put centroid coords on dataframe
# (these can then be used to construct spatial weights matrices)
dat <-
  dat %>%
  left_join(centroids,
            by = c("id")) 


#saveRDS(dat, file = "full-dat-centroids.RDS")
