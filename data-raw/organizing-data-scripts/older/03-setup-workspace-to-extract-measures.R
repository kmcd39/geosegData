# get data ----------------------------------------------------------------
rm(list = ls())
source("00-init-workspace.R")
dat <-  readRDS(file="sparse.RDS")

dat <- dat %>% data.frame() # (convert to dataframe to speed up processing; reconvert when need sf fcns )

summary(dat)
dat$id <- 1:nrow(dat) # (and add rownames)

# add cbsa identifiers ----------------------------------------------------
# for now, taken froml Missouri data center --- 2010 counties to 2015 cbsa areas
# I was trying approach in R to more or less automate (see "add.geotag" first fcn in helper fcn script...
# I was running into problems because sf library uses planar projections for spatial intersections and 
# the US is big enough that it's not straightforward to project everything to planar to do this
# i discovered "geosphere" library which might facillitate flexibly handling crosswalks in R and want to go back to automate xwalk generation
cbsa.crosswalk <-
  read.csv(paste0(data.dir,
                  "mdc crosswalks/geocorr2014.csv"))
cbsa.crosswalk <- cbsa.crosswalk[2:nrow(cbsa.crosswalk),]

glimpse(cbsa.crosswalk)

#count(dat,state) %>% data.frame()
dat <-
  dat %>%
  mutate(state_county =
           paste0(stringr::str_pad(state, 2, side = "left", pad = "0"), # add back in leading 0s and concatenate for merge
                  stringr::str_pad(county, 3, side = "left", pad = "0"))) %>%
  left_join(cbsa.crosswalk[,1:4],
            by = c("state_county" = "county"))


# make region identifiers factors with explicit NAs -----------------------
library(purrr)
dat <-
  dat %>%
  mutate_at(c("cbsa","cz"),
            ~forcats::fct_explicit_na(factor(.), na_level = "Missing"))

#dat <- dat %>% mutate(cbsa = trimws(as.character(cbsa)))

source("0X-extract-all-centroids.R")

# list of variables for which to retrieve inequality measures
ineq.vars <- colnames(dat)[c(6,8,11,14,18)]

save.image(file = "setup-w-centroids.Rdata")
