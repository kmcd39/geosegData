rm(list = ls())

# this generates population-weighted equivalents for all variables defined in other dataset.
dyn.load("/usr/local/gdal/2.2.4/lib64/libgdal.so.20.3.3") # necessary to do the spatial work thru server
suppressMessages(library(sf))
suppressMessages(library(dplyr))
suppressMessages(library(data.table))

# get fully prepped, non-weighted dataset
dat <-  read.csv(file = "data/fully-prepped.csv")

# define varnames to generate weighted versions of ------------------------
colnames(dat)

weigh.by.vars <- colnames(dat)[c(13:32)]
weigh.by.vars <-  weigh.by.vars[!grepl("(_se$|_n$)", weigh.by.vars)] #get rid of _n's and _se's


# extra columns to keep ---------------------------------------------------
extra.cols <- colnames(dat)[c(1:5,7,8:11,33,34)]


# checks ------------------------------------------------------------------

temp <- purrr::map(weigh.by.vars, ~dplyr::select(geoseg::get.all.weights(dat, ., "cz", "population")
                                         , c(tidyselect::all_of("geoid"),
                                             tidyselect::contains("_weighted"))))
temp_all <- Reduce(function(...) merge(..., by = c("geoid"), all = TRUE),
       temp)

count(temp_all, geoid) %>% arrange(desc(n))
dat[dat$geoid == "G0201950000200", ]
dat[sum(duplicated(dat$geoid)), ]
# end checks --------------------------------------------------------------



# generate weights by region ----------------------------------------------
# now just define function by running script
geoseg::all_regional_weighted

# czs
wcz <- geoseg::all_regional_weighted(dat, "cz", weigh.by.vars, add.extra = extra.cols)
# cbsas
wcbsa <- geoseg::all_regional_weighted(dat, "cbsa_id", weigh.by.vars)
# counties
wcounty <- geoseg::all_regional_weighted(dat, "county_id", weigh.by.vars)

colnames(wcz)
colnames(wcbsa)
colnames(wcounty)

list(wcz, wcbsa, wcounty) %>%
  purrr::map(~summary(.))

dat$cz %>% unique()
dat$cbsa_id %>% unique()
dat$county_id %>% unique()

wdat <- left_join(wcz , wcbsa,
                  by = "geoid")

wdat <- left_join(wdat , wcounty,
                  by = "geoid")

# usethis::use_data(wdat)


'
old.wdat <- readRDS(file = paste0(data.dir,
                                  "weighted-dfs.RDS"))

old.wdat$cz[!(old.wdat$cz %in% wdat$cz)]

old.wdat[is.na(old.wdat$kfr_26_pooled_pooled_mean.cz_weighted), c("id", "cz", "cbsa", "tract")] %>% pull(cz) %>% unique()
wdat[is.na(wdat$kfr_26_pooled_pooled_mean.cz_weighted), c("id", "cz", "cbsa", "tract")] %>% pull(cz) %>% unique()

data.table(wdat)[cz == 38200, c("id", "cz", "czname" )]#, "kfr_26_pooled_pooled_mean", "population")]
data.table(dat)[cz == 38200, c("id", "cz", "czname" )]#, "kfr_26_pooled_pooled_mean", "population")]

wdat[]

summary(old.wdat$kfr_pooled_pooled_p25.cz_weighted)
summary(wdat$kfr_pooled_pooled_p25.cz_weighted)

summary(na.omit(wdat$kfr_pooled_pooled_p25.cz_weighted) -
          na.omit(old.wdat$kfr_pooled_pooled_p25.cz_weighted))'

'saveRDS(wdat,
        file = paste0(data.dir,
                      "weighted-dfs.RDS"))




'
