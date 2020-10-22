#rm(list = ls())

source("organizing-data-scripts/01-agglomerating-data.R")
#load("all.RData")

# load state - fp code index ----------------------------------------------
state.fp <- fread(paste0(data.dir,
                         "csv-dat/state_fp_index.csv"))
states <- pull(state.fp,FIPS) #unique(incomeincarc$state)


# start with opp insights data --------------------------------------------
'This table reports predicted outcomes for children by Census tract, race and gender. Each
Census tract is uniquely identified by three identifiers – state, county, and tract (2010 FIPS codes).
The data is organized long on Census tract and wide on race, gender and parent income
percentile rank, so that there is exactly one row per tract, and a variable for each
race/gender/parent income percentile rank combination. We provide data for children born
between 1978 and 1983'

colnames(all.outcomes)

# vars to get from this table:
opp.vars <- c("kfr_pooled_pooled_p25",
              "jail_pooled_pooled_p25",
              "kfr_26_pooled_pooled_mean",
              "kfr_29_pooled_pooled_mean")


# select all vars, plus counts and standard errors
to.select <- c( opp.vars
                , paste0(opp.vars, "_se")
                , "kfr_pooled_pooled_n"
                , "jail_pooled_pooled_n"
                , "kfr_26_pooled_pooled_n"
                , "kfr_29_pooled_pooled_n")

dat <-
  all.outcomes %>%
  select(c(1:3, # geo identifiers
           sort(to.select))) 

head(dat)


# add life expectancy -----------------------------------------------------
dat <-
  dat %>%
  left_join(select(lifexp, -`Tract ID`),
            by = c("state" = "STATE2KX",
                   "county" = "CNTY2KX",
                   "tract" = "TRACT2KX"))

summary(dat)

# helper fcn: cleaning nhgis time series ----------------------------------
clean.ts <- function(dat, start.date = 1990) {
  nhgis.merge.cols <- c("FIPS State Code",
                        "FIPS County Code",
                        "NHGIS Integrated Census Tract Code")
  
  ret <-
    dat %>%
    mutate_at(nhgis.merge.cols, as.numeric) %>%
    mutate_at(c(18:ncol(dat)),
              ~replace(., is.na(.), 0))
  
  ret <-
    ret[(ret$`FIPS State Code` %in% states),]
  # and drop SEs and pre 1990 dates
  ret <-
    cbind(ret[,(1:18)],
          ret[,(tidyr::replace_na(as.numeric(substr(colnames(ret),1,4)),0) >= start.date)]) 
  
  return(ret)
}

# add census and acs tract-level info -------------------------------------
head(empl)
colnames(empl)
colnames(hh.inc)
emply <- clean.ts(empl[1:54], start.date = 1990) #drops se's and data before start date; trims to state selection

#empl[!(as.numeric(empl$`FIPS State Code`) %in% as.numeric(emply$`FIPS State Code`)),(1:18)] 
# dc is dropped, but seemingly not in opp insights data either
#empl$`FIPS State Code`[!(as.numeric(empl$`FIPS State Code`) %in% states)]


colnames(emply)
emply[]
emply[, 14:18] %>% head()

#hh.inc <- clean.ts(hh.inc)
dat <-
  dat %>%
  left_join(emply,
            by = c("state" = "FIPS State Code",
                   "county" = "FIPS County Code",
                   "tract" = "NHGIS Integrated Census Tract Code"))
# add tract populations ---------------------------------------------------
pop <-
  pop %>% 
  mutate_at(c(2,4,7:8),
            as.numeric)
dat <- dat %>%
  left_join(pop,
            by = c("state" = "State Code",
                   "county" = "County Code",
                   "tract" = "Census Tract Code"))

# create trimmed version --------------------------------------------------
# measuring upward mobility, % residents jailed, life expectancy, violent crime, poverty, joblessness
colnames(dat)

trimmed <-
  dat %>%
  select(c(1:10, # locational cols, kfr, jailed, and  count (opp insights)
           74:76, # poverty share (opp insights)
           99:100, # life expectancy
           117:134 # employment & labor force status
           ,139)) # total population
# check merges
#select(dat, c(1:5, "Tract ID", "Area Name, 2012"))
head(trimmed)
# create "jobless" column -------------------------------------------------
colnames(trimmed)
trimmed <-
  data.table(trimmed)[,jobless.2010 := 
                        (pull(trimmed[33]) + pull(trimmed[30])) ]

trimmed <- trimmed[,c(1:15,34:35)]
trimmed[,jobless.perc := jobless.2010 / population]
# bring in shapefiles -----------------------------------------------------
# to save time, use below command rather than re-downloading all the census tracts with the for loop
cts <- readRDS(file = "all-census-tracts.RDS")
'
cts <- tigris::tracts(state=states[1],class = "sf")
for (i in states[2:length(states)]) {
  cts <- rbind(cts, 
               tigris::tracts(state=i,class = "sf"))
}'
#head(cts)
#count(cts, STATEFP)
add.geo <- function(dat) {
  # adds tigris census tract geometry data to a-spatial dataframe with columns for state/county/tract
  left_join(dat,
            (mutate_at(cts,
                       1:4, as.numeric) %>%
               dplyr::select(c(1:4,"geometry"))),
            by = c("state" = "STATEFP",
                   "county" = "COUNTYFP",
                   "tract" = "TRACTCE")) }

dat <- add.geo(dat)

trimmed <- add.geo(trimmed)


# final steps before saving -----------------------------------------------
# neaten colnames for R
colnames(dat) <- make.names(colnames(dat))
colnames(trimmed) <- make.names(colnames(trimmed))

#saveRDS(dat,file="setup.RDS")
saveRDS(trimmed,file="sparse.RDS")
#saveRDS(cts, file="all-census-tracts.RDS")
#write.csv(dat, file = "all-cols.csv")
#write.csv(trimmed, file = "sparse.csv")
          