#rm(list = ls())

suppressMessages(library(sf))
suppressMessages(library(dplyr))
suppressMessages(library(tidyr))

# nhgis dir ---------------------------------------------------------------
# HH by income bucket & median & pov status
nhgis_income_dir <- "./inst/extdata/nhgis/income and pov status TS/"

income_ts <- data.table::fread( file = paste0(nhgis_income_dir,
                                       "nhgis0014_ts_nominal_tract.csv"))

# (descriptive column names are in codebook in same folder with the data)
head(income_ts, 2)
glimpse(income_ts)
id_cols <- c("STATEFP", "COUNTYFP", "TRACTA", "NHGISCODE")
# keep only median HH income and Persons by Poverty Status
# B79AA125: 2008-2012: Median income in previous year: Households
# AX7AA125: 2008-2012: Persons: Poverty status is determined ~ Income below poverty level
income <- income_ts %>%
  select(c(id_cols,
           "B79AA125",
           "AX7AA125"))


# function to ensure proper format ----------------------------------------
# adds back in leading 0s and leaves as.character for id columns.
# changing to numeric risks scientific notation and false negatives matching id cols for merge.
reformat_nhgis <- function(df) {
  # formats geo id columns as per https://www.nhgis.org/user-resources/geographic-crosswalks
  df %>%
    mutate(STATEFP = stringr::str_pad(STATEFP, width=2, side="left", pad="0"),
           COUNTYFP = stringr::str_pad(COUNTYFP, width=4, side="left", pad="0"),
           TRACTA = stringr::str_pad(TRACTA, width=7, side="left", pad="0"))
}
reformat_nhgis(income) %>% select(id_cols) %>% head()
income <- reformat_nhgis(income)
# population, race, & household count -------------------------------------
# note this table uses 2010 census figures rather than 2008-2012 acs
nhgis_pophhr_dir <- "./inst/extdata/nhgis/race-hh-population/"

demo_ts <- data.table::fread( file = paste0(nhgis_pophhr_dir,
                                            "nhgis0015_ts_nominal_tract.csv"))

head(demo_ts, 2)
demo <- demo_ts %>%
  select(c(id_cols,
           contains("2010")))

demo <- demo %>%
  rename(population = B78AA2010,
         hh = AR5AA2010)

demo <- reformat_nhgis(demo)
# employment & commuting --------------------------------------------------

nhgis_emply_dir <- "./inst/extdata/nhgis/employment and commuting; 1970-2012 TS/"

emply_ts <- data.table::fread( file = paste0(nhgis_emply_dir,
                                 "nhgis0002_ts_nominal_tract.csv"))
fullnames <- emply_ts[1, ]
emply_ts <- emply_ts[2:nrow(emply_ts), ]

emply <- emply_ts %>%
  select(c(id_cols,
           "B84AA125",  # total in labor force
           "B84AF125",  # total not in labor force
           "B84AD125",  # In labor force / employed
           "B84AE125")) # In labor force / UNemployed

# add jobless #
emply <- emply %>%
  mutate_at(c(1:3, 5:8), as.numeric) %>%
  mutate(persons.jobless =
           B84AF125 + B84AE125)

emply <- reformat_nhgis(emply)

# check tables ------------------------------------------------------------
# id columns across the three nhgis tables:
library(data.table)
head(demo[, ..id_cols])
head(income[, ..id_cols])
head(emply[, ..id_cols])
# non-na rows per table:
purrr::map(list(demo, income, emply), ~nrow(na.omit(.)))

# number of non-na CT identifiers in each df range from 73036 - 74002
# (tables with more rows do have extra information in those rows.
# see for example, demographics table has 18 rows with information for which information is missing in the income table:
demo[!(demo$NHGISCODE %in% income$NHGISCODE), ] %>% filter_at(c(7:11), any_vars(!is.na(.)))
# ...but most of the na's co-align btwn tables:
demo[!(demo$NHGISCODE %in% income$NHGISCODE), ] %>% filter_at(c(7:11), all_vars(is.na(.)))

# merge all ---------------------------------------------------------------
# NOTE-- I use JUST NHGISCODE to merge. this concatenates STATE, COUNTY, & TRACT--- but these sometimes load in different formats
# it's better to just use the one unique character identifier than trying to get all of the others into the same format
drop_extra_ids <- function(df) {
  df %>%
    select(-c(setdiff(id_cols, "NHGISCODE")))
}

allnh <- full_join(demo, drop_extra_ids(income),
            by = "NHGISCODE")# id_cols)

allnh <- full_join(allnh, drop_extra_ids(emply),
                   by = "NHGISCODE")# id_cols)
# (example of how tract causes mis-merge: some loaded in scientific notation)
#allnh %>% head()
#  filter(TRACTA != TRACTA.y|
#           TRACTA != TRACTA.x) %>%
#  select(contains("TRACTA"))
#allnh <- full_join(allnh, pop,
#                   by = c("NHGISCODE" = "GISJOIN", "TRACTA", "STATEFP" = "STATEA", "COUNTYFP" = "COUNTYA"))

# to check for scientific notation in tract ids:
# allnh[grepl("e+", allnh$TRACTA), ]

# at the end of the merge, does this kind of filter so that census tracts that ONLY
# have NAs are trimmed (I suspect airports and parks and stuff we dont' want to map
# as NAs) I'm leaving in for now rows where for which not all variables have NAs:
allnh %>%
  filter_at(c(7:ncol(allnh)),
            any_vars(!is.na(.)))
# Rows for which ALL vars are NAs: (37,000 of them)
allnh %>%
  filter_at(c(7:ncol(allnh)),
            all_vars(is.na(.)))
# ( i think these are tract identifiers for pre-2010... )

# add calculated variables ------------------------------------------------

# from demo:
# B18AA2010:   2010: Persons: White (single race)
# B18AB2010:   2010: Persons: Black or African American (single race)
# B18AC2010:   2010: Persons: American Indian and Alaska Native (single race)
# B18AD2010:   2010: Persons: Asian and Pacific Islander and Other Race (single race)
# B18AE2010:   2010: Persons: Two or More Races
# from pop:
# CL8AA2010:   2010: Persons: Total
# CM4AA2010:   2010: Households: Total
# from emply:
# B84AA125: 2008-2012: Persons: 16 years and over ~ In labor force
# B84AF125: 2008-2012: Persons: 16 years and over ~ Not in labor force
# B84AD125: 2008-2012: Persons: 16 years and over ~ In labor force--Civilian--Employed
# B84AE125: 2008-2012: Persons: 16 years and over ~ In labor force--Civilian--Unemployed
# from income:
# B79AA125: 2008-2012: Median income in previous year: Households
# AX7AA125: 2008-2012: Persons: Poverty status is determined ~ Income below poverty level

# a check to make sure they add up / I pulled tables correctly (demo.pop shld be
# equal to population; labor pop excludes children, population in poverty should be
# smaller)
allnh %>%
  mutate(demo.pop = B18AA2010 + B18AB2010 + B18AC2010 + B18AD2010 + B18AE2010) %>%
  mutate(labor.pop = B84AA125 + B84AF125) %>%
  select(c(population,demo.pop, labor.pop, povpop = AX7AA125)) %>% head(10)

# add race/ethnicity concentration columns
allnh <- allnh %>%
  rename(hh.median.income = B79AA125,
         povpop = AX7AA125) %>%
  mutate(perc.nonwhite = (population - B18AA2010) / population)

# add LFPR, jobless rate, unemployment rate, and poverty rate
allnh %>% colnames()
allnh <- allnh %>%
  mutate(jobless_rate = persons.jobless / population) %>%
  mutate(unemployment_rate = B84AE125 / B84AA125) %>%
  mutate(lfpr = B84AA125 / population) %>%
  mutate(poverty_rate = povpop / population)

head(allnh)

# drop columns I'm no longer interested in --------------------------------
allnh <- select( allnh,
                 c(1:4,
                   "hh.median.income", "population", "hh",
                   "perc.nonwhite", "jobless_rate", "unemployment_rate",
                   "lfpr", "poverty_rate") )

# (dropping columns for which ALL vars are NAs will happen after everything is merged)
