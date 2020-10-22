#rm(list = ls())
# dyn.load("/usr/local/gdal/2.2.4/lib64/libgdal.so.20.3.3") # necessary to do the spatial work thru server
suppressMessages(library(sf))
suppressMessages(library(dplyr))
suppressMessages(library(data.table))


# ALL CT OUTCOMES from opp insights ---------------------------------------
# I.e., table 4 --
# https://opportunityinsights.org/wp-content/uploads/2019/07/Codebook-for-Table-9.pdf
# note opp insights contains a lot of TS data but for ACS estimates they use
# 5-yr estimates ENDING for given year; i.e., 2006-2010 for 2010 estimates. For
# this reason, I use NHGIS variables for these variables, because the midrange
# estimates are slightly preferable (2008-2012 for 2010 estimates.)


# download, extract, & read opp insights data ----------------------------------

# download & extract compressed .csv from their site:
temp <- tempfile()

# (take a minute to run: )
download.file("https://opportunityinsights.org/wp-content/uploads/2018/10/tract_outcomes.zip"
              ,temp)
opp_cts <- read.csv(unz(temp, "tract_outcomes_early.csv"))

unlink(temp)

tibble(opp_cts)

# drop unneeded cols -----------------------------------------------------------

id_cols <- c("state",  # all areas are 2010 FIPS codes, except 1990 for CZs
             "county",
             "tract",
             "cz", "czname")
library(tidyselect)
opp_cts <- opp_cts %>%
  select(c(all_of(id_cols),
           contains("kfr"),
         contains("jail"))) %>%
  select(c(id_cols,contains("pooled_pooled")))


opp_vars_to_keep <- c("jail_pooled_pooled_mean", "jail_pooled_pooled_n", "jail_pooled_pooled_mean_se",  # mean values for jail and hh income @ age, and relevant cols to back-calculate variances
                      "kfr_29_pooled_pooled_mean", "kfr_29_pooled_pooled_n", "kfr_29_pooled_pooled_mean_se",
                      "kfr_26_pooled_pooled_mean", "kfr_26_pooled_pooled_n", "kfr_26_pooled_pooled_mean_se",
                      "kfr_pooled_pooled_p25", "kfr_pooled_pooled_p75") # and some income vars contingent on parents' income


cols_to_keep <- c(id_cols, opp_vars_to_keep)
opp_cts <- opp_cts %>% select(all_of(cols_to_keep)) %>% tibble()

saveRDS(opp_cts, file = "data-raw/Intermediate save/opp_cts.RDS")

# CZ OUTCOMES from opp insights -------------------------------------------
# get csv directly from their site

opp_czs <- data.table::fread("https://opportunityinsights.org/wp-content/uploads/2018/10/cz_outcomes.csv")

colnames(opp_czs)

cz_to_keep <- c("cz", opp_vars_to_keep)

opp_czs <- opp_czs[ , ..cz_to_keep]
