rm(list = ls())
dyn.load("/usr/local/gdal/2.2.4/lib64/libgdal.so.20.3.3") # necessary to do the spatial work thru server
suppressMessages(library(sf))
suppressMessages(library(dplyr))
suppressMessages(library(data.table))
suppressMessages(library(tidyr))

# agglomerating data ------------------------------------------------------

'Begin with datasets measuring upward mobility, % residents jailed, life expectancy, violent crime, poverty, 
joblessness at the level of census tracts. All of these except the dataset with violent crime will be measured in 
2010 census tracts. The violent crime dataset us.libes 2000 census tracts. Data are available at: 
Opportunity Insights (adult family income rank for people starting at 25th percentile and % jailed: 
            https://opportunityinsights.org/data/), 
CDC (life expectancy: https://www.cdc.gov/nchs/nvss/usaleep/usaleep.html), 
and poverty and joblessness from the American Community Survey or the census long form (for these measures it would be good to have measures from 1990, 2000, and 2010).
'

# helper fcn: move first row to colnames ----------------------------------------------
colnames.from.row <- function(dat) {
  ret <- 
    dat[2:nrow(dat),]
  
  colnames(ret) <- 
    (dat[1,] %>% unname() %>% as.character())
  
  return(ret)
}


# set directory for local data --------------------------------------------
# when possible, data is downloaded through URL or api; some is kept locally
data.dir <- "/scratch/network/km31/"


# get small-area life expectancy data -------------------------------------
# from https://www.cdc.gov/nchs/nvss/usaleep/usaleep.html 
# data dictionary: https://www.cdc.gov/nchs/data/nvss/usaleep/Record_Layout_CensusTract_Life_Expectancy.pdf 
lifexp <-
  fread("https://ftp.cdc.gov/pub/Health_Statistics/NCHS/Datasets/NVSS/USALEEP/CSV/US_A.CSV")


# get opp insights data ---------------------------------------------------
# from https://opportunityinsights.org/data/
# dd: https://opportunityinsights.org/wp-content/uploads/2019/07/Codebook-for-Table-1.pdf

all.outcomes <- # table 4 on opp insights https://opportunityinsights.org/wp-content/uploads/2019/07/Codebook-for-Table-4.pdf 
  fread(paste0(data.dir,
               "tract_outcomes_early.csv"))

neighborhood.info <- # table 9
  fread("https://opportunityinsights.org/wp-content/uploads/2018/10/tract_covariates.csv")
#"https://opportunityinsights.org/wp-content/uploads/2018/12/cty_covariates.csv")


# get census + ACS data ------------------------------------------------------------
# from https://www.nhgis.org/
# for NHGIS API: https://cran.r-project.org/web/packages/ipumsr/index.html

nh_gis_2010 <-
  fread(paste0(data.dir,
               "csv-dat/",
               "nhgis all - 2010 tracts (2006-10 ACS estimates)/",
               "nhgis0010_ds177_20105_2010_tract.csv"))

colnames(nh_gis_2010)

nh.2010 <- colnames.from.row(nh_gis_2010)

colnames(nh.2010)
# using codebook to get column codes for vars I want
to.select <- c(5,6,7,8,11, # geo identifiers & names
               "J43E001", # median FAMILY income in past 12 months
               "J6QE001", # total population
               )
head(nh.2010[, 34])
nh.2010[, 34:50] %>% head()
nh.2010[, 34:50] %>% colnames()

nh.2010[, 11:20] %>% head()

empl <-
  empl %>%
  mutate_at(19:182,
          ~as.numeric(.))

hh.inc <- 
  fread(paste0(data.dir,
               "csv-dat/nhgis households by income/nhgis0003_ts_nominal_tract.csv"))
hh.inc <- colnames.from.row(hh.inc)
# ---
pop <-
  fread(paste0(data.dir,
               "csv-dat/nhgis population data/nhgis0005_ds181_2010_tract.csv"))
pop <- colnames.from.row(pop) %>% rename("population" = Total) %>% select(c(5:11,"population"))

# Notes: for years 2008-2012 (from NHGIS documentation:)
# Source dataset: 2012 American Community Survey: 5-Year Data [2008-2012, Block Groups & Larger Areas]
# Source table: B23025. Employment Status for the Population 16 Years and Over
# Universe: Population 16 years and over
# Variable: [QXS002] In labor force


# installs and vignettes --------------------------------------------------
#install.packages("ipumsr")
#install.packages("totalcensus")
# ipumsr vignette: https://cran.r-project.org/web/packages/ipumsr/vignettes/ipums.html

#vignette("ipums-nhgis", package = "ipumsr")

#dict_acs5_table[grepl("POVERTY", dict_acs5_table$table_name),"table_name"]

