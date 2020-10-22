#rm(list = ls())

suppressMessages(library(sf))
suppressMessages(library(dplyr))

# get small-area life expectancy data -------------------------------------
# from https://www.cdc.gov/nchs/nvss/usaleep/usaleep.html
# data dictionary: https://www.cdc.gov/nchs/data/nvss/usaleep/Record_Layout_CensusTract_Life_Expectancy.pdf
lifexp <- data.table::fread("https://ftp.cdc.gov/pub/Health_Statistics/NCHS/Datasets/NVSS/USALEEP/CSV/US_A.CSV")
# Note all identifier colnames refer to 2010 FIPS codes

# make colnames easier & convert to character
colnames(lifexp) <- make.names(colnames(lifexp))
lifexp$Tract.ID <- as.character(lifexp$Tract.ID)

# data qual checks --------------------------------------------------------
# in map, i noticed this var was missing for all of wisconsin; i check here to see what might be going on
# (it's also missing for maine)
lifexp %>%
  filter(STATE2KX == 55)
lifexp %>%
  filter(STATE2KX == 23)

lifexp %>%
  filter(strtrim(Tract.ID, 2) == "23")
lifexp %>%
  filter(strtrim(Tract.ID, 2) == "55")

lifexp %>%
  arrange(desc(Tract.ID))

# add wisconsin back in ---------------------------------------------------
# however, they have by-state tables also listed and include wisconsin and maine.
wisc <- data.table::fread("https://ftp.cdc.gov/pub/Health_Statistics/NCHS/Datasets/NVSS/USALEEP/CSV/WI_A.CSV")

colnames(wisc) <- make.names(colnames(wisc))
wisc$Tract.ID <- as.character(wisc$Tract.ID)

lifexp %>% count(STATE2KX) %>% data.frame()
lifexp <- bind_rows(lifexp,
                    wisc)
lifexp %>% count(STATE2KX) %>% data.frame() # :)) there it is.

# add maine back in -------------------------------------------------------
maine <- data.table::fread("https://ftp.cdc.gov/pub/Health_Statistics/NCHS/Datasets/NVSS/USALEEP/CSV/ME_A.CSV")

colnames(maine) <- make.names(colnames(maine))
maine$Tract.ID <- as.character(maine$Tract.ID)

lifexp %>% count(STATE2KX) %>% data.frame()
lifexp <- bind_rows(lifexp,
                    maine)
lifexp %>% count(STATE2KX) %>% data.frame() # :)) there it is.


# are random rows missing for other states in natl table? -----------------
# other states sometimes missing a lot of random rows I think--- checking if state tables include any CTs that are left out of natl table
'ca <-
  fread("https://ftp.cdc.gov/pub/Health_Statistics/NCHS/Datasets/NVSS/USALEEP/CSV/CA_A.CSV")
colnames(ca) <- make.names(colnames(ca))
ca$Tract.ID <- as.character(ca$Tract.ID)

lifexp[STATE2KX == 6, ] %>% nrow()
  full_join(ca) %>%
  filter()'

# NOPE, not the case

rm(maine, wisc)
