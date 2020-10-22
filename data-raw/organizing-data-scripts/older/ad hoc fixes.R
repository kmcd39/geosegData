# fix for when i accidentally over-wrote cbsa output
csvs <- list.files(paste0(output.dir,
                          "cbsa-weighted"))

csvs
csvs[2:8]

cbsas.measures <- lapply(csvs[2:8],
                         function(f) fread(paste0(output.dir, 
                                            "cbsa-weighted/",
                                            f)))

cbsas.measures <- 
  cbsas.measures %>%
  bind_rows() %>%
  select(-V1)

cbsas.measures %>% write.csv(file = paste0(output.dir,
                                           "cbsa-weighted/",
                                           "collected-measures.csv"))
