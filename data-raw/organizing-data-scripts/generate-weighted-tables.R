source("00-init-workspace.R")

weigh.by.vars <- c(ineq.vars,
                   "kfr_26_pooled_pooled_mean_variance",
                   "kfr_29_pooled_pooled_mean_variance")

wcz <- all.regional.weighted(dat, "cz", weigh.by.vars)

wcbsa <- all.regional.weighted(dat, "cbsa", weigh.by.vars, add.extra = F)

colnames(wcz)
colnames(wcbsa)

wdat <- left_join(wcz , wcbsa,
                  by = "id")


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
          na.omit(old.wdat$kfr_pooled_pooled_p25.cz_weighted))

'saveRDS(wdat,
        file = paste0(data.dir,
                      "weighted-dfs.RDS"))
'
# some additional checks --------------------------------------------------
# (more checks in the formal 'checks' folder)
'
no.matches <-
  wczt[wczt$kfr_pooled_pooled_p25.cz_weighted !=
  wdat[wdat$id %in% wczt$id, ]$kfr_pooled_pooled_p25.cz_weighted, ]$id

wczt[wczt$id %in% no.matches, ]$kfr_pooled_pooled_p25.cz_weighted - 
  wdat[wdat$id %in% no.matches, ]$kfr_pooled_pooled_p25.cz_weighted


wczt[, c("cz", "tract", "kfr_26_pooled_pooled_mean_variance.cz_weighted")] %>% head()

get.all.weights(test.dat, "kfr_26_pooled_pooled_mean_variance", "cz") %>%
  select(c("cz", "tract", "kfr_26_pooled_pooled_mean_variance.cz_weighted")) %>% head()
     '