processDataV<-function (d, iso.column, data.column, se.column, obsyear.column, 
          region.column = NULL, source.column = NULL, start.year = NULL, 
          end.year = 2015) 
{
  d <- d %>% group_by_(iso.column) %>% mutate(n_obs = n()) %>% 
    mutate_(n_na = paste0("sum(is.na(", data.column, "))"))
  #if (sum(d$n_obs == d$n_na)) 
  #  stop("Please remove countries with no data.\n ")
  isos <- unique(d[, iso.column])[[1]]
  niso <- length(isos)
  n.c <- (d[!duplicated(d[, iso.column]), "n_obs"])[[1]]
  if (is.null(region.column)) {
    region.c <- rep(1, niso)
    region.lookup <- data.frame(region = "World", region_number = 1)
    nregions <- 1
  }
  else {
    region.c <- as.numeric(as.factor((d[!duplicated(d[, 
                                                      iso.column]), region.column])[[1]]))
    regions <- levels(as.factor((d[!duplicated(d[, iso.column]), 
                                   region.column])[[1]]))
    nregions <- length(unique(region.c))
    region.lookup <- data.frame(region = regions, region_number = 1:nregions)

  }
  if (is.null(source.column)) {
    nsources <- 1
  }
  else {
    nsources <- length(unique(d[, source.column])[[1]])
  }
  y.ci <- matrix(NA, ncol = max(d$n_obs), nrow = niso)
  se.ci <- matrix(NA, ncol = max(d$n_obs), nrow = niso)
  gett.ci <- matrix(NA, ncol = max(d$n_obs), nrow = niso)
  source.ci <- matrix(NA, ncol = max(d$n_obs), nrow = niso)
  for (i in 1:niso) {
    y.ci[i, 1:d$n_obs[d[, iso.column] == isos[i]][1]] <- d[d[, 
                                                             iso.column] == isos[i], data.column][[1]]
    se.ci[i, 1:d$n_obs[d[, iso.column] == isos[i]][1]] <- d[d[, 
                                                              iso.column] == isos[i], se.column][[1]]
    gett.ci[i, 1:d$n_obs[d[, iso.column] == isos[i]][1]] <- d[d[, 
                                                                iso.column] == isos[i], obsyear.column][[1]]
    if (is.null(source.column)) {
      source.ci[i, 1:d$n_obs[d[, iso.column] == isos[i]][1]] <- 1
    }
    else {
      source.ci[i, 1:d$n_obs[d[, iso.column] == isos[i]][1]] <- d[d[, 
                                                                    iso.column] == isos[i], source.column][[1]]
    }
  }
  if (is.null(start.year)) {
    startyear.c <- apply(gett.ci, 1, min, na.rm = T)
  }
  else {
    startyear.c <- rep(start.year, niso)
  }
  nyears.c <- sapply(1:length(startyear.c), function(i) length(startyear.c[i]:end.year))
  return(list(y.ci = y.ci, se.ci = se.ci, gett.ci = gett.ci, 
              source.ci = source.ci, isos = isos, niso = niso, n.c = n.c, 
              region.c = region.c, nregions = nregions, region.lookup = region.lookup, 
              nsources = nsources, startyear.c = startyear.c, nyears.c = nyears.c))
}
