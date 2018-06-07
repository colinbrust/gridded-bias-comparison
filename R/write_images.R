library(magrittr)
library(foreach)
library(parallel)

vars    <- c("tmax", "tmin", "ppt")
time    <- c("Monthly", "Seasonal", "Annual")
stat    <- c("Normal", "SD")
dev     <- c(TRUE, FALSE)
annTime <- "01"
seaTime <- c("01", "02", "03", "04")
monTime <- c("01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12")

expand.grid(vars, "Annual", stat, annTime, dev, stringsAsFactors = F) %>%
  tibble::as_tibble() %>%
  dplyr::rename(var = Var1, time = Var2, stat = Var3, timeID = Var4,
                dev = Var5)-> annTable

expand.grid(vars, "Seasonal", stat, seaTime, dev, stringsAsFactors = F) %>%
  tibble::as_tibble() %>%
  dplyr::rename(var = Var1, time = Var2, stat = Var3, timeID = Var4,
                dev = Var5)-> seaTable

expand.grid(vars, "Monthly", stat, monTime, dev, stringsAsFactors = F) %>%
  tibble::as_tibble()%>%
  dplyr::rename(var = Var1, time = Var2, stat = Var3, timeID = Var4,
                dev = Var5)-> monTable

write_map <- function(dat) {

  hexFile <- "./analysis/data/raw_data/shapefiles/hexFile.shp" %>%
    sf::read_sf() %>%
    dplyr::mutate("PointID" = as.character(ORIG_FID))

  cl <- parallel::makeCluster(parallel::detectCores() - 2)
  doParallel::registerDoParallel(cl)

  foreach(i = 1:nrow(dat)) %dopar% {

    source("./R/make_map.R")

    make_map(dat$var[i], dat$time[i], dat$stat[i], dat$timeID[i],
             dat$dev[i], hexFile)

  }

  stopCluster(cl)

}

write_map(annTable)
write_map(seaTable)
write_map(monTable)

#### Save box, density, and correlation plots ####
library(magrittr)
library(foreach)
library(parallel)

vars    <- c("tmax", "tmin", "ppt")
time    <- c("Monthly", "Seasonal", "Annual")
stat    <- c("Normal", "SD")

expand.grid(vars, time, stat, stringsAsFactors = F) %>%
  tibble::as_tibble() %>%
  dplyr::rename(var = Var1, time = Var2, stat = Var3)-> plotTable

bpF <- c("./R/make_boxplots.R", "make_boxplots")
bpT <- c("./R/make_dev_boxplots.R", "make_dev_boxplots")
dpF <- c("./R/make_den_plots.R", "make_den_plots")
dpT <- c("./R/make_dev_den_plots.R", "make_dev_den_plots")
cor <- c("./R/make_cor_plots.R", "make_cor_plots")


write_plot <- function(dat, pFun) {

  cl <- parallel::makeCluster(parallel::detectCores() - 2)
  doParallel::registerDoParallel(cl)

  foreach(i = 1:nrow(dat)) %dopar% {

    source(pFun[1])

    FUN <- match.fun(pFun[2])
    FUN(dat$var[i], dat$time[i], dat$stat[i])

  }

  stopCluster(cl)

}

write_plot(plotTable, bpF)
write_plot(plotTable, bpT)
write_plot(plotTable, dpF)
write_plot(plotTable, dpT)
write_plot(plotTable, cor)

#### Save Elevation Plots ####
library(magrittr)
library(foreach)
library(parallel)

vars    <- c("tmax", "tmin", "ppt")
stat    <- c("Normal", "SD")
CD      <- c("SOUTHWESTERN", "SOUTH CENTRAL", "SOUTHEASTERN", "WESTERN",
             "CENTRAL", "NORTHEASTERN", "NORTH CENTRAL")
dev     <- c(TRUE, FALSE)
annTime <- "01"
seaTime <- c("01", "02", "03", "04")
monTime <- c("01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12")

expand.grid(vars, "Annual", stat, annTime, CD, dev, stringsAsFactors = F) %>%
  tibble::as_tibble() %>%
  dplyr::rename(var = Var1, time = Var2, stat = Var3, timeID = Var4,
                CD = Var5, dev = Var6)-> annTable

expand.grid(vars, "Seasonal", stat, seaTime, CD, dev, stringsAsFactors = F) %>%
  tibble::as_tibble() %>%
  dplyr::rename(var = Var1, time = Var2, stat = Var3, timeID = Var4,
                CD = Var5, dev = Var6)-> seaTable

expand.grid(vars, "Monthly", stat, monTime, CD, dev, stringsAsFactors = F) %>%
  tibble::as_tibble()%>%
  dplyr::rename(var = Var1, time = Var2, stat = Var3, timeID = Var4,
                CD = Var5, dev = Var6)-> monTable

write_elev <- function(dat) {

  cl <- parallel::makeCluster(parallel::detectCores() - 2)
  doParallel::registerDoParallel(cl)

  foreach(i = 1:nrow(dat)) %dopar% {

    source("./R/make_elev_CD_plots.R")

    make_elev_CD_plots(dat$var[i], dat$time[i], dat$stat[i], dat$timeID[i],
             dat$CD[i], dat$dev[i])

  }

  stopCluster(cl)

}

write_elev(annTable)
write_elev(seaTable)
write_elev(monTable)
