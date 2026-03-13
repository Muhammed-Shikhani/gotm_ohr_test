rm(list = ls())
graphics.off()
Sys.setenv(TZ = "UTC")
start_time <- Sys.time()
suppressPackageStartupMessages({
library(dplyr)
library(ggplot2)
library(lubridate)
library(ncdf4)
library(gotmtools)
library(patchwork)
library(tidyr)
})

library(here)

root_dir <- here()
#root_dir <- "/home/muhammed/Projects/Mews_Data_Ohra/"
gotm_dir <- file.path(root_dir, "GOTM/GOTM_obs_hydrology_ERA5_4groups")
data_dir <- file.path(gotm_dir, "obs_data/lake_info/")
r_dir <- file.path(gotm_dir, "R_scripts/")
diagnostic_dir <- file.path(gotm_dir, "diagnostics/")

# Receive output_dir from diagnoser.R
args <- commandArgs(trailingOnly = TRUE)
output_dir <- args[1]
rds_dir <- file.path(output_dir, "GOTM_Output_RDS")
dir.create(rds_dir)

setwd(gotm_dir)


source(file.path(r_dir, "vertmean_functions.R"))


bathy <- read.csv(
  file = file.path(data_dir ,"ohra_bathymetric_curve.csv"),
  header = TRUE
)[, c(1, 2,4)]
names(bathy) <- c("elevation", "volume", "area")
bathy$volume <- bathy$volume * 1e6

ncdf <- "output.nc"
out_file <- ncdf
the_years <- c(2008:2024)
#indv_year <- 2019  # For the 2018-only plot

vars <- c(
  "temp", "selmaprotbas_o2", "selmaprotbas_nn",   "selmaprotbas_aa",  
  "total_phosphorus_calculator_result", "selmaprotbas_po",
  "total_chlorophyll_calculator_result","Stephanodiscus_c",
  "Nitzschia_c", "Planktothrix_c", "Limnothrix_c", "zooplankton_c"
)

var_names <- c(  "Temp" , "O2" , "NO3"  , "NH4" , "TP" ,  "PO4-P"  , "Chlorophyll-a" ,
                 "Stephanodiscus_c","Nitzschia_c", "Planktothrix_c", "Limnothrix_c", "zooplankton_c")

z <- get_vari(ncdf = out_file, var = 'z')
h <- get_vari(ncdf = out_file, var = 'h')
zeta_offset <- 525
zeta <- get_vari(ncdf = out_file, var = "zeta")
zeta$wl <- zeta$var1 + zeta_offset
zeta <- zeta[year(zeta$time) %in% the_years, ]

results_3layers <- list()
results_2layers <- list()
results_wholelake <- list()

for(v in 1:length(vars)){
  
  var_mat <- get_vari(ncdf = out_file, var = vars[v])
  
  ###############################

  va_mod_3layers <- vertmean_vec(
    VAR_MAT = var_mat, Z = z, init_level = 525,
    upr = 11, lwr = 16,
    bathy_elevation = bathy$elevation,
    bathy_area = bathy$area,
    wl = zeta$var1,
    h = h[, 2]
  )
  

  
  v_mod_3layers_long <- va_mod_3layers %>%
    pivot_longer(
      cols = c(top, mid, bot),
      names_to = "layer",
      values_to = "mean_value") %>%
    mutate(
      data_type = "mod",
      layer = factor(layer,
                     levels = c("top", "mid", "bot"),
                     labels = c("0-11 m", "11-16 m", "16 m to bottom"))
    )
  
  names(v_mod_3layers_long)[1] <- "Date"
  results_3layers[[v]] <-  v_mod_3layers_long
  names( results_3layers)[v] <- var_names[v]
  ########################
  
  va_mod_2layers <- vertmean_vec_2layers(
    VAR_MAT = var_mat, Z = z, init_level = 525,
    threshold = 20,
    bathy_elevation = bathy$elevation,
    bathy_area = bathy$area,
    wl = zeta$var1,
    h = h[, 2]
  )
  
  v_mod_2layers_long <- va_mod_2layers %>%
    pivot_longer(cols = c(top,  bot), names_to = "layer", values_to = "mean_value")%>%
    mutate(
      data_type = "mod",
      layer = factor(layer,
                     levels = c("top", "bot"),
                     labels = c("0-20 m",  "20 m to bottom"))
    )
  

  names(v_mod_2layers_long)[1] <- "Date"
  results_2layers[[v]] <-  v_mod_2layers_long
  names( results_2layers)[v] <- var_names[v]

 ##################
  
  va_mod_lake <- vertmean_vec_wholelake(
    VAR_MAT = var_mat, Z = z, init_level = 525,
    bathy_elevation = bathy$elevation,
    bathy_area = bathy$area,
    wl = zeta$var1,
    h = h[, 2]
  )
  
  v_mod_lake_long <-  va_mod_lake %>%
    pivot_longer(cols = c(wholelake), names_to = "layer", values_to = "mean_value")%>%
    mutate(
      data_type = "mod",
      layer = factor(layer,
                     levels = c("wholelake"),
                     labels = c("0 m to bottom"))
    )
  names(v_mod_lake_long)[1] <- "Date"
  results_wholelake[[v]] <-  v_mod_lake_long
  names( results_wholelake)[v] <- var_names[v]

  
}

# sim_long_3layers <- bind_rows(results_3layers, .id = "variable")
# write.table(sim_long_3layers,file.path("output_mod_3layers_11_16m.dat"), sep = "\t", quote = F, row.names = F)
# 
# sim_long_2layers <- bind_rows(results_wholelake, .id = "variable")
# write.table(sim_long_2layers,file.path("output_mod_2layers_20m.dat"), sep = "\t", quote = F, row.names = F)
# 
# sim_long_wholelake <- bind_rows(results_wholelake,.id = "variable")
# write.table(sim_long_wholelake,file.path("output_mod_wholelake.dat"), sep = "\t", quote = F, row.names = F)



saveRDS(results_3layers,file.path(rds_dir, "output_mod_3layers.RDS"))
saveRDS(results_2layers,file.path(rds_dir, "output_mod_2layers.RDS"))
saveRDS(results_wholelake,file.path(rds_dir, "output_mod_wholelake.RDS"))
