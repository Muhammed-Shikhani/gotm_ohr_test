rm(list = ls())
graphics.off()
Sys.setenv(TZ = "UTC")
start_time <- Sys.time()
suppressPackageStartupMessages({
  
  library(dplyr)
  library(ggplot2)
  library(lubridate)
  library(ncdf4)
  library(ggpubr)
  library(tidyr)
  library(gotmtools)
  
})


library(here)

root_dir <- here()
root_dir <- "/home/muhammed/Projects/MEWS/"
gotm_dir <- file.path(root_dir, "GOTM_Ohra_4groups")
r_dir <- file.path(gotm_dir, "R_scripts/")
obs_dir <- file.path(gotm_dir, "obs_data/profiles/")
vertmean_obs_dir <- file.path(gotm_dir, "obs_data/vert_means/")
diagnostic_dir <- file.path(gotm_dir, "diagnostics/")
# Receive output_dir from diagnoser.R
args <- commandArgs(trailingOnly = TRUE)
output_dir <- args[1]

setwd(gotm_dir)


ncdf <- "output.nc"
out_file <- ncdf

met_vars <- c( "u10" , "cloud" , "airt", "airp", "hum",  'I_0')

met_data <- data.frame()
for(m in met_vars){
  
  met <- get_vari(ncdf = out_file, var = m)
  met$vat <- m
  
  met_data <- rbind(met_data, met)
}

unit_labels <- c(
  "u10"   = "m/s",
  "cloud" = "-",
  "airt"  = "°C",
  "airp"  = "Pa",
  "hum"   = "%",
  "I_0"   = "W/m²"
)

met_data$unit <- unit_labels[met_data$vat]
met_data$facet_label <- paste0(met_data$vat, " (", met_data$unit, ")")

p_met <- 
  ggplot(met_data, aes(x = time, y = var1)) +
  geom_line(color = "#457B9D", linewidth = 0.6) +
  facet_wrap(.~facet_label, ncol = 1, scales = "free_y") +
  labs(x = NULL, y = NULL) +
  theme_classic() +
  theme(
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold")
  )

ggsave(filename = paste0( output_dir,"/", "plot_met.png"),
       plot =  p_met, width = 12, height = 15, dpi = 150, bg = "white")
