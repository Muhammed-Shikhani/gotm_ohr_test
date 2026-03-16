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
inflow_dir <- file.path(gotm_dir, "input")
setwd(gotm_dir)


inflow_files <- list.files(inflow_dir, pattern = "Inflow")

stream_names <- gsub("Inflow_|\\.dat", "", inflow_files )

p_inflow <- list()
for(f in 1: length( inflow_files)){
  dat <- read.table(file.path(inflow_dir, inflow_files[f]), sep= "\t", header = T )[,c(1,2,5,6)]
  names(dat)[1] <- "date"
  dat$date <- as.Date(dat$date)
  dat_long <- pivot_longer(data = dat, names_to = "var", values_to = "value", cols = c("Q","PO4_P","TP") )
  dat_long$var <- factor(dat_long$var, levels = c("Q", "TP", "PO4_P"))
  
  p_inflow[[f]] <- ggplot(dat_long, aes(x = date, y = value)) +
    geom_line(color = "#457B9D", linewidth = 0.8) +
    facet_wrap(.~var, nrow = 1, scales = "free_y") +
    labs(x = NULL, y = NULL, title = stream_names [f]) +
    theme_bw() + 
    theme(plot.title = element_text(margin = margin(t = 10, b = -20)))
  
  
  if( f != length(inflow_files)){
    p_inflow[[f]] <- p_inflow[[f]] +
      theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())
  }

}
p_qp <- ggarrange(plotlist = p_inflow, ncol = 1, common.legend = TRUE, legend = "none")

ggsave(filename = paste0( output_dir,"/", "plot_QP.png"),
       plot =  p_qp, width = 20, height = 15, dpi = 150, bg = "white")
