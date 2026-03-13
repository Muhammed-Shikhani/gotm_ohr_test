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
})


library(here)

root_dir <- here()
#root_dir <- "/home/muhammed/Projects/Mews_Data_Ohra/"
gotm_dir <- file.path(root_dir, "GOTM/GOTM_obs_hydrology_ERA5_4groups")
r_dir <- file.path(gotm_dir, "R_scripts/")
obs_dir <- file.path(gotm_dir, "obs_data/profiles/")
vertmean_obs_dir <- file.path(gotm_dir, "obs_data/vert_means/")
diagnostic_dir <- file.path(gotm_dir, "diagnostics/")
# Receive output_dir from diagnoser.R
args <- commandArgs(trailingOnly = TRUE)
output_dir <- args[1]
rds_dir <- file.path(output_dir, "GOTM_Output_RDS")
plot_dir <- file.path(output_dir, "wholelake/")
indv_dir <- file.path(output_dir, "wholelake/ind_vars/")

dir.create(plot_dir)
dir.create(indv_dir)

setwd(gotm_dir)

obs_rds <- readRDS(file.path(vertmean_obs_dir,"obs_vertmeans_wholelake.rds"))
#sim_rds <- readRDS(file.path(rds_dir,"output_mod_wholelake.RDS"))
obs_phyto <- read.table(file.path(obs_dir, "Phytoplankton_mmol-per-m3_obs.dat"), sep="\t", header = T)
obs_zoo <- read.table(file.path(obs_dir, "Zooplankton_mmol-per-m3_obs.dat"), sep="\t", header = T)

zoo_long <- data.frame(variable="zooplankton_c"  , Date=obs_zoo$Date, layer=as.factor("0 m to bottom"), mean_value=obs_zoo$Biomass_mmol_per_m3, data_type="obs")

phyto_long <-expand.grid(variable=c(c("Stephanodiscus_c", "Nitzschia_c", "Planktothrix_c", "Limnothrix_c", "phytoplankton_c" )) , 
Date=unique(obs_phyto$Date), layer=as.factor("0 m to bottom"), mean_value=NA ,data_type="obs")



obs_long <- bind_rows(obs_rds, .id = "variable")
sim_long <- read.table(file.path(rds_dir, "output_mod_wholelake.dat"), sep="\t", header = T)
sim_long$Date <- as.Date(sim_long$Date)
phyto_sum <- sim_long %>%
  filter(variable %in% c("Stephanodiscus_c", "Nitzschia_c", "Planktothrix_c", "Limnothrix_c")) %>%
  group_by(Date, layer) %>%                   
  summarise(mean_value = sum(mean_value, na.rm = TRUE), .groups = "drop") %>%
  mutate(variable = "phytoplankton_c") %>% mutate(data_type = "mod")


sim_long <- bind_rows(sim_long, phyto_sum)
obs_long <- rbind(obs_long,phyto_long, zoo_long)

#######################



layers <- c("0 m to bottom")

all_vars <- unique(sim_long$variable)
nonplankton_vars <- c("Temp", "O2", "NO3", "NH4", "TP", "PO4-P","Chlorophyll-a" )
plankton_vars <- c( c("Chlorophyll-a"),unique(all_vars)[!unique(all_vars)%in% nonplankton_vars])

nonplankton_units <- c( "°C", "mmol m⁻³","mmol m⁻³","mmol m⁻³","mmol m⁻³","mmol m⁻³","mg m⁻³" )
plankton_units <-  c("mg m⁻³" , "mmol C m⁻³","mmol C m⁻³","mmol C m⁻³","mmol C m⁻³","mmol C m⁻³","mmol C m⁻³")

for(i in 1:length(layers)){
  
  sim_nonplankton <- sim_long %>% filter(variable %in% nonplankton_vars)  %>% filter(layer == layers[i])
  obs_layer <- obs_long %>% filter(layer == layers[i])
  
  data_all <- rbind(sim_nonplankton, obs_layer)
  
  
  p_nutrients <- list()
  
  for (v in 1:length(nonplankton_vars)) {
    dat <- filter(data_all, variable ==nonplankton_vars[v])
    
    p_nutrients[[v]] <- ggplot(dat, aes(x = Date, y = mean_value, group = data_type, color = data_type)) +
      geom_line(data  = filter(dat, data_type == "mod"), linewidth = 0.8) +
      geom_point(data = filter(dat, data_type == "obs"), size = 1.5, alpha = 0.7) +
      scale_color_manual(values = c("obs" = "#E63946", "mod" = "#457B9D"),
                         labels = c("obs" = "Observed", "mod" = "Modelled")) +
      labs(y = nonplankton_units[v], x = NULL, title = nonplankton_vars[v])+
      theme_bw() + 
      theme(plot.title = element_text(margin = margin(t = 10, b = -20)))
    
    ggsave(filename = paste0( indv_dir ,paste0(nonplankton_vars[v], layers[i], ".png")),
           plot = p_nutrients[[v]] ,width = 12, height = 10, dpi = 150)
  }
  
  p_layer <- annotate_figure(
    ggarrange(plotlist = p_nutrients, ncol = 1, common.legend = TRUE, legend = "none"),
    top = text_grob(paste("Biogeochemical Variables at", layers[i]), face = "bold", size = 14)
  )
  
  ggsave(filename = paste0(plot_dir, paste0("Biogeochemical_vars_", layers[i], ".png")),
         plot = p_layer, width = 12, height = 15, dpi = 150, bg = "white")
  
  
  sim_plankton <- sim_long %>% filter(variable %in% plankton_vars)  %>% filter(layer == layers[i])
  obs_layer <- obs_long %>% filter(layer == layers[i])
  
  data_all <- rbind(sim_plankton, obs_layer)
  p_plankton <- list()
  
  for (v in 1:length(plankton_vars)) {
    dat <- filter(data_all, variable ==plankton_vars[v])
    
    p_plankton[[v]] <-  ggplot(dat, aes(x = Date, y = mean_value, group = data_type, color = data_type)) +
      geom_line(data  = filter(dat, data_type == "mod"), linewidth = 0.8) +
      geom_point(data = filter(dat, data_type == "obs"), size = 1.5, alpha = 0.7) +
      scale_color_manual(values = c("obs" = "#E63946", "mod" = "#457B9D"),
                         labels = c("obs" = "Observed", "mod" = "Modelled")) +
      labs(y = plankton_units[v], x = NULL, title = plankton_vars[v])+
      theme_bw() + 
      theme(plot.title = element_text(margin = margin(t = 10, b = -20)), legend.position = "none")
    
    if(plankton_vars[v]=="Chlorophyll-a"){
      next
    }else{
      ggsave(filename = paste0(indv_dir, paste0(plankton_vars[v], "_",layers[i], ".png")),
             plot = p_plankton[[v]] ,width = 12, height = 10, dpi = 150)
    }
    
    
  }
  
  p_layer <- annotate_figure(
    ggarrange(plotlist = p_plankton, ncol = 1, common.legend = TRUE, legend = "none"),
    top = text_grob(paste("Plankton Variables at", layers[i]), face = "bold", size = 14)
  )
  
  ggsave(filename = paste0(plot_dir, paste0("Plankton_vars_", layers[i], ".png")),
         plot = p_layer, width = 12, height = 15, dpi = 150, bg = "white")
  
  
}

