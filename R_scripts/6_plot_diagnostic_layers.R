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
  library(RColorBrewer)
})


library(here)

#root_dir <- here()
root_dir <- "/home/muhammed/Projects/MEWS/"
gotm_dir <- file.path(root_dir, "GOTM_Ohra_4groups")
r_dir <- file.path(gotm_dir, "R_scripts/")
obs_dir <- file.path(gotm_dir, "obs_data/profiles/")
vertmean_obs_dir <- file.path(gotm_dir, "obs_data/vert_means/")
diagnostic_dir <- file.path(gotm_dir, "diagnostics/")

# Receive output_dir from diagnoser.R
args <- commandArgs(trailingOnly = TRUE)
output_dir <- args[1]
rds_dir <- file.path(output_dir, "GOTM_Output_RDS")



setwd(gotm_dir)

obs_rds <- readRDS(file.path(vertmean_obs_dir,"obs_vertmeans_3layers_11_16.rds"))
#sim_rds <- readRDS(file.path(rds_dir, "output_mod_3layers.RDS"))
obs_phyto <- read.table(file.path(obs_dir, "Phytoplankton_mmol-per-m3_obs.dat"), sep="\t", header = T) %>%
  filter(Type %in% c( "Phytoplankton_total"  ))

obs_zoo <- read.table(file.path(obs_dir, "Zooplankton_mmol-per-m3_obs.dat"), sep="\t", header = T)

zoo_long <- data.frame(variable="zooplankton_c"  , Date=obs_zoo$Date, layer=as.factor("0 m to bottom"), mean_value=obs_zoo$Biomass_mmol_per_m3, data_type="obs")


phyto_long_top  <- obs_phyto %>%
  mutate(Type = case_match(Type,
                           "Phytoplankton_total"   ~ "phytoplankton_c",
                           .default = Type
  )) %>% mutate(data_type="obs", layer= as.factor("0-20 m")) %>%
  rename("mean_value"="Biomass_mmolC_per_m3", "variable"="Type"  )

phyto_long_bottom <- expand.grid(variable=c("phytoplankton_c") , 
                                 Date=unique(obs_phyto$Date), layer=as.factor (c( "20 m to bottom")), mean_value=NA ,data_type="obs")

phyto_long <- rbind(phyto_long_top, phyto_long_bottom)

obs_long <- bind_rows(obs_rds, .id = "variable")

sim_long <- read.table(file.path(rds_dir, "output_mod_3layers_11_16m.dat"), sep="\t", header = T)  %>% filter(variable != "zooplankton_c")
sim_long$Date <- as.Date(sim_long$Date)


sim_long_2layers <- read.table(file.path(rds_dir, "output_mod_2layers_20m.dat"), sep="\t", header = T)  %>% filter(variable != "zooplankton_c")
sim_long_2layers$Date <- as.Date(sim_long_2layers$Date)
phyto_sum <- sim_long_2layers %>%
  filter(variable %in% c("Stephanodiscus_c", "Nitzschia_c", "Planktothrix_c", "Limnothrix_c")) %>%
  group_by(Date, layer) %>%                    # add any other grouping columns you have
  summarise(mean_value = sum(mean_value, na.rm = TRUE), .groups = "drop") %>%
  mutate(variable = "phytoplankton_c") %>% mutate(data_type = "mod")

sim_long_lake <- read.table(file.path(rds_dir, "output_mod_wholelake.dat"), sep="\t", header = T)
sim_long_lake$Date <- as.Date(sim_long_lake$Date)

sim_zoo <- sim_long_lake %>% filter(variable == "zooplankton_c")


sim_long <- bind_rows(sim_long, phyto_sum, sim_zoo)
obs_long <- rbind(obs_long,phyto_long, zoo_long)


#######################


layers <- c("0-11 m", "11-16 m", "16 m to bottom")

all_vars <- unique(sim_long$variable)
nonplankton_vars <- c("Temp", "O2", "NO3", "NH4", "TP", "PO4-P","Chlorophyll-a" )
#plankton_vars <- c( c("Chlorophyll-a"),unique(all_vars)[!unique(all_vars)%in% nonplankton_vars])
plankton_vars  <- c("Chlorophyll-a" , "Stephanodiscus_c" ,"Nitzschia_c" ,"Planktothrix_c","Limnothrix_c" ,"phytoplankton_c" ,"zooplankton_c"  )
phyto_vars <- c("Stephanodiscus_c" ,"Nitzschia_c" ,"Planktothrix_c","Limnothrix_c")

nonplankton_units <- c( "°C", "mmol m⁻³","mmol m⁻³","mmol m⁻³","mmol m⁻³","mmol m⁻³","mg m⁻³" )
plankton_units <-  c("mg m⁻³" , "mmol C m⁻³","mmol C m⁻³","mmol C m⁻³","mmol C m⁻³","mmol C m⁻³","mmol C m⁻³")



p_layer <- list()

for(i in 1:length(layers)){
  
  sim_nonplankton <- sim_long %>% filter(variable %in% nonplankton_vars)  %>% filter(layer == layers[i])
  obs_layer <- obs_long %>% filter(layer == layers[i])
  
  data_all <- rbind(sim_nonplankton, obs_layer)
  
  p_nutrients <- list()
  
  for (v in 1:length(nonplankton_vars)) {
    dat <- filter(data_all, variable ==nonplankton_vars[v])
    
    p_nutrients[[v]] <- ggplot(dat, aes(x = Date, y = mean_value, group = data_type, color = data_type)) +
      geom_line(data  = filter(dat, data_type == "mod"), linewidth = 1) +
      geom_point(data = filter(dat, data_type == "obs"), size = 1.5, alpha = 0.7) +
      scale_color_manual(values = c("obs" = "#E63946", "mod" = "#457B9D"),
                         labels = c("obs" = "Observed", "mod" = "Modelled")) +
      labs(y = nonplankton_units[v], x = NULL, title = nonplankton_vars[v])+
      theme_bw() + 
      theme(plot.title = element_text(margin = margin(t = 10, b = -20)))
    

    
    p_nutrients[[v]] <-p_nutrients[[v]] +
      theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())
  }
  
  p_biogeochem_layer <- annotate_figure(
    ggarrange(plotlist = p_nutrients, ncol = 1, common.legend = TRUE, legend = "none"),
    top = text_grob(paste("Biogeochemical Variables at", layers[i]), face = "bold", size = 14))
  
  p1 <-   ggarrange(plotlist = p_nutrients, ncol = 1, common.legend = F, legend = "none")
  
  
  
  sim_phyto<- sim_long %>% filter(variable %in% phyto_vars)  %>% filter(layer == layers[i])
  
  p_plankton <- list()
  
  p_phyto <- ggplot(sim_phyto, aes(x = Date, y = mean_value, group = variable, color = variable)) +
    geom_line(linewidth = 1) +
    scale_color_brewer(palette = "Set2") +
    labs(y = "mmol C m⁻³", x = NULL, title = "Phytoplankton Groups", color = NULL) +
    theme_bw() +
    theme(
      #axis.text.x = element_blank(), axis.ticks.x = element_blank(),
          legend.position = c(0.5, 0.95),
          legend.direction = "horizontal",
          legend.background = element_rect(fill = alpha("white", 0.7), color = NA),
          legend.text = element_text(size = 9),
          panel.grid.minor = element_blank(),
          plot.title = element_text(margin = margin(t = 9, b = -20,l =550) , size = 12)
    )
  
  
  p_layer[[i]] <- annotate_figure(
    ggarrange(p1,p_phyto, ncol = 1,
              heights = c(7, 1.2, 1, 1)),
    top = text_grob(paste(layers[i]), face = "bold", size = 14))
  
  
}

p_3layers <- ggarrange(plotlist =p_layer, ncol = 3, common.legend = F, legend = "none")


p_phyto_top <- ggplot(data = filter(sim_long, variable == "phytoplankton_c", layer == "0-20 m"),
                      aes(x = Date, y = mean_value, group = data_type, color = data_type)) +
  geom_line() +
  geom_point(data = filter(obs_long, variable == "phytoplankton_c", layer == "0-20 m")) +
  scale_color_manual(values = c("obs" = "#E63946", "mod" = "#457B9D"),
                     labels = c("obs" = "Observed", "mod" = "Modelled")) +
  labs(y = "mmol C m⁻³", x = NULL, title = "Total phytoplankton (0-20 m)") +
  theme_bw() +
  theme(plot.title = element_text(margin = margin(t = 10, b = -20)),
        legend.position = "none")



p_zoo <-ggplot(data = filter(sim_long, variable == "zooplankton_c"),
               aes(x = Date, y = mean_value, group = data_type, color = data_type)) +
  geom_line() +
  geom_point(data = filter(obs_long, variable == "zooplankton_c") )+
  scale_color_manual(values = c("obs" = "#E63946", "mod" = "#457B9D"),
                     labels = c("obs" = "Observed", "mod" = "Modelled")) +
  labs(y =  "mmol C m⁻³", x = NULL, title = "zooplankton_c (0-bottom)")+
  theme_bw() + 
  theme(plot.title = element_text(margin = margin(t = 10, b = -20)), 
        legend.position = "none")


p_bottom <- ggarrange(p_phyto_top, p_zoo, ncol = 2,
                      common.legend = FALSE, legend = "none")

p_bottom_centered <- ggarrange(NULL, p_bottom, NULL,
                               ncol = 3, widths = c(0.5, 2, 0.5))

p_all <- ggarrange(p_3layers, p_bottom_centered,
                   ncol = 1, heights = c(8, 1.7),
                   common.legend = FALSE, legend = "none")

ggsave(filename = paste0( output_dir, "/", "plot_diagnostics.png"),
       plot =  p_all, width = 30, height = 35, dpi = 450, bg = "white")
