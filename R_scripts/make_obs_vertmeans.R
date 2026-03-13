rm(list = ls())
graphics.off()
Sys.setenv(TZ = "UTC")
start_time <- Sys.time()
#setwd("/home/muhammed/Projects/Mews_Data_Ohra/GOTM/GOTM_ERA5_newSelmaprotbas_spinup_newgotm/")

library(dplyr)
library(ggplot2)
library(lubridate)
library(marelac)
library(tidyr)
library(here)

root_dir <- here()
#root_dir <- "/home/muhammed/Projects/Mews_Data_Ohra/"
gotm_dir <- file.path(root_dir, "GOTM/GOTM_obs_hydrology_ERA5_4groups")
obs_dir <- file.path(gotm_dir, "obs_data/profiles/")
data_dir <- file.path(gotm_dir, "obs_data/lake_info/")
vertmean_obs_dir <- file.path(gotm_dir, "obs_data/vert_means/")
plots_dir <- file.path(root_dir, "Output_plots/Volume_means_obs/")
setwd(gotm_dir)


bathy <- read.csv(
  file = file.path(data_dir ,"ohra_bathymetric_curve.csv"),
  header = TRUE
)[, c(1, 2,4)]
names(bathy) <- c("elevation", "volume", "area")
bathy$volume <- bathy$volume * 1e6


volume_func <- function(z, level) {
  zz <- level - z  # Height from bottom
  if (any(zz < 0)) stop("Error: Depth exceeds maximum depth.")
  vol <- approx(x = bathy$elevation, y = bathy$volume, xout = zz, rule = c(1,1))$y
  return(vol)
}




the_years <- c(2008:2024)

water_balance <-  read.csv( file = file.path(data_dir ,"Ohra_water_balance_2008_2024_corrected.csv"))
water_volume <- water_balance[,which(names(water_balance) %in% c("date", "Vol"))]
water_volume$Vol <- water_volume$Vol * 1e6

# water_level <- read.csv( file = file.path(data_dir ,"water_level.csv"))
# water_level$elev <- bathy[1, "elevation"] + water_level$water_level # TS: we need the elevation of the water surface

water_level <- data.frame(date= as.Date(water_volume$date), elev=approx(x = bathy$volume, y =  bathy$elevation, xout = water_volume$Vol, method = "linear")$y)

obs_all_files <- list.files(obs_dir)
obs_all_files <- obs_all_files[!grepl("plankton|DOC", obs_all_files, ignore.case = TRUE)]

vars <- unlist(lapply(obs_all_files, FUN = function(x) unlist(strsplit(x, split = "_"))[2]))
the_units <- unlist(lapply(obs_all_files, FUN = function(x) unlist(strsplit(x, split = "_"))[3]))

#the_units <- c("mg_m3-1", "mmol_m3-1", "mmol_m3-1", "mmol_m3-1", "mmol_m3-1", "°C", "mmol_m3-1")

obs_means <- list()

for(f in 1:length(obs_all_files)){
  
  obs_file <- paste0(obs_dir, obs_all_files[f]) 
  
  
  obs_data<- read.table(
    file = obs_file,
    sep = "\t",
    header = FALSE
  )
  names(obs_data) <- c("date", "depth", "value")
  obs_data$date <- as.Date(obs_data$date)
#  obs_data$depth <- -1 * obs_data$depth  # Convert to positive depth from surface
#  obs_data<- obs_data[year(obs_data$date)%in% the_years,]
  
  
  unique_obs_dates <- unique(obs_data$date)
  
  ####################################
  
  results <- data.frame(
    Date = character(),
    layer = character(),
    mean_value = numeric(),
    stringsAsFactors = FALSE,
    data_type = character()
  )
  
  
  
  
  for (i in 1:length( unique_obs_dates) ){
    current_date <- unique_obs_dates[i]
    df <- obs_data[which(obs_data$date == current_date), ]
    
    wl_df <- water_level[which(as.Date(water_level$date) == as.Date(current_date)), ]
    
    layers <- list(
      list(top = 0,   bot = 11,   label = "0-11 m"),
      list(top = 11,  bot = 16,   label = "11-16 m"),
      list(top = 16,  bot = max(df$depth), label = "16 m to bottom")
    )
    
    for (layer in layers) {
      mean_val <- vertmean(
        depth = df$depth,
        vari = df$value,
        level = wl_df$elev, #TS: this is correct
        top = layer$top,
        bot = layer$bot,
        vol = volume_func
      )
      
      results <- rbind(
        results,
        data.frame(
          Date = as.character(current_date),
          layer = layer$label,
          mean_value = mean_val,
          stringsAsFactors = FALSE,
          data_type = "obs"
          
        )
      )
    }
  }
  results$Date <- ymd(results$Date)
  results$layer <- as.factor(results$layer)
  obs_means[[f]] <- results
  names(obs_means)[f]  <- vars[f]
  
  
}

#first run visual inspection showed some temp and o2 values at 9, i assume that is an error in the vertmean output, i will just write them into NA
for(nn in c(  "O2"  ,"Temp" )){
  obs_means[nn][[1]]$mean_value[which(obs_means[nn][[1]]$mean_value == 0)] <- NA
  
}

obs_long <- bind_rows(obs_means, .id = "variable")
write.table(obs_long,file.path(vertmean_obs_dir,"obs_vertmeans_3layers_11_16m.dat"), sep = "\t", quote = F, row.names = F)
saveRDS(obs_means, file.path(vertmean_obs_dir,"obs_vertmeans_3layers_11_16.rds"))




obs_means_2layers <- list()

for(f in 1:length(obs_all_files)){
  
  obs_file <- paste0(obs_dir, obs_all_files[f]) 
  
  
  obs_data<- read.table(
    file = obs_file,
    sep = "\t",
    header = FALSE
  )
  names(obs_data) <- c("date", "depth", "value")
  obs_data$date <- as.Date(obs_data$date)
  #  obs_data$depth <- -1 * obs_data$depth  # Convert to positive depth from surface
  #  obs_data<- obs_data[year(obs_data$date)%in% the_years,]
  
  
  unique_obs_dates <- unique(obs_data$date)
  
  ####################################
  
  results <- data.frame(
    date = character(),
    layer = character(),
    mean_value = numeric(),
    stringsAsFactors = FALSE,
    data_type = character()
  )
  
  
  
  
  for (i in 1:length( unique_obs_dates) ){
    current_date <- unique_obs_dates[i]
    df <- obs_data[which(obs_data$date == current_date), ]
    
    wl_df <- water_level[which(as.Date(water_level$date) == as.Date(current_date)), ]
    
    layers <- list(
      list(top = 0,   bot = 20,   label = "0-20 m"),
      list(top = 20,  bot = max(df$depth), label = "20 m to bottom")
    )
    
    for (layer in layers) {
      mean_val <- vertmean(
        depth = df$depth,
        vari = df$value,
        level = wl_df$elev, #TS: this is correct
        top = layer$top,
        bot = layer$bot,
        vol = volume_func
      )
      
      results <- rbind(
        results,
        data.frame(
          Date = as.character(current_date),
          layer = layer$label,
          mean_value = mean_val,
          stringsAsFactors = FALSE,
          data_type = "obs"
          
        )
      )
    }
  }
  results$Date <- ymd(results$Date)
  results$layer <- as.factor(results$layer)
  obs_means_2layers[[f]] <- results
  names(obs_means_2layers)[f]  <- vars[f]
  
  
}
#first run visual inspection showed some temp and o2 values at 9, i assume that is an error in the vertmean output, i will just write them into NA

for(nn in c(  "O2"  ,"Temp" )){
  obs_means_2layers[nn][[1]]$mean_value[which(obs_means_2layers[nn][[1]]$mean_value == 0)] <- NA
  
}

obs_long_2layers <- bind_rows(obs_means_2layers, .id = "variable")
write.table(obs_long_2layers,file.path(vertmean_obs_dir,"obs_vertmeans_2layers_20m.dat"), sep = "\t", quote = F, row.names = F)
saveRDS(obs_means_2layers, file.path(vertmean_obs_dir,"obs_vertmeans_2layers_20m.rds"))



obs_means_lake <- list()

for(f in 1:length(obs_all_files)){
  
  obs_file <- paste0(obs_dir, obs_all_files[f]) 
  
  
  obs_data<- read.table(
    file = obs_file,
    sep = "\t",
    header = FALSE
  )
  names(obs_data) <- c("date", "depth", "value")
  obs_data$date <- as.Date(obs_data$date)
  #  obs_data$depth <- -1 * obs_data$depth  # Convert to positive depth from surface
  #  obs_data<- obs_data[year(obs_data$date)%in% the_years,]
  
  
  unique_obs_dates <- unique(obs_data$date)
  
  ####################################
  
  results <- data.frame(
    date = character(),
    layer = character(),
    mean_value = numeric(),
    stringsAsFactors = FALSE,
    data_type = character()
  )
  
  
  
  
  for (i in 1:length( unique_obs_dates) ){
    current_date <- unique_obs_dates[i]
    df <- obs_data[which(obs_data$date == current_date), ]
    
    wl_df <- water_level[which(as.Date(water_level$date) == as.Date(current_date)), ]
    
    layers <- list(
      list(top = 0,  bot = max(df$depth), label = "0 m to bottom")
    )
    
    for (layer in layers) {
      mean_val <- vertmean(
        depth = df$depth,
        vari = df$value,
        level = wl_df$elev, #TS: this is correct
        top = layer$top,
        bot = layer$bot,
        vol = volume_func
      )
      
      results <- rbind(
        results,
        data.frame(
          Date = as.character(current_date),
          layer = layer$label,
          mean_value = mean_val,
          stringsAsFactors = FALSE,
          data_type = "obs"
          
        )
      )
    }
  }
  results$Date <- ymd(results$Date)
  results$layer <- as.factor(results$layer)
  obs_means_lake[[f]] <- results
  names(obs_means_lake)[f]  <- vars[f]
  
  
}
#first run visual inspection showed some temp and o2 values at 9, i assume that is an error in the vertmean output, i will just write them into NA

for(nn in c(  "O2"  ,"Temp" )){
  obs_means_lake[nn][[1]]$mean_value[which(obs_means_lake[nn][[1]]$mean_value == 0)] <- NA
  
}

obs_long_lake <- bind_rows(obs_means_lake, .id = "variable")
write.table(obs_long_lake,file.path(vertmean_obs_dir,"obs_vertmeans_wholelake.dat"), sep = "\t", quote = F, row.names = F)

saveRDS(obs_means_lake, file.path(vertmean_obs_dir,"obs_vertmeans_wholelake.rds"))






for(j in 1:length(vars)){
  
  obs_means[[j]]$layer <- factor(
    obs_means[[j]]$layer,
    levels = c("0-11 m", "11-16 m", "16 m to bottom")
  )
  
  
  
  obs_means[[j]]$date <- as.Date(obs_means[[j]]$date)
  
  p_facets <- ggplot() +
    geom_point(
      data = obs_means[[j]],
      aes(x = as.Date(date), y = mean_value),
      color = "red", size = 1.5, na.rm = TRUE
    ) +
    facet_wrap(~ layer, ncol = 1, scales = "free_y") +
    labs(
      title = vars[j],
      x = NULL,
      y = paste(vars[j], the_units[j], " ")
    ) +
    theme_bw(base_size = 12) +
    theme(
      legend.position = "none",
      axis.text.x = element_text(angle = 45, hjust = 1),
      plot.title = element_text(hjust = 0.5, face = "bold")
    )
  
  ggsave(
    filename =file.path(plots_dir,paste0("3layers_11_16/",vars[j],"_3layers_11_16.png"))  ,
    plot = p_facets,
    width = 10,
    height = 8,
    dpi = 300
  )  
}





for(j in 1:length(vars)){
  
  obs_means_2layers[[j]]$layer <- factor(
    obs_means_2layers[[j]]$layer,
    levels = c("0-20 m", "20 m to bottom")
  )
  
  
  
  obs_means_2layers[[j]]$date <- as.Date(obs_means_2layers[[j]]$date)
  
  p_facets <- ggplot() +
    geom_point(
      data = obs_means_2layers[[j]],
      aes(x = as.Date(date), y = mean_value),
      color = "red", size = 1.5, na.rm = TRUE
    ) +
    facet_wrap(~ layer, ncol = 1, scales = "free_y") +
    labs(
      title = vars[j],
      x = NULL,
      y = paste(vars[j], the_units[j], " ")
    ) +
    theme_bw(base_size = 12) +
    theme(
      legend.position = "none",
      axis.text.x = element_text(angle = 45, hjust = 1),
      plot.title = element_text(hjust = 0.5, face = "bold")
    )
  
  ggsave(
    filename =file.path(plots_dir,paste0("2layers_20/",vars[j],"_2layers_20.png"))  ,
    plot = p_facets,
    width = 10,
    height = 8,
    dpi = 300
  )  
}





for(j in 1:length(vars)){
  
  obs_means_lake[[j]]$layer <- factor(
    obs_means_lake[[j]]$layer,
    levels = c("0 to bottom")
  )
  
  
  
  obs_means_lake[[j]]$date <- as.Date(obs_means_lake[[j]]$date)
  
  p_facets <- ggplot() +
    geom_point(
      data = obs_means_lake[[j]],
      aes(x = as.Date(date), y = mean_value),
      color = "red", size = 1.5, na.rm = TRUE
    ) +
    facet_wrap(~ layer, ncol = 1, scales = "free_y") +
    labs(
      title = vars[j],
      x = NULL,
      y = paste(vars[j], the_units[j], " ")
    ) +
    theme_bw(base_size = 12) +
    theme(
      legend.position = "none",
      axis.text.x = element_text(angle = 45, hjust = 1),
      plot.title = element_text(hjust = 0.5, face = "bold")
    )
  
  ggsave(
    filename =file.path(plots_dir,paste0("whole_lake_average/",vars[j],"_whole_lake_average.png"))  ,
    plot = p_facets,
    width = 10,
    height = 8,
    dpi = 300
  )  
}
