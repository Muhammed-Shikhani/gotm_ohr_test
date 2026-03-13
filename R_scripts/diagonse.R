
library(yaml)


root_dir <- "/home/muhammed/Projects/Mews_Data_Ohra/"
gotm_dir <- file.path(root_dir, "GOTM/GOTM_obs_hydrology_ERA5_4groups")
r_dir <- file.path(gotm_dir, "R_scripts/")
plot_dir <- file.path(root_dir, "Output_plots/diagnostics/")
setwd(gotm_dir)

datetime <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
output_dir <- file.path(plot_dir,datetime)
dir.create(output_dir)


fabm_org <- read_yaml("fabm_org.yaml")
fabm_new <- read_yaml("fabm.yaml")

library(yaml)

# Recursive function to compare two lists
compare_lists <- function(a, b, path = "") {
  keys_a <- names(a)
  keys_b <- names(b)
  
  # Keys only in a
  only_a <- setdiff(keys_a, keys_b)
  if (length(only_a) > 0)
    cat("Only in original:", paste(path, only_a, sep="/"), "\n")
  
  # Keys only in b
  only_b <- setdiff(keys_b, keys_a)
  if (length(only_b) > 0)
    cat("Only in new:", paste(path, only_b, sep="/"), "\n")
  
  # Compare shared keys
  for (key in intersect(keys_a, keys_b)) {
    current_path <- paste(path, key, sep = "/")
    val_a <- a[[key]]
    val_b <- b[[key]]
    
    if (is.list(val_a) && is.list(val_b)) {
      compare_lists(val_a, val_b, current_path)
    } else if (!identical(val_a, val_b)) {
      cat("DIFFERENCE at", current_path, "\n")
      cat("  Original:", val_a, "\n")
      cat("  New:     ", val_b, "\n")
    }
  }
}


compare_lists(fabm_org, fabm_new)


output <- capture.output({
  cat("FABM Parameter Differences\n")
  cat("Generated:", datetime, "\n")
  cat(strrep("=", 40), "\n\n")
  compare_lists(fabm_org, fabm_new)
})

writeLines(file.path(output_dir, output), "fabm_differences.txt")

system("Rscript R_scripts/0_change_path.R")
system("Rscript R_scripts/1_make_sim_vertmeans.R")
system("Rscript R_scripts/2_3layers_plot.R")
system("Rscript R_scripts/3_2layers_plot.R")
system("Rscript R_scripts/4_wholelake_plot.R")


