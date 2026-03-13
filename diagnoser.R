
library(yaml)

datetime <- format(Sys.time(), "%Y-%m-%d_%H-%M")


root_dir <- here::here()
gotm_dir <- file.path(root_dir, "GOTM/GOTM_obs_hydrology_ERA5_4groups")
r_dir <- file.path(gotm_dir, "R_scripts/")
setwd(gotm_dir)

diagnostic_dir <- file.path(gotm_dir, "diagnostics/")

output_dir <- file.path(diagnostic_dir,datetime)

# rds_dir <- file.path(output_dir, "GOTM_Output_RDS")
# plot_dir_3layers <- file.path(output_dir, "3layers")
# indv_dir_3layers <- file.path(output_dir, "3layers/ind_vars/")
# plot_dir_2layers <- file.path(output_dir, "2layers")
# indv_dir_2layers <- file.path(output_dir, "2layers/ind_vars/")
# plot_dir_wholelake <- file.path(output_dir, "wholelake")
# indv_dir_wholelake <- file.path(output_dir, "wholelake/ind_vars/")

if (!dir.exists(diagnostic_dir)) {
  dir.create(diagnostic_dir)
}
dir.create(output_dir)
# dir.create(rds_dir)
# dir.create(plot_dir_3layers)
# dir.create(indv_dir_3layers)
# dir.create(plot_dir_2layers)
# dir.create(indv_dir_2layers)
# dir.create(plot_dir_wholelake)
# dir.create(indv_dir_wholelake)

file.copy(from = file.path(gotm_dir, "fabm.yaml"), to= file.path(output_dir, "fabm.yaml") )

library(yaml)



fabm_org <- read_yaml("fabm_org.yaml")
fabm_new <- read_yaml("fabm.yaml")


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

writeLines( output,file.path(output_dir, "fabm_differences.txt"))

#system("Rscript R_scripts/0_change_path.R")

system(paste("Rscript --vanilla R_scripts/1_make_sim_vertmeans.R", output_dir), ignore.stdout = TRUE)
print("Finished Calculating Vertical means")
system(paste("Rscript --vanilla R_scripts/2_3layers_plot.R", output_dir), ignore.stdout = TRUE)
print("Finished plotting at 3 layers 0 11 16 bottom")
system(paste("Rscript --vanilla R_scripts/3_2layers_plot.R", output_dir), ignore.stdout = TRUE)
print("Finished plotting at 2 layers 0 20 bottom")
system(paste("Rscript --vanilla R_scripts/4_wholelake_plot.R", output_dir), ignore.stdout = TRUE)
print("Finished plotting at 1 layer 0  bottom")
print("DONE!!")

