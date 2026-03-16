old_path <- 'root_dir <- "/home/muhammed/Projects/MEWS/"'
#new_path <- 'root_dir <- "C:/Tom/.../Mews_Data_Ohra/"' this is the path for you tom all the way to the gir folder
new_path <- here::here()


r_files <- list.files("R scripts", pattern = "\\.R$", full.names = TRUE)

for (file in r_files) {
  content <- readLines(file)
  updated <- gsub(old_path, new_path, content, fixed = TRUE)
  writeLines(updated, file)
}