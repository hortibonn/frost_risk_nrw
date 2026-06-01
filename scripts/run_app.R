cmd_args <- commandArgs(trailingOnly = FALSE)
file_arg <- cmd_args[grepl("^--file=", cmd_args)]
script_path <- if (length(file_arg) > 0) {
  sub("^--file=", "", file_arg[[1]])
} else {
  "app/scripts/run_app.R"
}
script_path <- normalizePath(script_path, winslash = "/", mustWork = TRUE)

shiny::runApp(
  appDir = normalizePath(file.path(dirname(script_path), ".."), winslash = "/", mustWork = TRUE),
  host = "127.0.0.1",
  port = 3838,
  launch.browser = FALSE
)


setwd("D:/OneDrive_backup/spatial_data_nrw_maps")



files <- list.files(path = "app/data/layers/frost_risk/", full.names = T)
rast(files[1])


library(terra)

path <- "app/data/layers/frost_risk/"

files <- list.files(
  path = path,
  pattern = "\\.tif$",
  full.names = TRUE
)

max_check <- do.call(rbind, lapply(files, function(f) {
  r <- rast(f)
  
  data.frame(
    file = basename(f),
    max_frost_damage_risk = global(r, "max", na.rm = TRUE)[1, 1]
  )
}))

max_check <- max_check[order(-max_check$max_frost_damage_risk), ]

print(max_check, row.names = FALSE)

highest <- max_check[1, ]
highest















