# Prepare small, app-local display layers for the Shiny viewer.
# Run manually from the repository root or from app/:
#   Rscript app/scripts/prepare_app_layers.R
#
# This script copies existing outputs into app/data and creates 1 km display
# GeoTIFFs where source rasters are finer. It does not run any modeling step.

required <- c("terra", "sf")
missing <- required[!vapply(required, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing) > 0) {
  stop("Missing required packages: ", paste(missing, collapse = ", "), call. = FALSE)
}

`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0 || is.na(x)) y else x
}

cmd_args <- commandArgs(trailingOnly = FALSE)
file_arg <- cmd_args[grepl("^--file=", cmd_args)]
script_path <- if (length(file_arg) > 0) {
  sub("^--file=", "", file_arg[[1]])
} else {
  "app/scripts/prepare_app_layers.R"
}
script_path <- normalizePath(script_path, winslash = "/", mustWork = TRUE)
app_dir <- normalizePath(file.path(dirname(script_path), ".."), winslash = "/", mustWork = TRUE)
repo_dir <- normalizePath(file.path(app_dir, ".."), winslash = "/", mustWork = TRUE)

source_paths <- list(
  frost_combined_2050_weighted = file.path(repo_dir, "outputs/gam_surrogate_testing3/frost_damage_risk_gam_Golden_Delicious_cmip6_weighted_2050.tif"),
  frost_historic_2010 = file.path(repo_dir, "outputs/gam_surrogate_testing/frost_damage_risk_gam_Golden_Delicious_historic_2010.tif"),
  frost_combined_2085_weighted = file.path(repo_dir, "outputs/gam_surrogate_testing/frost_damage_risk_gam_Golden_Delicious_cmip6_weighted_2085.tif"),
  predictor_stack = file.path(repo_dir, "data/nrw_predictor_stack.tif"),
  nrw_boundary = file.path(repo_dir, "data/nrw_boundary.gpkg"),
  support_points = file.path(repo_dir, "data/support_points/support_points_50.gpkg")
)

target_paths <- list(
  frost_combined_2050_weighted = file.path(app_dir, "data/layers/frost_risk/frost_damage_risk_gam_Golden_Delicious_cmip6_weighted_2050.tif"),
  frost_historic_2010 = file.path(app_dir, "data/layers/frost_risk/frost_damage_risk_gam_Golden_Delicious_historic_2010.tif"),
  frost_combined_2085_weighted = file.path(app_dir, "data/layers/frost_risk/frost_damage_risk_gam_Golden_Delicious_cmip6_weighted_2085.tif"),
  terrain_elevation = file.path(app_dir, "data/layers/terrain/terrain_elevation.tif"),
  terrain_slope = file.path(app_dir, "data/layers/terrain/terrain_slope.tif"),
  terrain_roughness = file.path(app_dir, "data/layers/terrain/terrain_roughness.tif"),
  terrain_tpi = file.path(app_dir, "data/layers/terrain/terrain_tpi.tif"),
  terrain_eastness = file.path(app_dir, "data/layers/terrain/terrain_eastness.tif"),
  terrain_northness = file.path(app_dir, "data/layers/terrain/terrain_northness.tif"),
  climate_predictor_frost_days = file.path(app_dir, "data/layers/climate_predictors/climate_predictor_frost_days.tif"),
  climate_predictor_forcing = file.path(app_dir, "data/layers/climate_predictors/climate_predictor_forcing.tif"),
  nrw_boundary = file.path(app_dir, "data/vectors/nrw_boundary.gpkg"),
  support_points = file.path(app_dir, "data/vectors/support_points.gpkg")
)

dir.create(file.path(app_dir, "data/layers/frost_risk"), recursive = TRUE, showWarnings = FALSE)
dir.create(file.path(app_dir, "data/layers/terrain"), recursive = TRUE, showWarnings = FALSE)
dir.create(file.path(app_dir, "data/layers/climate_predictors"), recursive = TRUE, showWarnings = FALSE)
dir.create(file.path(app_dir, "data/vectors"), recursive = TRUE, showWarnings = FALSE)
dir.create(file.path(app_dir, "www/tiles"), recursive = TRUE, showWarnings = FALSE)

message("Preparing app layers in: ", app_dir)

copy_vector <- function(source, target) {
  if (!file.exists(source)) {
    message("Skipping missing vector: ", source)
    return(invisible(FALSE))
  }

  x <- sf::st_read(source, quiet = TRUE)
  if (file.exists(target)) {
    unlink(target)
  }
  sf::st_write(x, target, quiet = TRUE)
  message("Wrote vector: ", target)
  invisible(TRUE)
}

write_1km_raster <- function(source, target, layer_name = NULL, multiply = 1, method = "bilinear") {
  if (!file.exists(source)) {
    message("Skipping missing raster: ", source)
    return(invisible(FALSE))
  }

  r <- terra::rast(source)
  if (!is.null(layer_name)) {
    if (!layer_name %in% names(r)) {
      message("Skipping missing raster layer '", layer_name, "' in ", source)
      return(invisible(FALSE))
    }
    r <- r[[layer_name]]
  }

  r <- r * multiply
  target_resolution <- 1000
  current_resolution <- max(terra::res(r))

  if (is.finite(current_resolution) && current_resolution < target_resolution) {
    fact <- max(1, round(target_resolution / current_resolution))
    r <- terra::aggregate(r, fact = fact, fun = mean, na.rm = TRUE)
  }

  dir.create(dirname(target), recursive = TRUE, showWarnings = FALSE)
  terra::writeRaster(
    r,
    target,
    overwrite = TRUE,
    gdal = c("COMPRESS=LZW", "PREDICTOR=2")
  )
  message("Wrote raster: ", target)
  invisible(TRUE)
}

create_tiles_if_available <- function(source_tif, layer_id) {
  gdal2tiles <- Sys.which("gdal2tiles.py")
  if (!nzchar(gdal2tiles)) {
    gdal2tiles <- Sys.which("gdal2tiles")
  }
  if (!nzchar(gdal2tiles)) {
    message("GDAL gdal2tiles not found; keeping GeoTIFF fallback for ", layer_id)
    return(invisible(FALSE))
  }

  target_dir <- file.path(app_dir, "www/tiles", layer_id)
  dir.create(target_dir, recursive = TRUE, showWarnings = FALSE)
  args <- c("-z", "6-12", "-r", "bilinear", "-w", "none", source_tif, target_dir)
  status <- system2(gdal2tiles, args = args)
  if (identical(status, 0L)) {
    message("Wrote tiles: ", target_dir)
  } else {
    message("Tile creation failed for ", layer_id, "; GeoTIFF fallback remains available.")
  }
  invisible(identical(status, 0L))
}

copy_vector(source_paths$nrw_boundary, target_paths$nrw_boundary)
copy_vector(source_paths$support_points, target_paths$support_points)

# Frost-risk rasters are copied at their source scale convention. The app uses
# dynamic legend limits instead of forcing a 0-20 percent colorbar.
write_1km_raster(source_paths$frost_combined_2050_weighted, target_paths$frost_combined_2050_weighted)
write_1km_raster(source_paths$frost_historic_2010, target_paths$frost_historic_2010)
write_1km_raster(source_paths$frost_combined_2085_weighted, target_paths$frost_combined_2085_weighted)

write_1km_raster(source_paths$predictor_stack, target_paths$terrain_elevation, "elevation_100m")
write_1km_raster(source_paths$predictor_stack, target_paths$terrain_slope, "slope_100m")
write_1km_raster(source_paths$predictor_stack, target_paths$terrain_roughness, "roughness_100m")
write_1km_raster(source_paths$predictor_stack, target_paths$terrain_tpi, "tpi_100m")
write_1km_raster(source_paths$predictor_stack, target_paths$terrain_eastness, "eastness_100m")
write_1km_raster(source_paths$predictor_stack, target_paths$terrain_northness, "northness_100m")
write_1km_raster(source_paths$predictor_stack, target_paths$climate_predictor_frost_days, "frost_days_mar_may_q90")
write_1km_raster(source_paths$predictor_stack, target_paths$climate_predictor_forcing, "gdd_forcing_mean")

# Uncomment selected calls if you want tiled PNG overlays for deployment.
# create_tiles_if_available(target_paths$frost_combined_2050_weighted, "frost_combined_2050_weighted")

message("Done. Run the app from app/ with: R -e \"shiny::runApp()\"")
