load_vector_safe <- function(layer) {
  if (is.null(layer) || !isTRUE(layer$available)) {
    return(NULL)
  }

  tryCatch(
    {
      sf_obj <- sf::st_read(layer$abs_path, quiet = TRUE)
      if (is.na(sf::st_crs(sf_obj))) {
        warning("Vektorebene hat kein CRS: ", layer$id, call. = FALSE)
        return(NULL)
      }
      sf::st_transform(sf_obj, 4326)
    },
    error = function(e) {
      warning("Vektorebene konnte nicht geladen werden: '", layer$id, "': ", conditionMessage(e), call. = FALSE)
      NULL
    }
  )
}

load_raster_safe <- function(layer) {
  if (is.null(layer) || !isTRUE(layer$available)) {
    return(NULL)
  }

  tryCatch(
    terra::rast(layer$abs_path),
    error = function(e) {
      warning("Rasterebene konnte nicht geladen werden: '", layer$id, "': ", conditionMessage(e), call. = FALSE)
      NULL
    }
  )
}

get_nrw_bbox <- function(boundary) {
  if (is.null(boundary)) {
    return(list(lng1 = 5.6, lat1 = 50.2, lng2 = 9.7, lat2 = 52.7))
  }

  bbox <- sf::st_bbox(boundary)
  list(
    lng1 = unname(bbox[["xmin"]]),
    lat1 = unname(bbox[["ymin"]]),
    lng2 = unname(bbox[["xmax"]]),
    lat2 = unname(bbox[["ymax"]])
  )
}

expand_bbox <- function(bbox, factor = 0.28) {
  width <- bbox$lng2 - bbox$lng1
  height <- bbox$lat2 - bbox$lat1

  list(
    lng1 = bbox$lng1 - width * factor,
    lat1 = bbox$lat1 - height * factor,
    lng2 = bbox$lng2 + width * factor,
    lat2 = bbox$lat2 + height * factor
  )
}

bbox_center <- function(bbox) {
  list(
    lng = (bbox$lng1 + bbox$lng2) / 2,
    lat = (bbox$lat1 + bbox$lat2) / 2
  )
}

upper_half_focus <- function(bbox, shift_fraction = 0.34, bounds_factor = 0.75) {
  center <- bbox_center(bbox)
  height <- bbox$lat2 - bbox$lat1
  bounds <- expand_bbox(bbox, bounds_factor)
  shifted_lat <- center$lat + height * shift_fraction
  shifted_lat <- min(max(shifted_lat, bounds$lat1), bounds$lat2)

  list(lng = center$lng, lat = shifted_lat)
}
