# Set in probability units; rasters stored as percentages are capped at this value * 100.
FROST_RISK_DISPLAY_MAX <- 0.21

create_leaflet_map <- function(boundary = NULL) {
  bbox <- get_nrw_bbox(boundary)
  max_bounds <- expand_bbox(bbox, 0.75)
  relaxed_bounds <- expand_bbox(bbox, 1.8)

  leaflet::leaflet(
    options = leaflet::leafletOptions(
      zoomControl = TRUE,
      preferCanvas = TRUE,
      maxBounds = list(
        list(max_bounds$lat1, max_bounds$lng1),
        list(max_bounds$lat2, max_bounds$lng2)
      ),
      maxBoundsViscosity = 0.85
    )
  ) |>
    leaflet::addProviderTiles(
      leaflet::providers$CartoDB.Positron,
      options = leaflet::providerTileOptions(noWrap = TRUE)
    ) |>
    leaflet::fitBounds(bbox$lng1, bbox$lat1, bbox$lng2, bbox$lat2) |>
    htmlwidgets::onRender(
      "
      function(el, x) {
        var map = this;
        var normalBounds = [[%f, %f], [%f, %f]];
        var relaxedBounds = [[%f, %f], [%f, %f]];
        var nrwBounds = [[%f, %f], [%f, %f]];

        window.nrwLeafletMap = map;
        window.nrwMapState = {
          normalBounds: normalBounds,
          relaxedBounds: relaxedBounds,
          nrwBounds: nrwBounds,
          normalViscosity: 0.85,
          relaxedViscosity: 0.35,
          openedBySheet: false,
          userMovedAfterOpen: false
        };

        map.whenReady(function() {
          var initialZoom = map.getZoom();
          window.nrwMapState.initialZoom = initialZoom;
          window.nrwMapState.initialCenter = map.getCenter();
          map.setMinZoom(Math.max(0, initialZoom - 1));

          map.on('dragstart zoomstart', function() {
            if (window.nrwMapState && window.nrwMapState.openedBySheet) {
              window.nrwMapState.userMovedAfterOpen = true;
            }
          });
        });
      }
      " |>
        sprintf(
          max_bounds$lat1, max_bounds$lng1, max_bounds$lat2, max_bounds$lng2,
          relaxed_bounds$lat1, relaxed_bounds$lng1, relaxed_bounds$lat2, relaxed_bounds$lng2,
          bbox$lat1, bbox$lng1, bbox$lat2, bbox$lng2
        )
    )
}

layer_palette <- function(layer, raster = NULL) {
  if (identical(layer$palette, "orchard")) {
    return(function(values) {
      colors <- rep("transparent", length(values))
      colors[!is.na(values) & values > 0] <- "#2f855a"
      colors
    })
  }

  limits <- layer_limits(layer, raster)
  min_value <- limits[[1]]
  max_value <- limits[[2]]

  colors <- switch(
    layer$palette,
    frost_risk = rev(c("#000004", "#1f0c48", "#550f6d", "#88226a", "#a83655", "#e35933", "#f9950a", "#fcffa4")),
    terrain = c("#2c7bb6", "#ffffbf", "#d7191c"),
    viridis = c("#440154", "#414487", "#2a788e", "#22a884", "#7ad151", "#fde725"),
    sequential = c("#f7fbff", "#c6dbef", "#6baed6", "#2171b5", "#08306b"),
    diverging = c("#2166ac", "#f7f7f7", "#b2182b"),
    cold = c("#f7fbff", "#deebf7", "#9ecae1", "#3182bd", "#08519c"),
    warm = c("#ffffcc", "#fed976", "#fd8d3c", "#e31a1c", "#800026"),
    population = c("#fff7bc", "#fee391", "#fec44f", "#ec7014", "#7f0000"),
    orchard = c("#2f855a", "#2f855a"),
    c("#f7fbff", "#9ecae1", "#08519c")
  )

  leaflet::colorNumeric(colors, domain = c(min_value, max_value), na.color = "transparent")
}

layer_limits <- function(layer, raster = NULL) {
  configured <- c(suppressWarnings(as.numeric(layer$legend_min)), suppressWarnings(as.numeric(layer$legend_max)))
  if (length(configured) == 2 && all(is.finite(configured))) {
    return(configured)
  }

  if (is.null(raster) && identical(layer$type, "raster_tif") && isTRUE(layer$available)) {
    raster <- load_raster_safe(layer)
  }

  if (!is.null(raster)) {
    if (is_frost_risk_layer(layer)) {
      display_max <- frost_risk_display_max_for_raster(raster)
      return(c(0, display_max))
    }

    range_values <- terra::global(raster, "range", na.rm = TRUE)
    values <- as.numeric(range_values[1, ])
    if (length(values) == 2 && all(is.finite(values)) && values[[1]] != values[[2]]) {
      return(values)
    }
    if (length(values) == 2 && all(is.finite(values))) {
      return(values + c(-0.5, 0.5))
    }
  }

  c(0, 1)
}

add_app_layer <- function(map, layer, opacity = NULL, group = layer$id, raster_cache = NULL) {
  opacity <- opacity %||% as.numeric(layer$opacity)

  if (identical(layer$type, "raster_tiles")) {
    return(add_raster_tiles(map, layer, opacity, group))
  }

  if (identical(layer$type, "raster_tif")) {
    raster <- NULL
    if (!is.null(raster_cache) && exists(layer$id, envir = raster_cache, inherits = FALSE)) {
      raster <- get(layer$id, envir = raster_cache, inherits = FALSE)
    } else {
      raster <- prepare_display_raster(layer, load_raster_safe(layer))
      if (!is.null(raster_cache) && !is.null(raster)) {
        assign(layer$id, raster, envir = raster_cache)
      }
    }
    return(add_raster_tif(map, layer, raster, opacity, group))
  }

  map
}

prepare_display_raster <- function(layer, raster) {
  raster <- truncate_frost_risk_raster(layer, raster)
  truncate_population_raster(layer, raster)
}

is_frost_risk_layer <- function(layer) {
  identical(layer$role, "main_frost_risk") || identical(layer$palette, "frost_risk")
}

frost_risk_display_max_for_raster <- function(raster) {
  range_values <- terra::global(raster, "range", na.rm = TRUE)
  values <- as.numeric(range_values[1, ])
  if (length(values) == 2 && is.finite(values[[2]]) && values[[2]] > 1) {
    return(FROST_RISK_DISPLAY_MAX * 100)
  }
  FROST_RISK_DISPLAY_MAX
}

truncate_frost_risk_raster <- function(layer, raster) {
  if (is.null(raster) || !is_frost_risk_layer(layer)) {
    return(raster)
  }

  display_max <- frost_risk_display_max_for_raster(raster)
  terra::clamp(raster, lower = -Inf, upper = display_max, values = TRUE)
}

truncate_population_raster <- function(layer, raster) {
  if (is.null(raster) || !identical(layer$palette, "population")) {
    return(raster)
  }

  upper <- suppressWarnings(as.numeric(layer$legend_max))
  if (!is.finite(upper)) {
    return(raster)
  }

  terra::clamp(raster, lower = -Inf, upper = upper, values = TRUE)
}

add_raster_tiles <- function(map, layer, opacity, group = layer$id) {
  leaflet::addTiles(
    map,
    urlTemplate = file.path("tiles", layer$id, "{z}", "{x}", "{y}.png"),
    group = group,
    options = leaflet::tileOptions(opacity = opacity, tms = FALSE)
  )
}

add_raster_tif <- function(map, layer, raster, opacity, group = layer$id) {
  if (is.null(raster)) {
    return(map)
  }

  pal <- layer_palette(layer, raster)

  leaflet::addRasterImage(
    map,
    raster,
    colors = pal,
    opacity = opacity,
    group = group,
    project = TRUE
  )
}

add_layer_legend <- function(map, layer, layer_id = paste0("legend_", layer$id), position = "bottomright", raster_cache = NULL) {
  if (!layer$type %in% c("raster_tif", "raster_tiles")) {
    return(map)
  }

  if (identical(layer$palette, "orchard")) {
    return(
      leaflet::addLegend(
        map,
        position = position,
        colors = "#2f855a",
        labels = "Vorkommen",
        title = htmltools::htmlEscape(layer$legend_title),
        opacity = 0.75,
        layerId = layer_id
      )
    )
  }

  raster <- NULL
  if (identical(layer$type, "raster_tif")) {
    if (!is.null(raster_cache) && exists(layer$id, envir = raster_cache, inherits = FALSE)) {
      raster <- get(layer$id, envir = raster_cache, inherits = FALSE)
    } else {
      raster <- load_raster_safe(layer)
    }
  }

  pal <- layer_palette(layer, raster)
  limits <- layer_limits(layer, raster)
  lab_format <- function(type, cuts, p) {
    format_layer_legend_labels(layer, cuts, limits)
  }

  map <- leaflet::addLegend(
    map,
    position = position,
    pal = pal,
    values = limits,
    title = htmltools::htmlEscape(layer$legend_title),
    opacity = 0.9,
    layerId = layer_id,
    labFormat = lab_format
  )

  reverse_latest_legend(map)
}

format_layer_legend_labels <- function(layer, cuts, limits = NULL) {
  if (is_frost_risk_layer(layer)) {
    values <- cuts
    if (legend_values_are_probability_scale(limits %||% cuts)) {
      values <- values * 100
    }
    return(paste0(format(signif(values, 3), trim = TRUE), "%"))
  }

  unit <- layer$unit %||% ""
  labels <- format(signif(cuts, 3), trim = TRUE)
  if (nzchar(unit)) {
    labels <- paste0(labels, " ", unit)
  }
  labels
}

legend_values_are_probability_scale <- function(values) {
  values <- suppressWarnings(as.numeric(values))
  values <- values[is.finite(values)]
  length(values) > 0 && max(abs(values)) <= 1
}

reverse_latest_legend <- function(map) {
  call_index <- length(map$x$calls)
  if (call_index == 0) {
    return(map)
  }

  legend_call <- map$x$calls[[call_index]]
  if (!identical(legend_call$method, "addLegend") || length(legend_call$args) == 0) {
    return(map)
  }

  legend_args <- legend_call$args[[1]]
  if (!is.null(legend_args$labels)) {
    legend_args$labels <- rev(legend_args$labels)
  }
  if (!is.null(legend_args$colors)) {
    legend_args$colors <- reverse_legend_colors(legend_args$colors)
  }

  legend_call$args[[1]] <- legend_args
  map$x$calls[[call_index]] <- legend_call
  map
}

reverse_legend_colors <- function(colors) {
  color_class <- class(colors)
  reversed <- if (length(colors) == 1 && grepl(",", colors[[1]], fixed = TRUE)) {
    reverse_gradient_stops(colors[[1]])
  } else {
    rev(colors)
  }

  if (length(color_class) > 0) {
    class(reversed) <- color_class
  }
  reversed
}

reverse_gradient_stops <- function(gradient) {
  stops <- trimws(strsplit(gradient, ",", fixed = TRUE)[[1]])
  stops <- rev(stops)
  stops <- vapply(stops, reverse_gradient_stop_position, character(1))
  paste(stops, collapse = ", ")
}

reverse_gradient_stop_position <- function(stop) {
  match <- regexec("^(.*?)([0-9]+(?:\\.[0-9]+)?)%\\s*$", stop)
  parts <- regmatches(stop, match)[[1]]
  if (length(parts) == 0) {
    return(stop)
  }

  position <- 100 - as.numeric(parts[[3]])
  paste0(trimws(parts[[2]]), " ", format(signif(position, 6), trim = TRUE), "%")
}

add_nrw_boundary <- function(map, boundary, group = "nrw_boundary") {
  if (is.null(boundary)) {
    return(map)
  }

  leaflet::addPolylines(
    map,
    data = boundary,
    group = group,
    color = "#111827",
    weight = 2,
    opacity = 0.9,
    fill = FALSE
  )
}

add_vector_context <- function(map, layer, data, group = layer$id) {
  if (is.null(data)) {
    return(map)
  }

  leaflet::addPolylines(
    map,
    data = data,
    group = group,
    color = "#374151",
    weight = 1,
    opacity = as.numeric(layer$opacity),
    fill = FALSE
  )
}

`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}
