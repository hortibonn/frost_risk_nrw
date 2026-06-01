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
  unit <- layer$unit %||% ""
  lab_format <- function(type, cuts, p) {
    labels <- format(signif(cuts, 3), trim = TRUE)
    if (nzchar(unit)) {
      labels <- paste0(labels, " ", unit)
    }
    labels
  }

  leaflet::addLegend(
    map,
    position = position,
    pal = pal,
    values = limits,
    title = htmltools::htmlEscape(layer$legend_title),
    opacity = 0.9,
    layerId = layer_id,
    labFormat = lab_format
  )
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

add_support_points <- function(map, points, group = "support_points") {
  if (is.null(points)) {
    return(map)
  }

  points <- add_lon_lat_columns(points)
  category_col <- pick_first_existing(names(points), c("selection_group", "model_role", "type", "category"))
  fill <- "#2563eb"

  if (!is.null(category_col)) {
    categories <- as.character(points[[category_col]])
    pal <- leaflet::colorFactor(
      c("#2563eb", "#059669", "#d97706", "#7c3aed", "#dc2626", "#0891b2", "#4b5563"),
      domain = sort(unique(categories)),
      na.color = "#4b5563"
    )
    fill <- pal(categories)
  }

  leaflet::addCircleMarkers(
    map,
    data = points,
    group = group,
    radius = 5,
    stroke = TRUE,
    color = "#111827",
    weight = 1,
    fillColor = fill,
    fillOpacity = 0.88,
    opacity = 0.95,
    popup = build_support_popups(points)
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

build_support_popups <- function(points) {
  attrs <- sf::st_drop_geometry(points)
  show_cols <- setdiff(
    names(attrs),
    c("id", "ID", "cell", "fid", "FID", "geom", "geometry")
  )
  show_cols <- show_cols[!grepl("(^|_)id$", show_cols, ignore.case = TRUE)]
  preferred <- c(
    "selection_group", "model_role", "spatial_stratum", "elev_band",
    "frost_damage_pct", "frost_damage_risk", "elevation_100m",
    "frost_days_mar_may_mean", "frost_days_mar_may_q90",
    "last_frost_doy_mean", "gdd_forcing_mean", "lon", "lat"
  )
  show_cols <- unique(c(intersect(preferred, show_cols), show_cols))
  show_cols <- head(show_cols, 10)

  apply(attrs[, show_cols, drop = FALSE], 1, function(row) {
    rows <- vapply(names(row), function(col) {
      value <- row[[col]]
      if (is.na(value) || !nzchar(as.character(value))) {
        return("")
      }
      paste0(
        "<tr><th>", htmltools::htmlEscape(popup_field_label(col)), "</th><td>",
        htmltools::htmlEscape(popup_value_label(col, value)),
        "</td></tr>"
      )
    }, character(1))

    htmltools::HTML(paste0("<table class='popup-table'>", paste(rows, collapse = ""), "</table>"))
  })
}

popup_field_label <- function(col) {
  labels <- c(
    selection_group = "Auswahlgruppe",
    model_role = "Modellrolle",
    spatial_stratum = "Räumliche Klasse",
    elev_band = "Höhenklasse",
    frost_damage_pct = "Frostrisiko (%)",
    frost_damage_risk = "Frostrisiko",
    elevation_100m = "Höhe",
    slope_100m = "Hangneigung",
    northness_100m = "Nordexposition",
    eastness_100m = "Ostexposition",
    roughness_100m = "Rauigkeit",
    tpi_100m = "TPI",
    min_temp_mar_may_q10 = "Mindesttemperatur März-Mai q10",
    min_temp_mar_may_mean = "Mittlere Mindesttemperatur März-Mai",
    frost_days_mar_may_mean = "Mittlere Frosttage März-Mai",
    frost_days_mar_may_q90 = "Frosttage März-Mai q90",
    last_frost_doy_mean = "Mittlerer letzter Frosttag",
    last_frost_doy_q90 = "Letzter Frosttag q90",
    gdd_forcing_mean = "Mittlere Wärmesumme (GDD)",
    gdd_forcing_q10 = "Wärmesumme q10",
    gdd_forcing_q90 = "Wärmesumme q90",
    first_5daywarm_doy_mean = "Mittlerer erster 5-Tage-Wärmezeitraum",
    first_5daywarm_doy_q10 = "Erster 5-Tage-Wärmezeitraum q10",
    warm_spell_count_mean = "Mittlere Anzahl Wärmeperioden",
    validation_score = "Validierungswert",
    x_band = "X-Klasse",
    y_band = "Y-Klasse",
    lon = "Länge",
    lat = "Breite"
  )

  if (col %in% names(labels)) labels[[col]] else col
}

popup_value_label <- function(col, value) {
  text <- format(value, trim = TRUE, digits = 4)
  if (col == "model_role") {
    labels <- c(training = "Training", validation = "Validierung")
    key <- as.character(value)
    return(if (key %in% names(labels)) labels[[key]] else text)
  }
  if (col == "selection_group") {
    labels <- c(
      clhs = "cLHS",
      forced_coverage = "Erzwungene Abdeckung",
      forced_coverage_refill = "Nachbesetzung der Abdeckung",
      extreme_highest_elevation = "Extrempunkt höchste Lage",
      extreme_lowest_elevation = "Extrempunkt niedrigste Lage",
      extreme_coldest_spring_extreme = "Extrempunkt kältester Frühling",
      extreme_most_frost_days = "Extrempunkt meiste Frosttage",
      extreme_earliest_warm_spell = "Extrempunkt frühester Wärmezeitraum",
      extreme_highest_forcing = "Extrempunkt höchste Wärmesumme",
      extreme_refill_cold = "Nachbesetzung kalter Extrempunkt"
    )
    key <- as.character(value)
    return(if (key %in% names(labels)) labels[[key]] else text)
  }
  text
}

add_lon_lat_columns <- function(points) {
  coords <- sf::st_coordinates(points)
  points$lon <- round(coords[, "X"], 5)
  points$lat <- round(coords[, "Y"], 5)
  points
}

pick_first_existing <- function(values, candidates) {
  hits <- intersect(candidates, values)
  if (length(hits) == 0) {
    return(NULL)
  }
  hits[[1]]
}

`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}
