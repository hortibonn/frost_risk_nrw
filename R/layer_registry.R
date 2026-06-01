read_layer_registry <- function(path = file.path("config", "layers.yml"), app_dir = getwd()) {
  if (!requireNamespace("yaml", quietly = TRUE)) {
    stop("Das Paket 'yaml' wird benötigt, um config/layers.yml zu lesen.", call. = FALSE)
  }

  registry_path <- normalizePath(file.path(app_dir, path), winslash = "/", mustWork = TRUE)
  raw <- yaml::read_yaml(registry_path)
  layers <- raw$layers

  if (is.null(layers) || length(layers) == 0) {
    stop("Keine Ebenen in config/layers.yml gefunden.", call. = FALSE)
  }

  required <- c(
    "id", "title", "group", "type", "path", "default_visible", "opacity",
    "palette", "legend_title", "legend_min", "legend_max", "unit", "role"
  )

  normalized <- lapply(layers, function(layer) {
    missing <- setdiff(required, names(layer))
    if (length(missing) > 0) {
      stop(
        "Ebene '", layer$id %||% "<unbekannt>", "' enthält nicht alle Pflichtfelder: ",
        paste(missing, collapse = ", "),
        call. = FALSE
      )
    }

    layer$abs_path <- normalizePath(file.path(app_dir, layer$path), winslash = "/", mustWork = FALSE)
    layer$available <- layer_exists(layer)
    layer
  })

  class(normalized) <- c("layer_registry", class(normalized))
  normalized
}

layer_exists <- function(layer) {
  if (identical(layer$type, "raster_tiles")) {
    return(dir.exists(layer$abs_path))
  }
  file.exists(layer$abs_path)
}

available_layers <- function(registry) {
  Filter(function(layer) isTRUE(layer$available), registry)
}

filter_layers <- function(registry, group = NULL, role = NULL, type = NULL) {
  Filter(function(layer) {
    (is.null(group) || identical(layer$group, group)) &&
      (is.null(role) || identical(layer$role, role)) &&
      (is.null(type) || identical(layer$type, type))
  }, registry)
}

find_layer <- function(registry, id) {
  matches <- Filter(function(layer) identical(layer$id, id), registry)
  if (length(matches) == 0) {
    return(NULL)
  }
  matches[[1]]
}

find_main_frost_layer <- function(registry, year, cmip6_mode, stage = NULL) {
  layers_for_year <- Filter(function(layer) {
    identical(layer$role, "main_frost_risk") &&
      identical(as.integer(layer$year), as.integer(year))
  }, registry)

  if (is.null(cmip6_mode) || identical(as.character(cmip6_mode), "")) {
    if (length(layers_for_year) == 1) {
      return(layers_for_year[[1]])
    }
    return(NULL)
  }

  matches <- Filter(function(layer) {
    identical(layer$role, "main_frost_risk") &&
      identical(as.integer(layer$year), as.integer(year)) &&
      identical(as.character(layer$cmip6_mode), as.character(cmip6_mode)) &&
      (is.null(stage) || identical(as.character(layer$stage), as.character(stage)))
  }, registry)

  if (length(matches) == 0) {
    return(NULL)
  }
  matches[[1]]
}

frost_year_choices <- function(registry) {
  layers <- available_layers(filter_layers(registry, role = "main_frost_risk"))
  years <- sort(unique(vapply(layers, function(layer) as.integer(layer$year), integer(1))))
  stats::setNames(as.character(years), as.character(years))
}

frost_mode_choices <- function(registry, year = NULL) {
  layers <- available_layers(filter_layers(registry, role = "main_frost_risk"))
  if (!is.null(year)) {
    layers <- Filter(function(layer) identical(as.integer(layer$year), as.integer(year)), layers)
  }
  modes <- unique(vapply(layers, function(layer) as.character(layer$cmip6_mode), character(1)))
  modes <- setdiff(modes, "historic")
  if (length(modes) == 0) {
    return(character(0))
  }
  preferred <- c("weighted", "ssp126", "ssp245", "ssp370", "ssp585")
  modes <- c(intersect(preferred, modes), sort(setdiff(modes, preferred)))
  label_lookup <- c(
    weighted = "Gewichtet",
    ssp126 = "SSP1-2.6",
    ssp245 = "SSP2-4.5",
    ssp370 = "SSP3-7.0",
    ssp585 = "SSP5-8.5"
  )
  labels <- unname(label_lookup[modes])
  labels[is.na(labels)] <- modes[is.na(labels)]
  stats::setNames(modes, labels)
}

has_cmip6_choices <- function(registry, year) {
  length(frost_mode_choices(registry, year)) > 0
}

layer_choices <- function(registry, group) {
  layers <- available_layers(filter_layers(registry, group = group))
  stats::setNames(vapply(layers, `[[`, character(1), "id"), vapply(layers, `[[`, character(1), "title"))
}

`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}
