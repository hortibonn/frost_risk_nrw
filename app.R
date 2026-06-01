source("R/zzz.R")
check_app_packages()

library(shiny)
library(bslib)
library(leaflet)
library(sf)
library(terra)
library(htmltools)

source("R/layer_registry.R")
source("R/data_loading.R")
source("R/map_helpers.R")
source("R/ui_bottom_sheet.R")

app_dir <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
registry <- read_layer_registry(app_dir = app_dir)

ui <- build_app_ui(registry)

server <- function(input, output, session) {
  raster_cache <- new.env(parent = emptyenv())
  vector_cache <- new.env(parent = emptyenv())
  active_overlays <- reactiveVal(character(0))
  active_main_layer <- reactiveVal(NULL)

  get_vector <- function(layer) {
    if (is.null(layer) || !isTRUE(layer$available)) {
      return(NULL)
    }
    if (!exists(layer$id, envir = vector_cache, inherits = FALSE)) {
      assign(layer$id, load_vector_safe(layer), envir = vector_cache)
    }
    get(layer$id, envir = vector_cache, inherits = FALSE)
  }

  boundary_layer <- find_layer(registry, "nrw_boundary")
  support_layer <- find_layer(registry, "support_points")
  boundary <- get_vector(boundary_layer)
  support_points <- get_vector(support_layer)
  boundary_bbox <- get_nrw_bbox(boundary)

  output$map <- leaflet::renderLeaflet({
    map <- create_leaflet_map(boundary)

    if (!is.null(boundary) && isTRUE(boundary_layer$default_visible)) {
      map <- add_nrw_boundary(map, boundary)
    }
    if (!is.null(support_points) && isTRUE(support_layer$default_visible)) {
      map <- add_support_points(map, support_points)
    }

    map
  })

  selected_main_layer <- reactive({
    mode <- input$risk_cmip6_mode
    if (!has_cmip6_choices(registry, input$risk_year %||% "2050")) {
      mode <- NULL
    }

    find_main_frost_layer(
      registry,
      year = input$risk_year %||% "2050",
      cmip6_mode = mode
    )
  })

  output$cmip6_mode_ui <- renderUI({
    year <- input$risk_year %||% "2050"
    choices <- frost_mode_choices(registry, year)
    if (length(choices) == 0) {
      return(NULL)
    }

    selected <- input$risk_cmip6_mode
    if (is.null(selected) || !selected %in% unname(choices)) {
      selected <- if ("weighted" %in% unname(choices)) "weighted" else unname(choices)[[1]]
    }

    radioButtons(
      "risk_cmip6_mode",
      "CMIP6-Szenario",
      choices = choices,
      selected = selected,
      inline = TRUE
    )
  })

  observeEvent(input$risk_year, {
    choices <- frost_mode_choices(registry, input$risk_year)
    if (length(choices) == 0) {
      return()
    }
    selected <- input$risk_cmip6_mode
    if (is.null(selected) || !selected %in% unname(choices)) {
      selected <- if ("weighted" %in% unname(choices)) "weighted" else unname(choices)[[1]]
    }
    updateRadioButtons(session, "risk_cmip6_mode", choices = choices, selected = selected)
  }, ignoreInit = TRUE)

  output$main_layer_message <- renderText({
    layer <- selected_main_layer()
    if (is.null(layer)) {
      return("Ebene noch nicht verfügbar.")
    }
    if (!isTRUE(layer$available)) {
      return("Ebene noch nicht verfügbar.")
    }
    ""
  })

  observe({
    layer <- selected_main_layer()
    proxy <- leafletProxy("map")
    proxy <- leaflet::clearGroup(proxy, "main_frost_risk")
    proxy <- leaflet::removeControl(proxy, "legend_main_frost_risk")

    if (is.null(layer) || !isTRUE(layer$available)) {
      active_main_layer(NULL)
      session$sendCustomMessage("layer-status", "Die Standardebene für das Frostrisiko fehlt. Bitte die Ebenen unter app/data vor der Bereitstellung vorbereiten.")
      return()
    }

    active_main_layer(layer$id)
    session$sendCustomMessage("layer-status", "")
    proxy <- add_app_layer(proxy, layer, opacity = input$main_opacity %||% layer$opacity, group = "main_frost_risk", raster_cache = raster_cache)
    add_layer_legend(proxy, layer, layer_id = "legend_main_frost_risk", raster_cache = raster_cache)
  })

  observeEvent(input$main_opacity, {
    layer <- selected_main_layer()
    if (is.null(layer) || !isTRUE(layer$available)) {
      return()
    }

    proxy <- leafletProxy("map")
    proxy <- leaflet::clearGroup(proxy, "main_frost_risk")
    proxy <- leaflet::removeControl(proxy, "legend_main_frost_risk")
    proxy <- add_app_layer(proxy, layer, opacity = input$main_opacity, group = "main_frost_risk", raster_cache = raster_cache)
    add_layer_legend(proxy, layer, layer_id = "legend_main_frost_risk", raster_cache = raster_cache)
  }, ignoreInit = TRUE)

  overlay_ids <- reactive({
    unique(c(input$terrain_layers %||% character(0), input$climate_layers %||% character(0), input$misc_layers %||% character(0)))
  })

  observeEvent(overlay_ids(), {
    wanted <- overlay_ids()
    current <- active_overlays()
    proxy <- leafletProxy("map")

    for (id in setdiff(current, wanted)) {
      proxy <- leaflet::clearGroup(proxy, id)
      proxy <- leaflet::removeControl(proxy, paste0("legend_", id))
    }

    for (id in setdiff(wanted, current)) {
      layer <- find_layer(registry, id)
      if (is.null(layer) || !isTRUE(layer$available)) {
        next
      }
      opacity <- input[[paste0("opacity_", id)]] %||% layer$opacity
      if (identical(layer$type, "vector")) {
        proxy <- add_vector_context(proxy, layer, get_vector(layer), group = id)
      } else {
        proxy <- add_app_layer(proxy, layer, opacity = opacity, group = id, raster_cache = raster_cache)
        proxy <- add_layer_legend(proxy, layer, layer_id = paste0("legend_", id), position = "bottomleft", raster_cache = raster_cache)
      }
    }

    active_overlays(wanted)
  }, ignoreNULL = FALSE)

  lapply(available_layers(registry), function(layer) {
    if (!layer$type %in% c("raster_tif", "raster_tiles") || identical(layer$role, "main_frost_risk")) {
      return(NULL)
    }

    observeEvent(input[[paste0("opacity_", layer$id)]], {
      if (!layer$id %in% active_overlays()) {
        return()
      }
      proxy <- leafletProxy("map")
      proxy <- leaflet::clearGroup(proxy, layer$id)
      proxy <- leaflet::removeControl(proxy, paste0("legend_", layer$id))
      proxy <- add_app_layer(proxy, layer, opacity = input[[paste0("opacity_", layer$id)]], group = layer$id, raster_cache = raster_cache)
      add_layer_legend(proxy, layer, layer_id = paste0("legend_", layer$id), position = "bottomleft", raster_cache = raster_cache)
    }, ignoreInit = TRUE)
  })

  observeEvent(input$show_support_points, {
    proxy <- leafletProxy("map")
    proxy <- leaflet::clearGroup(proxy, "support_points")
    if (isTRUE(input$show_support_points) && !is.null(support_points)) {
      add_support_points(proxy, support_points)
    }
  }, ignoreInit = TRUE)

  observeEvent(input$show_nrw_boundary, {
    proxy <- leafletProxy("map")
    proxy <- leaflet::clearGroup(proxy, "nrw_boundary")
    if (isTRUE(input$show_nrw_boundary) && !is.null(boundary)) {
      add_nrw_boundary(proxy, boundary)
    }
  }, ignoreInit = TRUE)

  session$onFlushed(function() {
    session$sendCustomMessage("layer-status", "")
  }, once = TRUE)
}

shinyApp(ui, server)
