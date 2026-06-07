build_app_ui <- function(registry) {
  bslib::page_fillable(
    title = "NRW-Frostrisiko",
    theme = bslib::bs_theme(version = 5, bootswatch = NULL),
    fillable_mobile = TRUE,
    tags$head(
      tags$meta(name = "viewport", content = "width=device-width, initial-scale=1, viewport-fit=cover"),
      tags$meta(name = "theme-color", content = "#f8fafc"),
      tags$link(rel = "icon", type = "image/png", href = "app-icon.png"),
      tags$link(rel = "apple-touch-icon", href = "app-icon.png"),
      tags$link(rel = "stylesheet", type = "text/css", href = "styles.css"),
      tags$script(src = "bottom-sheet.js")
    ),
    div(
      class = "app-shell",
      div(class = "map-canvas", leaflet::leafletOutput("map", width = "100%", height = "100%")),
      div(id = "layer_status", class = "layer-status"),
      build_bottom_sheet(registry)
    )
  )
}

build_bottom_sheet <- function(registry) {
  div(
    id = "bottom_sheet",
    class = "bottom-sheet collapsed",
    tags$button(
      id = "bottom_sheet_handle",
      class = "sheet-handle",
      type = "button",
      `aria-label` = "Ebeneneinstellungen öffnen",
      span(class = "handle-bar"),
      span(class = "handle-arrow")
    ),
    div(
      class = "sheet-content",
      div(
        class = "sheet-title-row",
        div(class = "sheet-title", "Ebenen"),
        tags$button(id = "bottom_sheet_close", class = "sheet-close", type = "button", `aria-label` = "Ebeneneinstellungen schließen", HTML("&times;"))
      ),
      build_main_controls(registry),
      build_overlay_controls(registry, "Gelände", "terrain_layers"),
      build_overlay_controls(registry, "Klimaprädiktoren", "climate_layers"),
      build_overlay_controls(registry, "Sonstiges", "misc_layers"),
      build_vector_controls(registry)
    )
  )
}

build_main_controls <- function(registry) {
  year_choices <- frost_year_choices(registry)
  selected_year <- if ("2050" %in% unname(year_choices)) "2050" else unname(year_choices)[[1]]
  mode_choices <- frost_mode_choices(registry, selected_year)
  selected_mode <- if ("weighted" %in% unname(mode_choices)) "weighted" else unname(mode_choices)[[1]]

  tags$details(
    class = "control-group",
    open = NA,
    tags$summary("Frostrisiko"),
    div(
      class = "control-grid",
      radioButtons("risk_year", "Jahr", choices = year_choices, selected = selected_year, inline = TRUE),
      uiOutput("cmip6_mode_ui"),
      sliderInput("main_opacity", "Deckkraft", min = 0, max = 1, value = 0.82, step = 0.05)
    ),
    div(textOutput("main_layer_message"), class = "sheet-message")
  )
}

build_overlay_controls <- function(registry, group, input_id) {
  layers <- available_layers(filter_layers(registry, group = group))
  if (length(layers) == 0) {
    return(NULL)
  }

  choice_values <- vapply(layers, `[[`, character(1), "id")
  choice_names <- lapply(layers, layer_choice_label)

  tags$details(
    class = "control-group",
    tags$summary(group),
    checkboxGroupInput(input_id, NULL, choiceNames = choice_names, choiceValues = choice_values, selected = character(0)),
    lapply(layers, function(layer) {
      conditionalPanel(
        condition = sprintf("input['%s'] && input['%s'].indexOf('%s') !== -1", input_id, input_id, layer$id),
        div(
          class = "opacity-row",
          sliderInput(
            paste0("opacity_", layer$id),
            paste(layer$title, "Deckkraft"),
            min = 0,
            max = 1,
            value = as.numeric(layer$opacity),
            step = 0.05
          )
        )
      )
    })
  )
}

layer_choice_label <- function(layer) {
  if (is.null(layer$source_label) || !nzchar(layer$source_label)) {
    return(layer$title)
  }

  tags$span(
    class = "layer-choice layer-choice-with-source",
    tags$span(class = "layer-choice-title", layer$title),
    tags$a(
      class = "layer-source",
      href = layer$source_url %||% "#",
      target = "_blank",
      rel = "noopener noreferrer",
      layer$source_label
    )
  )
}

build_vector_controls <- function(registry) {
  boundary <- find_layer(registry, "nrw_boundary")

  div(
    class = "control-group vector-group",
    div(class = "group-label", "Punkte und Grenzen"),
    if (!is.null(boundary) && isTRUE(boundary$available)) {
      checkboxInput("show_nrw_boundary", boundary$title, value = isTRUE(boundary$default_visible))
    }
  )
}
