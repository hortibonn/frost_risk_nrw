required_app_packages <- c("shiny", "bslib", "leaflet", "sf", "terra", "yaml", "htmltools", "htmlwidgets")

check_app_packages <- function() {
  missing <- required_app_packages[!vapply(required_app_packages, requireNamespace, logical(1), quietly = TRUE)]
  if (length(missing) > 0) {
    stop(
      "Fehlende R-Pakete: ", paste(missing, collapse = ", "),
      ". Bitte vor dem Start der App installieren.",
      call. = FALSE
    )
  }
}
