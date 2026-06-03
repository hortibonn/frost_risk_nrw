# NRW Frost-Risk Shiny App

This directory contains a standalone Shiny app for displaying existing NRW frost-risk outputs as a mobile-first interactive map. It does not calculate frost risk, run PhenoFlex, process CMIP6 data, or fit the GAM surrogate.

## Run Locally

From this directory:

```r
shiny::runApp()
```

From the repository root:

```r
shiny::runApp("app")
```

For a fixed local demo port:

```sh
Rscript app/scripts/run_app.R
```

Then open <http://127.0.0.1:3838>.

Required R packages:

```r
install.packages(c("shiny", "bslib", "leaflet", "sf", "terra", "yaml", "htmltools"))
```

## Data Layout

The app reads only from paths inside this directory:

```text
data/
  layers/
    frost_risk/
    terrain/
    climate_predictors/
    cmip6/
    misc/
  vectors/
    nrw_boundary.gpkg
www/
  app-icon.png
  tiles/
```

Layer metadata lives in `config/layers.yml`. Add new layers there instead of hard-coding them in the app.
The browser and touch icon is loaded from `www/app-icon.png`.

## Preparing Layers

Run the optional helper from the repository root:

```sh
Rscript app/scripts/prepare_app_layers.R
```

The helper copies vectors and writes 1 km display GeoTIFFs into `app/data`. Frost-risk rasters are written as percent values so the default 0-20% legend is meaningful.

The helper can also be extended to create lossless PNG XYZ tiles under `www/tiles/<layer_id>/`. If `gdal2tiles` is not installed, the app will continue to use GeoTIFF fallback layers.

## Missing Layers

The default demo expects `data/layers/frost_risk/frost_combined_2050_weighted.tif` plus the vector files. Optional layers listed in `config/layers.yml` are skipped when missing. If the selected main frost-risk layer is unavailable, the app stays open and reports that the layer is not available yet.

## Adding A Layer

1. Put the display-ready file under `data/layers/`, `data/vectors/`, or `www/tiles/`.
2. Add an entry to `config/layers.yml`.
3. Use `type: raster_tif`, `type: raster_tiles`, or `type: vector`.
4. Set `group`, `role`, `legend_min`, `legend_max`, `unit`, and `palette`.

For frost-risk selector layers, also set:

```yaml
year: 2050
cmip6_mode: weighted
```

Current `cmip6_mode` values used by the app are `weighted`, `ssp126`, `ssp245`, and `ssp370`. Stage-specific frost-risk controls are not exposed in the app for now.

The historic 2010 frost-risk layer is shown as a standalone year with no CMIP6 selector. Frost-risk layers use the Viridis Inferno palette and are display-capped by `FROST_RISK_DISPLAY_MAX` in `R/map_helpers.R`; set that value in probability units, for example `0.21` for 21%.

Climate predictor layers currently use the default Viridis palette, and terrain layers use the shared blue-yellow-red terrain gradient configured in `R/map_helpers.R`. Population density is read from `data/layers/misc/einwohner_density_nrw.tif` and displayed with a 6500 Einw./km2 cap so regional differences remain visible.

## Deployment Notes

For mobile/online deployment, pre-rendered PNG tiles are preferred for large rasters. The GeoTIFF fallback is useful for small 1 km demo layers and local development.
