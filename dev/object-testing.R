# blocks
# x <- feature_layer("https://services.arcgis.com/P3ePLMYs2RVChkJx/arcgis/rest/services/USA_Census_2020_Redistricting_Blocks/FeatureServer/0")

# tracts
# https://services.arcgis.com/P3ePLMYs2RVChkJx/ArcGIS/rest/services/USA_Census_2020_Redistricting_Tracts/FeatureServer/0

# counties
# https://services.arcgis.com/P3ePLMYs2RVChkJx/arcgis/rest/services/Census_2020_Redistricting_Counties/FeatureServer/0

url <- "https://services.arcgis.com/P3ePLMYs2RVChkJx/ArcGIS/rest/services/USA_Counties_Generalized_Boundaries/FeatureServer/0"

counties <- feature_layer(url)

counties

counties |>
  select(starts_with("STATE")) |>
  collect()

counties |>
  filter(STATE_FIPS == "01") |>
  select(FIPS, POP_SQMI) |>
  collect()

x <- x |>
  select(1:8) |>
  filter(P0010001 > 100)
