library(arcgis)

# Feature Layer -----------------------------------------------------------

# define the feature layer that we are interested in:
usa_counties_url <- "https://services.arcgis.com/P3ePLMYs2RVChkJx/ArcGIS/rest/services/USA_Counties_Generalized_Boundaries/FeatureServer/0"

# "open" the feagture layer
usa_fl <- arc_open(usa_counties_url)

# print it out
# this contains metadata for the layer itself
usa_fl

# to bring data into R memory as an R object use
usa_sf <- arc_select(usa_fl)

# print it out
usa_sf

# example plot:
plot(usa_sf[["geometry"]])


# specify where clause or output fields
ca <- arc_select(
  usa_fl,
  fields = c("STATE_NAME", "NAME", "POPULATION"),
  where = "STATE_NAME = 'California'"
  )

plot(ca["POPULATION"])


# Table -------------------------------------------------------------------

# similar behavior for tables
provider_url <- "https://services2.arcgis.com/j80Jz20at6Bi0thr/ArcGIS/rest/services/List_of_Providers/FeatureServer/27"

# use pipes to reduce intermediate objects
provider_tbl <- provider_url |>
  arc_open() |>
  arc_select() |>
  tibble::as_tibble()

provider_tbl



# Raster ------------------------------------------------------------------

# based on old issue from 2019 in arcgisbinding
# example
polys <- arc_open("https://maps2.dcgis.dc.gov/dcgis/rest/services/DCGIS_DATA/Planning_Landuse_and_Zoning_WebMercator/MapServer/31") |>
  arc_select()


img_srv <- arc_open(
  "https://imagery.dcgis.dc.gov/dcgis/rest/services/Ortho/Ortho_2019/ImageServer"
  )

# view the image server
img_srv

# extract the extent of a polygon
bbox <- sf::st_bbox(polys)

# use the extent to specify the raster
# this syntax isn't locked down yet
rst <- arc_raster(
  img_srv,
  bbox["xmin"],
  bbox["xmax"],
  bbox["ymin"],
  bbox["ymax"],
  width = 1000,
  height = 1000,
  bbox_crs = sf::st_crs(bbox)
)

# plot the raster
terra::plotRGB(rst, 1, 2, 3)

# plot the vector over it
polys |>
  sf::st_transform(sf::st_crs(rst)) |>
  sf::st_geometry() |>
  plot(add = TRUE, lwd = 3, border = "white")


