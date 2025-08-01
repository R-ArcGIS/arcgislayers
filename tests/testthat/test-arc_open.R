# These tests test that the service can be read correctly.
# can be improved to check attributes like query, class, etc.

test_that("arc_open(): Feature Layer", {
  ft_url <- "https://services.arcgis.com/P3ePLMYs2RVChkJx/ArcGIS/rest/services/USA_Counties_Generalized_Boundaries/FeatureServer/0"

  lyr <- arc_open(ft_url)

  expect_no_error(lyr)

  ft_query_url <- "https://services.arcgis.com/P3ePLMYs2RVChkJx/ArcGIS/rest/services/USA_Counties_Generalized_Boundaries/FeatureServer/0/query?outFields=%2A&where=1%3D1"

  lyr_q <- arc_open(ft_query_url)

  expect_identical(lyr, lyr_q)
})


test_that("arc_open(): Table", {
  tbl_url <- "https://services2.arcgis.com/j80Jz20at6Bi0thr/ArcGIS/rest/services/List_of_Providers/FeatureServer/27"

  expect_no_error(arc_open(tbl_url))
})

test_that("arc_open(): Feature Server", {
  server_url <- "https://services2.arcgis.com/j80Jz20at6Bi0thr/ArcGIS/rest/services/hexagons_state/FeatureServer"
  expect_no_error(arc_open(server_url))
})

test_that("arc_open(): Map Server", {
  map_url <- paste0(
    "https://services.arcgisonline.com/ArcGIS/rest/services/",
    "World_Imagery/MapServer"
  )

  expect_no_error(arc_open(map_url))
})

test_that("arc_open(): Image Server", {
  img_url <- "https://landsat2.arcgis.com/arcgis/rest/services/Landsat/MS/ImageServer"

  expect_no_error(arc_open(img_url))
})

test_that("arc_open(): GroupLayer", {
  gurl <- "https://geodata.baltimorecity.gov/egis/rest/services/CitiMap/DOT_Layers/MapServer/0"
  expect_no_error(arc_open(gurl))
})

test_that("arc_open(): doesn't filter NULL properties", {
  furl <- "https://geodata.md.gov/imap/rest/services/Transportation/MD_Transit/FeatureServer/8"

  flayer <- arc_open(furl)

  expect_identical(length(flayer), 56L)
})

test_that("arc_open(): generic item support", {
  test_cases <- c(
    map_server = "https://image.discomap.eea.europa.eu/arcgis/rest/services/Corine/CLC2000_WM/MapServer",
    feature_layer = "https://image.discomap.eea.europa.eu/arcgis/rest/services/Corine/CLC2000_WM/MapServer/0",
    feature_server = "https://services.arcgis.com/P3ePLMYs2RVChkJx/arcgis/rest/services/USA_Major_Cities_/FeatureServer",
    scene = "https://tiles.arcgis.com/tiles/oPre3pOfRfefL8y0/arcgis/rest/services/3D_Buildings_Switzerland_wgs84/SceneServer",
    tile_imagery = "https://image.arcgisonline.nl/arcgis/rest/services/KEA/Maximale_overstromingsdiepte/ImageServer",
    elevation = "https://tiles.arcgis.com/tiles/qHLhLQrcvEnxjtPr/arcgis/rest/services/British_National_Grid_Terrain_3D/ImageServer",
    webmap_app = "https://esri2.maps.arcgis.com/apps/instant/media/index.html?appid=80eb92ffc89b4086abe8cedd58ab160c",
    storymap = "https://storymaps.arcgis.com/stories/ad791fda858c46fdbe79636aa5f35dd8",
    instant_app = "https://actgov.maps.arcgis.com/apps/instant/interactivelegend/index.html?appid=f2dfd67d29ed4cabbb91e742e0297955",
    dashboard = "https://www.arcgis.com/apps/dashboards/84ba9c03786e462d960e3172bc1b2204",
    experience = "https://experience.arcgis.com/experience/6e360741bfd84db79d5db774a1147815",
    webapp = "https://governmentofbc.maps.arcgis.com/apps/webappviewer/index.html?id=950b4eec577a4dc5b298a61adab41c06",
    notebook_item = "https://geosaurus.maps.arcgis.com/home/item.html?id=9a9fca3f09bb41dd856c9cd4239b8519",
    notebook = "https://geosaurus.maps.arcgis.com/home/notebook/notebook.html?id=9a9fca3f09bb41dd856c9cd4239b8519",
    webscene = "https://analysis-1.maps.arcgis.com/home/webscene/viewer.html?webscene=7b506043536246faa4194d4c3d4c921b",
    group = "https://analysis-1.maps.arcgis.com/home/group.html?id=2f0ec8cb03574128bd673cefab106f39#overview",
    user = "https://analysis-1.maps.arcgis.com/home/user.html?user=jparry_ANGP",
    item_db = "https://analysis-1.maps.arcgis.com/home/item.html?id=84ba9c03786e462d960e3172bc1b2204",
    item_mapserver = "https://analysis-1.maps.arcgis.com/home/item.html?id=1d150c40d9f642cb8bd691017bf22cee",
    feature_collection = "https://analysis-1.maps.arcgis.com/home/item.html?id=24aa36ce1d7747c2b5a6aa57711d03fb",
    gp_server = "https://gis.pikepa.org/arcgis/rest/services/Utilities/GeocodingTools/GPServer",
    geometry_server = "https://sampleserver6.arcgisonline.com/arcgis/rest/services/Utilities/Geometry/GeometryServer",
    geocode = "https://geocode.arcgis.com/arcgis/rest/services/World/GeocodeServer",
    table = "https://services.arcgis.com/P3ePLMYs2RVChkJx/arcgis/rest/services/USA_Wetlands/FeatureServer/1",
    fserv_id = "3c164274a80748dda926a046525da610"
  )

  all_classes <- list(
    c("MapServer", "list"),
    c("FeatureLayer", "list"),
    c("FeatureServer", "list"),
    c("SceneServer", "list"),
    c("ImageServer", "list"),
    c("ImageServer", "list"),
    c("PortalItem", "list"),
    c("PortalItem", "list"),
    c("PortalItem", "list"),
    c("PortalItem", "list"),
    c("PortalItem", "list"),
    c("PortalItem", "list"),
    c("PortalItem", "list"),
    c("PortalItem", "list"),
    c("PortalItem", "list"),
    c("PortalGroup", "list"),
    c("PortalUser", "list"),
    c("PortalItem", "list"),
    c("MapServer", "list"),
    c("PortalItem", "list"),
    c("GPServer", "list"),
    c("GeometryServer", "list"),
    c("GeocodeServer", "list"),
    c("Table", "list"),
    c("FeatureServer", "list")
  )

  for (i in seq_along(test_cases)) {
    cli::cli_alert_info("Testing {test_cases[[i]]}")
    item <- arc_open(test_cases[[i]])

    observed <- class(item)
    expected <- all_classes[[i]]

    if (!identical(observed, expected)) {
      cli::cli_alert_danger(
        "Found mismatch between Observed: {.cls {observed}} and expected: {.cls {expected}}"
      )
    }
    expect_equal(class(item), all_classes[[i]])
  }
})


test_that("arc_open(): informative error with unsupported type", {
  expect_error(arc_open("https://www.google.com/"))
})

test_that("arc_open(): can open service folders", {
  url <- "https://egisdata.baltimorecity.gov/egis/rest/services/BaseMaps"
  expect_equal(class(arc_open(url)), "list")
})
