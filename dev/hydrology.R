library(httr2)
set_auth_token(arcgis::auth_client())

x <- sf::st_point(c(-11946425.36295705, 4930494.072505761)) |>
  sf::st_sfc(crs = 3857)

hydrology_service <- function(x, token = Sys.getenv("ARCGIS_TOKEN")) {

  stopifnot(inherits(x, "sfc_POINT"))

  burl <- "https://hydro.arcgis.com/arcgis/rest/services/Tools/Hydrology/GPServer/Watershed/submitJob"


}




resp <- request(burl) |>
  req_body_form(
    InputPoints = '{"geometryType":"esriGeometryPoint","features":[{"geometry":{"x":-11946425.36295705,"y":4930494.072505761,"spatialReference":{"wkid":102100,"latestWkid":3857}}}],"sr":{"wkid":102100,"latestWkid":3857}}',
    SnapDistance = 500,
    SnapDistanceUnits = "Meters",
    Generalize = TRUE,
    f = "json",
    token = token
  ) |>
  req_perform()

res_id <- resp_body_json(resp)


job_res_url <- glue::glue("https://hydro.arcgis.com/arcgis/rest/services/Tools/Hydrology/GPServer/Watershed/jobs/{res_id$jobId}")

Sys.sleep(5)

job_resp <- request(job_res_url) |>
  req_body_form(
    f = "json",
    token = token
  ) |>
  req_perform()

job_res <- resp_body_json(job_resp)
job_res$results

job_res_url_outp1 <- glue::glue("https://hydro.arcgis.com/arcgis/rest/services/Tools/Hydrology/GPServer/Watershed/jobs/{res_id$jobId}/{job_res$results$WatershedArea}")

job_outp1 <- request(job_res_url_outp1) |>
  req_body_form(
    f = "json",
    token = token
  ) |>
  req_perform()



string <- resp_body_string(job_outp1)

parse_esri_json(string, query = "/value") |> plot()



job_res_url_outp1 <- glue::glue("https://hydro.arcgis.com/arcgis/rest/services/Tools/Hydrology/GPServer/Watershed/jobs/{res_id$jobId}/{job_res$results$SnappedPoints$paramUrl}")
