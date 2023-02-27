
# Client auth -------------------------------------------------------------


#' Authentication
#'
#' @export
#' @rdname auth
auth_client <- function(client = Sys.getenv("ARCGIS_CLIENT"),
                        secret = Sys.getenv("ARCGIS_SECRET"),
                        host = "https://www.arcgis.com") {
  # https://developers.arcgis.com/documentation/mapping-apis-and-services/security/application-credentials/
  token_url <- paste(
    host,
    "sharing",
    "rest",
    "oauth2",
    "token",
    sep = "/"
  )

  req <- httr2::request(token_url)

  # build the entire query url
  req_built <-
    httr2::req_url_query(
      req,
      client_id = httr2::obfuscated(client),
      client_secret = httr2::obfuscated(secret),
      grant_type = "client_credentials"
    )

  # send the request
  result <- httr2::req_perform(req_built)

  token <- httr2::resp_body_json(result)

  # handle errors
  if (is.null(token[["access_token"]])) {
    stop(
      "\n  Status code: ", token[["error"]][["code"]], "\n  ", token$error$message,
      call. = FALSE
    )
  }

  # return the token
  httr2::oauth_token(
    token[["access_token"]],
    expires_in = token[["expires_in"]]
  )

}




# Code flow ---------------------------------------------------------------


#' @export
#' @rdname auth
auth_code <- function(client = Sys.getenv("ARCGIS_CLIENT"),
                      host = "https://www.arcgis.com") {

  token_url <- paste(
    host,
    "sharing",
    "rest",
    "oauth2",
    "token",
    sep = "/"
  )

  auth_url <- paste(
    host,
    "sharing",
    "rest",
    "oauth2",
    "authorize",
    sep = "/"
  )


  client <- httr2::oauth_client(
    id = Sys.getenv("ARCGIS_CLIENT"),
    token_url = token_url
  )


  auth_url <- httr2::oauth_flow_auth_code_url(
    client,
    auth_url = auth_url,
    redirect_uri = "urn:ietf:wg:oauth:2.0:oob"
  )

  browseURL(auth_url)
  {
    code <- readline(prompt = "Enter code: ")
  }

  # use the internal fucntion from httr2 to complete the token
  # transfer
  fx <- getFromNamespace("oauth_client_get_token", "httr2")

  fx(
    client,
    grant_type = "authorization_code",
    code = code,
    redirect_uri = "urn:ietf:wg:oauth:2.0:oob"
  )


}

#' @export
#' @rdname auth
set_auth_token <- function(token, quietly = FALSE) {

  stopifnot(inherits(token, c("httr2_token", "character")))

  if (inherits(token, "httr2_token")) {
    Sys.setenv("ARCGIS_TOKEN" = token[["access_token"]])
  } else if (inherits(token, "character")) {
    Sys.setenv("ARCGIS_TOKEN" = token[["access_token"]])
  }

  if (quietly) return(invisible())

  cat("Token set to environment variable `ARCGIS_TOKEN`")
  invisible(token)
}







