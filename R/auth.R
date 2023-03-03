
# Client auth -------------------------------------------------------------


#' Authentication
#'
#' @export
#' @rdname auth
auth_client <- function(client = Sys.getenv("ARCGIS_CLIENT"),
                        secret = Sys.getenv("ARCGIS_SECRET"),
                        host = "https://www.arcgis.com",
                        expiration = 120) {
  # https://developers.arcgis.com/documentation/mapping-apis-and-services/security/application-credentials/
  token_url <- paste(
    host,
    "sharing",
    "rest",
    "oauth2",
    "token",
    sep = "/"
  )

  cln <- httr2::oauth_client(client, token_url, secret)
  httr2::oauth_flow_client_credentials(
    cln,
    token_params = list(expiration = 120)
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


# legacy auth -------------------------------------------------------------


# uses request ip does not support other forms
# use oauth as the recommended approach
#' @export
#' @rdname auth
auth_password <- function(
    username = Sys.getenv("ARCGIS_USER"),
    password = Sys.getenv("ARCGIS_PASSWORD"),
    host = "https://www.arcgis.com",
    expiration = 60
) {

  if (expiration > 21600) stop("`expiration` cannot be more than 15 days (21600)")

  burl <- paste0(host, "/sharing/rest/generateToken")
  req <- httr2::request(burl) |>
    httr2::req_body_form(
      username = username,
      password = password,
      client = "requestip",
      expiration = expiration,
      f = "json"
    )

  resp <- httr2::req_perform(req)

  token <- httr2::resp_body_json(resp)

  # handle errors
  if (is.null(token[["token"]])) {
    stop(
      "\n  Status code: ", token[["error"]][["code"]], "\n  ", token$error$message,
      call. = FALSE
    )
  }

  # return the token
  httr2::oauth_token(
    token[["token"]],
    expires_in = expiration
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








