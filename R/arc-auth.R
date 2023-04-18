
# Code flow ---------------------------------------------------------------


#' Authorization
#'
#' Authorize your R session to connect to an ArcGIS Portal. ArcGIS Online and ArcGIS Enterprise
#' utilize OAuth2 authorization flow.
#'
#' @details
#'
#' Create a OAuth2.0 ArcGIS Application at https://developers.arcgis.com/applications/
#'
#' - `set_auth_token()` is a helper function that takes an `httr2_token` as created by `auth_code()` or `auth_client()` and sets the `ARCGIS_TOKEN` environment variable

#' - `auth_user()` uses legacy username and password authorization using the `generateToken` endpoint
#' - `auth_binding()` fetches a token from the active portal set by arcgisbinding
#'
#' @param client an OAuth 2.0 developer application client ID. By default uses the
#'  environment variable `ARCGIS_CLIENT`.
#' @param secret an OAuth 2.0 developer application secret. By default uses the environment
#'   variable `ARCGIS_SECRET`.
#' @param host default `"https://www.arcgis.com`
#' @param expiration the duration of the token in minutes.
#'
#' @rdname auth
#' @export
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
# Client auth -------------------------------------------------------------
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
    token_params = list(expiration = expiration)
  )
}



# legacy auth -------------------------------------------------------------


# uses request ip does not support other forms
# use oauth as the recommended approach
#' @export
#' @rdname auth
auth_user <- function(
    username = Sys.getenv("ARCGIS_USER"),
    password = Sys.getenv("ARCGIS_PASSWORD"),
    host = "https://www.arcgis.com",
    expiration = 60
) {

  if (expiration > 21600) stop("`expiration` cannot be more than 15 days (21600)")

  burl <- paste0(host, "/sharing/rest/generateToken")

  req <- httr2::req_body_form(
    httr2::request(burl),
    username = username,
    password = password,
    client = "requestip",
    expiration = expiration,
    f = "json"
  )

  resp <- httr2::req_perform(req)

  token_raw <- httr2::resp_body_string(resp)

  token <- RcppSimdJson::fparse(token_raw)

  detect_errors(token)

  # return the token
  httr2::oauth_token(
    token[["token"]],
    expires_in = expiration
  )

}

# this is not exported for now. We may not need it.
#'
#' @rdname auth
#' @export
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




# refreshment -------------------------------------------------------------

#'
#' @rdname auth
#' @export
refresh_token <- function(
    token,
    client = Sys.getenv("ARCGIS_CLIENT"),
    host = "https://arcgis.com"
) {

  token_url <- paste(
    host,
    "sharing",
    "rest",
    "oauth2",
    "token",
    sep = "/"
  )

  cln <- httr2::oauth_client(client, token_url)

  if (is.null(token[["refresh_token"]])) {
    stop("`token` has expired and no `refresh_token` available")
  } else
    # if it has a refresh check to see if refresh hasn't expired
    if ((cur_time + token[["refresh_token_expires_in"]]) < cur_time) {
      stop("`refresh_token` has gone past its expiry")
    }

  # should be able to refresh, go ahead.
  httr2::oauth_flow_refresh(cln, token[["refresh_token"]])

}



# arcgisbinding -----------------------------------------------------------

#' @export
#' @rdname auth
auth_binding <- function() {
  rlang::check_installed("arcgisbinding")
  tkn <- arcgisbinding::arc.check_portal()
  httr2::oauth_token(res[["token"]])
}


#'
#' @rdname auth
#' @export
validate_or_refresh_token <- function(
    token,
    client = Sys.getenv("ARCGIS_CLIENT"),
    host = "https://arcgis.com",
    refresh_threshold = 0
) {

  cur_time <- as.numeric(Sys.time())
  # check if token is expired or expires within threshold
  # the idea being if the token is going to expire soon, refresh it
  if (token[["expires_at"]] - refresh_threshold <= cur_time) {
    # if it is refresh the token
    token <- refresh_token(client, host, token)
  } else {
    # if not return token
    token
  }

}





