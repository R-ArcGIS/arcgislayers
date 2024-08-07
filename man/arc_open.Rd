% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/arc-open.R
\name{arc_open}
\alias{arc_open}
\title{Open connection to remote resource}
\usage{
arc_open(url, token = arc_token())
}
\arguments{
\item{url}{The url of the remote resource. Must be of length one.}

\item{token}{your authorization token.}
}
\value{
Depending on the provided URL returns a \code{FeatureLayer}, \code{Table}, \code{FeatureServer}, \code{ImageServer}, or \code{MapServer}. Each of these objects is a named list containing the properties of the service.
}
\description{
Provided a URL, create an object referencing the remote resource.
The resultant object acts as a reference to the remote data source.
}
\details{
To extract data from the remote resource utilize \code{\link[=arc_select]{arc_select()}} for objects of class
\code{FeatureLayer} or \code{Table}. For \code{ImageServer}s, utilize \code{\link[=arc_raster]{arc_raster()}}.

\ifelse{html}{\href{https://lifecycle.r-lib.org/articles/stages.html#experimental}{\figure{lifecycle-experimental.svg}{options: alt='[Experimental]'}}}{\strong{[Experimental]}}
}
\examples{
\dontrun{
# FeatureLayer
furl <- paste0(
  "https://services3.arcgis.com/ZvidGQkLaDJxRSJ2/arcgis/rest/services/",
  "PLACES_LocalData_for_BetterHealth/FeatureServer/0"
)

arc_open(furl)

# Table
furl <- paste0(
  "https://services.arcgis.com/P3ePLMYs2RVChkJx/arcgis/rest/services/",
  "USA_Wetlands/FeatureServer/1"
)

arc_open(furl)

# ImageServer
arc_open(
  "https://landsat2.arcgis.com/arcgis/rest/services/Landsat/MS/ImageServer"
)

# FeatureServer
furl <- paste0(
  "https://services3.arcgis.com/ZvidGQkLaDJxRSJ2/arcgis/rest/services/",
  "PLACES_LocalData_for_BetterHealth/FeatureServer"
)

arc_open(furl)

# MapServer
map_url <- paste0(
  "https://services.arcgisonline.com/ArcGIS/rest/services/",
  "World_Imagery/MapServer"
)

arc_open(map_url)
}
}
\seealso{
arc_select arc_raster
}
