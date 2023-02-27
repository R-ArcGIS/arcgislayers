# info needed to get a whole feature layer
#| 1. URL
#| 2. total number of features
#| 3. number of records per page

#| note that we can parallelize these requests using
#| httr2::multi_req_perform() and curl::new_pool()
#| this can make fetching data from AGOL super duper fast


# To Do's and considerations ----------------------------------------------

#> - Get data without geometry
#> - provide own where statements
#> - download data directly without reading into memory
#>  - (probably a lot faster)
#>  - also could optionally write to geoparquet
#> - ability to specify a maximum number of features to return
#>  - this would also require a check for paginated results where
#>    the `n_max` is greater than the total number of features
#>  - the objective of this is to not query too many features on accident
#>  - if someone does want all of this data the recommendation would be to
#>    store the data per page into a parquet file or something
#> - Consider token refreshing (if that is supported)

#> Implement `st_transform()` method for specifying the output CRS
#> filter() support stringr functions (LIKE)


# FEATURE TABLES
# https://services2.arcgis.com/j80Jz20at6Bi0thr/ArcGIS/rest/services/List_of_Providers/FeatureServer/27
# How to handle them in FeatureServer
# FeatureTable class


# Consider using memoise to speed up calls to the same endpoint


# Dates and date times? ---------------------------------------------------

#| how are dates handled from the API? Particularly how are timezones handled? Should all data be converted to a common time-zone?
