# info needed to get a whole feature layer
#| 1. URL
#| 2. total number of features
#| 3. number of records per page

#| note that we can parallelize these requests using
#| httr2::req_perform_parallel() and curl::new_pool()
#| this can make fetching data from AGOL super duper fast


# To Do's and considerations ----------------------------------------------

#> filter() support stringr functions (LIKE)

# Consider using memoise to speed up calls to the same endpoint



