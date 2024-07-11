## R CMD check results

0 errors | 0 warnings | 1 note

* This is a new release with small bug fixes and 2 minor new functions.

-----

Previous notes: 

* ArcGIS is a brand name and not a software name which is why it is unquoted some places in the DESCRIPTION's. These being the `Title` field and in the `Description` where it says "ArcGIS location services."
* REST is not a software but an architecture so it is not quoted.
* Removed R from the title per request
* Replaced instances of `if (interactive())` with `\dontrun{}`. All of these examples require an authorization token that is generated interactively as well as modifies remote data sources. As such 
