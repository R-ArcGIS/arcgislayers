library(sf)
library(sfheaders)


# points ------------------------------------------------------------------

m <- matrix(runif(200, -180, 180), ncol = 2)
pnts_sfc <- sfc_point(m)
pnt_sfg <- pnts_sfc[[1]]


# multipoint
mpnt_df <- as.data.frame(cbind(m, sort(rep(1:5, 100/5))))
colnames(mpnt_df) <- c("x", "y", "multipoint_id")

mpnt_sfc <- sfc_multipoint(mpnt_df, "x", "y", multipoint_id = "multipoint_id")
mpnt_sfg <- mpnt_sfc[[1]]



xyzm_mpnt <- sfc_multipoint(
  cbind(mpnt_df, z = runif(100), m = runif(100)),
  "x", "y", "z", "m", "multipoint_id"
)


# points with xy, xyz, xyzm
xyzm_pnt <- sfc_point(
  cbind(mpnt_df, z = runif(100), m = runif(100)),
  "x", "y", "z", "m"
)


xyz_pnt <- sfc_point(
  cbind(mpnt_df, z = runif(100), m = runif(100)),
  "x", "y", "z"
)

xyz_pnt <- sfc_point(
  cbind(mpnt_df, z = runif(100), m = runif(100)),
  "x", "y", "z"
)



# polygons ----------------------------------------------------------------


poly_sfc <- st_cast(sfdep::guerry[["geometry"]], "POLYGON")[1:20]
poly_sfg <- poly_sfc[[1]]

# multipolygon
mpoly_sfc <- c(
  st_combine(poly_sfc[1:5]),
  st_combine(poly_sfc[6:10]),
  st_combine(poly_sfc[11:15]),
  st_combine(poly_sfc[16:20])
)


mpoly_sfg <- mpoly_sfc[[1]]

# z value polygon
nc <- st_read(system.file("shape/nc.shp", package = "sf"))

xyzm_poly_df <- cbind(
  as.data.frame(st_coordinates(st_cast(nc, "POLYGON"))),
  z = runif(2529, 0, 25000)
)

xyz_poly <- sfc_polygon(
  xyzm_poly_df,
  x = "X",
  y = "Y",
  z = "z",
  polygon_id = "L2"
)


# LINESTRING --------------------------------------------------------------

lns <- sfnetworks::roxel$geometry[1:20]


xyzm_lns_df <- cbind(st_coordinates(lns), z = runif(72), m = runif(72)) |>
  as.data.frame()


xyzm_lns <-  sfc_linestring(
  xyzm_lns_df,
  x = "X",
  y = "Y",
  z = "z",
  "m",
  linestring_id = "L1"
)

# MULTILINESTRING ---------------------------------------------------------

library(dplyr)

roxy <- sfnetworks::roxel


mlns <- roxy |>
  semi_join(
    roxy |>
      st_drop_geometry() |>
      count(type) |>
      filter(n > 10)
  ) |>
  group_by(type) |>
  sample_n(5) |>
  summarise()


