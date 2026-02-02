library(arcgisutils)

set_arc_token(auth_user())


# create fake feature service
res <- publish_layer(
  penguins[1:5, ],
  sprintf("test-add-attachments-%s", ulid::ulid())
)

# fetch layer attachment ID from AGOL UI
# navigate to the actual table and switch enable-attachments
layer <- arc_open("ca58d5cc44c540e6b54c7f8f3bc8d366") |>
  get_layer(0)

# download images to add to new feature service
furl <- "https://services1.arcgis.com/hLJbHVT9ZrDIzK0I/arcgis/rest/services/v8_Wide_Area_Search_Form_Feature_Layer___a2fe9c/FeatureServer/0"
layer <- arc_open(furl)

tmp <- tempdir()
att <- query_layer_attachments(layer)[1:5, ]
images <- download_attachments(att, tmp, overwrite = TRUE)


add_attachments(layer, 1:5L, images)
