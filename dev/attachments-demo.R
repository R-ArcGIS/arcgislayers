# install the development version
# install.packages("pak")
# pak::pak("r-arcgis/arcgislayers")

# for authorizing to your portal
library(arcgisutils)

# for accessing feature service content
library(arcgislayers)

# authorize
set_arc_token(auth_user())

# URL to the Survey123 feature service with attachments
furl <- "https://services1.arcgis.com/hLJbHVT9ZrDIzK0I/arcgis/rest/services/v8_Wide_Area_Search_Form_Feature_Layer___a2fe9c/FeatureServer/0"

# create a reference to it in R
layer <- arc_open(furl)

# There are two parts to working with attachments:
# 1. Finding the attachments associated with a feature service
# 2. After finding the attachments, we can download them to our machine

# Query the attachments of a layer using query_layer_attachments()
# By default this returns metadata about every attachment in your
# feature service
att <- query_layer_attachments(layer)
att

# We can limit this by providing a SQL where clause to the
# definition_expression argument.
# For example to find all of the attachments that have the
# followup_status field set to `needs_followup` we can provide the
# following definition_expression
query_layer_attachments(
  layer, "followup_status = 'needs_followup'"
)

# To find all of the attachments from after a point in time
query_layer_attachments(
  layer, "start_time >= '2023-01-01'"
)

# To find attachments with a name that starts with `image0`
query_layer_attachments(
  layer,
  attachments_definition_expression = "att_name like 'image0%'"
)

# Find attachments that contain "20221005"
query_layer_attachments(
  layer,
  attachments_definition_expression = "att_name like '%20221005%'"
)


# to download the attachments:
# res <- download_attachments(
#   att,
#   "dev/field_images"
# )
