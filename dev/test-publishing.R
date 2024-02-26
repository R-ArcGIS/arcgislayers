
set_arc_token(auth_code())

# PUBLISHING NON 3857 CRS does not work.
x <- .data <- sf::st_set_crs(sfdep::guerry[10:22,3], 27572)

debugonce(add_item)
publish_layer(x, "Poly 2572")



