# get_layer(): `FeatureServer` by ID

    Code
      get_layer(fsrv, 0)
    Output
      <FeatureLayer>
      Name: states_hex
      Geometry Type: esriGeometryPolygon
      CRS: 4326
      Capabilities: Query

# get_layer(): `FeatureServer` by name

    Code
      get_layer(fsrv, name = "states_hex")
    Output
      <FeatureLayer>
      Name: states_hex
      Geometry Type: esriGeometryPolygon
      CRS: 4326
      Capabilities: Query

# get_layer(): `MapServer` by ID

    Code
      get_layer(msrv, 3)
    Output
      <FeatureLayer>
      Name: High Resolution 30cm Imagery
      Geometry Type: esriGeometryPolygon
      CRS: 3857
      Capabilities: Map,Query,Data,Tilemap

# get_layer(): `MapServer` by name

    Code
      get_layer(msrv, name = "Citations")
    Output
      <FeatureLayer>
      Name: Citations
      Geometry Type: esriGeometryPolygon
      CRS: 3857
      Capabilities: Map,Query,Data,Tilemap

# get_layer(): `GroupLayer` by ID

    Code
      get_layer(glyr, 2)
    Output
      <FeatureLayer>
      Name: Bus Routes
      Geometry Type: esriGeometryPolyline
      CRS: 2248
      Capabilities: Map,Query,Data

# get_layer(): `GroupLayer` by name

    Code
      get_layer(glyr, name = "Bus Stops")
    Output
      <FeatureLayer>
      Name: Bus Stops
      Geometry Type: esriGeometryPoint
      CRS: 2248
      Capabilities: Map,Query,Data

