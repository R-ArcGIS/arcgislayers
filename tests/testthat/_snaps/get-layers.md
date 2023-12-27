# get_layers(): FeatureServer ID

    Code
      get_layers(fsrv, 0:1)
    Output
      [[1]]
      <FeatureLayer>
      Name: PlacePoints
      Geometry Type: esriGeometryPoint
      CRS: 3785
      Capabilities: Query,Extract
      
      [[2]]
      <FeatureLayer>
      Name: PlaceBoundaries
      Geometry Type: esriGeometryPolygon
      CRS: 3785
      Capabilities: Query,Extract
      

# get_layers(): FeatureServer name

    Code
      get_layers(fsrv, name = c("Tracts", "ZCTAs"))
    Output
      [[1]]
      <FeatureLayer>
      Name: Tracts
      Geometry Type: esriGeometryPolygon
      CRS: 3785
      Capabilities: Query,Extract
      
      [[2]]
      <FeatureLayer>
      Name: ZCTAs
      Geometry Type: esriGeometryPolygon
      CRS: 3785
      Capabilities: Query,Extract
      

# get_layers(): MapServer ID

    Code
      get_layers(msrv, 1:2)
    Output
      [[1]]
      <FeatureLayer>
      Name: Census Block Group
      Geometry Type: esriGeometryPolygon
      CRS: 4269
      Capabilities: Map,Query,Data
      
      [[2]]
      <FeatureLayer>
      Name: Detailed Counties
      Geometry Type: esriGeometryPolygon
      CRS: 4269
      Capabilities: Map,Query,Data
      

# get_layers(): MapServer name

    Code
      get_layers(msrv, name = c("Census Block Points", "Census Block Group"))
    Output
      [[1]]
      <FeatureLayer>
      Name: Census Block Points
      Geometry Type: esriGeometryPoint
      CRS: 4269
      Capabilities: Map,Query,Data
      
      [[2]]
      <FeatureLayer>
      Name: Census Block Group
      Geometry Type: esriGeometryPolygon
      CRS: 4269
      Capabilities: Map,Query,Data
      

# get_layers(): GroupLayer ID

    Code
      get_layers(glyr, 1:2)
    Output
      [[1]]
      <FeatureLayer>
      Name: Bus Stops
      Geometry Type: esriGeometryPoint
      CRS: 2248
      Capabilities: Map,Query,Data
      
      [[2]]
      <FeatureLayer>
      Name: Bus Routes
      Geometry Type: esriGeometryPolyline
      CRS: 2248
      Capabilities: Map,Query,Data
      

# get_layers(): GroupLayer name

    Code
      get_layers(glyr, name = c("Bus Stops", "Bus Routes"))
    Output
      [[1]]
      <FeatureLayer>
      Name: Bus Stops
      Geometry Type: esriGeometryPoint
      CRS: 2248
      Capabilities: Map,Query,Data
      
      [[2]]
      <FeatureLayer>
      Name: Bus Routes
      Geometry Type: esriGeometryPolyline
      CRS: 2248
      Capabilities: Map,Query,Data
      

