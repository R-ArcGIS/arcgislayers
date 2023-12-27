# get_all_layers(): FeatureServer

    Code
      get_all_layers(fsrv)
    Output
      $layers
      $layers$`0`
      <FeatureLayer>
      Name: states_hex
      Geometry Type: esriGeometryPolygon
      CRS: 4326
      Capabilities: Query
      
      $layers$`1`
      <FeatureLayer>
      Name: states_con
      Geometry Type: esriGeometryPolygon
      CRS: 4326
      Capabilities: Query
      
      $layers$`2`
      <FeatureLayer>
      Name: hexagons
      Geometry Type: esriGeometryPolygon
      CRS: 4326
      Capabilities: Query
      
      

# get_all_layers(): MapLayer

    Code
      get_all_layers(msrv)
    Output
      $layers
      $layers$`0`
      <FeatureLayer>
      Name: Census Block Points
      Geometry Type: esriGeometryPoint
      CRS: 4269
      Capabilities: Map,Query,Data
      
      $layers$`1`
      <FeatureLayer>
      Name: Census Block Group
      Geometry Type: esriGeometryPolygon
      CRS: 4269
      Capabilities: Map,Query,Data
      
      $layers$`2`
      <FeatureLayer>
      Name: Detailed Counties
      Geometry Type: esriGeometryPolygon
      CRS: 4269
      Capabilities: Map,Query,Data
      
      $layers$`3`
      <FeatureLayer>
      Name: states
      Geometry Type: esriGeometryPolygon
      CRS: 4269
      Capabilities: Map,Query,Data
      
      

# get_all_layers(): GroupLayer

    Code
      get_all_layers(glyr)
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
      

