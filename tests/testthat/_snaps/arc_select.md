# arc_select() works on `ImageServer`s

    Code
      tmp
    Output
      Simple feature collection with 2 features and 25 fields
      Geometry type: MULTIPOLYGON
      Dimension:     XY
      Bounding box:  xmin: -5034779 ymin: 9132387 xmax: -4335570 ymax: 10064260
      Projected CRS: WGS 84 / Pseudo-Mercator
        OBJECTID                                     Name MinPS MaxPS LowPS HighPS
      1       99 LC08_L1GT_001014_20200218_20200823_02_T2     0   300    30    120
      2      106 LC08_L1GT_001015_20200202_20200823_02_T2     0   300    30    120
        Category                                    GroupName ProductName  CenterX
      1        1 LC08_L1GT_001014_20200218_20200823_02_T2_MTL      Level1 -4629722
      2        1 LC08_L1GT_001015_20200202_20200823_02_T2_MTL      Level1 -4767010
        CenterY SensorName     AcquisitionDate SunAzimuth SunElevation CloudCover
      1 9766962  Landsat 8 2020-02-18 14:12:23   168.0054    12.205823     1.0000
      2 9409622  Landsat 8 2020-02-02 14:12:50   167.4229     8.295621     0.4544
             Best DateUpdated   PR Latest DayOfYear Month      LANDSAT_SCENE_ID
      1 114199232        <NA> 1014     NA        14     2 LC80010142020049LGN00
      2  59199217        <NA> 1015     NA        15     2 LC80010152020033LGN00
                                      LANDSAT_PRODUCT_ID dataset_id
      1 LC08_L1GT_001014_20200218_20200823_02_T2_MTL.txt   Landsat8
      2 LC08_L1GT_001015_20200202_20200823_02_T2_MTL.txt   Landsat8
                              geometry
      1 MULTIPOLYGON (((-4335570 98...
      2 MULTIPOLYGON (((-4489331 95...

