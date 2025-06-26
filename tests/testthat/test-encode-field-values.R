test_that("encode_field_values() encodes field values", {
  skip_on_cran()
  layer <- arc_open(
    "https://geodata.baltimorecity.gov/egis/rest/services/Housing/dmxOwnership/MapServer/0"
  )

  res <- arc_select(layer, n_max = 100, where = "RESPAGCY <> '  '")
  encoded <- encode_field_values(res, layer)

  # get unique encoded vals
  encoded_vals <- sort(unique(encoded$RESPAGCY))

  # fetch domains and known values
  domains <- list_field_domains(layer)
  domain_vals <- domains[[c("RESPAGCY", "codedValues", "name")]]

  expect_true(all(encoded_vals %in% domain_vals))
})

test_that("encode_field_values() encodes field values when field is set", {
  skip_on_cran()
  flayer <- arc_open(
    "https://services1.arcgis.com/99lidPhWCzftIe9K/ArcGIS/rest/services/UtahRoads/FeatureServer/0"
  )

  res <- arc_select(flayer, n_max = 100)

  encoded <- encode_field_values(res, flayer, field = "CARTOCODE")

  # fetch domains and known values
  domains <- list_field_domains(flayer, field = "CARTOCODE")
  domain_vals <- domains[[c("CARTOCODE", "codedValues", "name")]]

  expect_true(all(encoded[["CARTOCODE"]] %in% domain_vals))

  # expect message for field w/ invalid codes
  expect_message(
    encode_field_values(res, flayer, field = "SPEED_LMT")
  )

  # expect no message for field w/ invalid codes if `codes = "replace-valid"`
  expect_no_message(
    encode_field_values(
      res,
      flayer,
      field = "SPEED_LMT",
      codes = "replace-valid"
    )
  )
})

test_that("encode_field_values() works with a range type", {
  feature_layer_arc <- structure(
    list(
      currentVersion = 11.2,
      id = 1L,
      name = "Bird_trapping_event",
      type = "Table",
      serviceItemId = "XXXXXXXXXX",
      cacheMaxAge = 30L,
      isView = TRUE,
      isUpdatableView = TRUE,
      sourceSchemaChangesAllowed = TRUE,
      displayField = "Date_time_arrival",
      description = "",
      copyrightText = "",
      defaultVisibility = TRUE,
      editFieldsInfo = list(
        creationDateField = "created_date",
        creatorField = "created_user",
        editDateField = "last_edited_date",
        editorField = "last_edited_user"
      ),
      editingInfo = list(
        lastEditDate = 1750271177494,
        schemaLastEditDate = 1750271177494,
        dataLastEditDate = 1750271048324
      ),
      relationships = NULL,
      isDataVersioned = FALSE,
      hasContingentValuesDefinition = FALSE,
      supportsAppend = TRUE,
      supportsCalculate = TRUE,
      supportsASyncCalculate = TRUE,
      supportsTruncate = FALSE,
      supportsAttachmentsByUploadId = TRUE,
      supportsAttachmentsResizing = TRUE,
      supportsRollbackOnFailureParameter = TRUE,
      supportsStatistics = TRUE,
      supportsExceedsLimitStatistics = TRUE,
      supportsAdvancedQueries = TRUE,
      supportsValidateSql = TRUE,
      supportsCoordinatesQuantization = TRUE,
      supportsLayerOverrides = TRUE,
      supportsTilesAndBasicQueriesMode = TRUE,
      supportsFieldDescriptionProperty = TRUE,
      supportsQuantizationEditMode = TRUE,
      supportsColumnStoreIndex = TRUE,
      supportsApplyEditsWithGlobalIds = TRUE,
      advancedQueryCapabilities = list(
        supportsPagination = TRUE,
        supportsQueryAttachmentsCountOnly = TRUE,
        supportsPaginationOnAggregatedQueries = TRUE,
        supportsQueryRelatedPagination = TRUE,
        supportsQueryWithDistance = TRUE,
        supportsReturningQueryExtent = FALSE,
        supportsStatistics = TRUE,
        supportsOrderBy = TRUE,
        supportsDistinct = TRUE,
        supportsQueryWithResultType = TRUE,
        supportsSqlExpression = TRUE,
        supportsTimeRelation = TRUE,
        supportsAdvancedQueryRelated = TRUE,
        supportsCountDistinct = TRUE,
        supportsPercentileStatistics = TRUE,
        supportsApproxPercentileStatistics = TRUE,
        supportsSpatialAggregationStatistics = TRUE,
        supportedSpatialAggregationStatistics = c(
          "EnvelopeAggregate",
          "CentroidAggregate",
          "ConvexHullAggregate"
        ),
        supportsLod = TRUE,
        supportsQueryWithLodSR = FALSE,
        supportedLodTypes = "geohash",
        supportsReturningGeometryCentroid = FALSE,
        supportsReturningGeometryEnvelope = TRUE,
        supportsQueryWithDatumTransformation = TRUE,
        supportsCurrentUserQueries = TRUE,
        supportsHavingClause = TRUE,
        supportsOutFieldSQLExpression = TRUE,
        supportsMaxRecordCountFactor = TRUE,
        supportsTopFeaturesQuery = TRUE,
        supportsQueryWithCacheHint = TRUE,
        supportedOperationsWithCacheHint = c(
          "query",
          "queryTopFilter",
          "queryAnalytics",
          "queryAttachments",
          "queryRelated",
          "queryBins"
        ),
        supportsQueryAnalytic = TRUE,
        supportsQueryBins = TRUE,
        supportsDefaultSR = TRUE,
        supportsFullTextSearch = TRUE,
        fullTextSearchCapabilities = list(
          supportsAccentSensitivity = TRUE,
          supportsStopWords = TRUE,
          supportsSearchOperator = TRUE,
          supportsOperator = TRUE
        )
      ),
      advancedQueryAnalyticCapabilities = list(
        supportsLinearRegression = TRUE,
        supportsTrendlineRegression = TRUE,
        supportsAsync = TRUE,
        supportsPercentileAnalytic = TRUE
      ),
      queryBinsCapabilities = list(
        supportsAsync = TRUE,
        supportsAutoIntervalBin = TRUE,
        supportsFixedIntervalBin = TRUE,
        supportsFixedBoundariesBin = TRUE,
        supportsDateBin = TRUE,
        supportsStackBy = TRUE,
        supportsSplitBy = TRUE,
        supportsFirstDayOfWeek = TRUE,
        supportsSnapToData = TRUE,
        supportsReturnFullIntervalBin = TRUE,
        supportsNormalization = TRUE,
        supportedStatisticTypes = c(
          "COUNT",
          "SUM",
          "AVG",
          "VAR",
          "STDDEV",
          "MIN",
          "MAX",
          "PERCENTILE_CONT",
          "PERCENTILE_DISC",
          "APPROX_PERCENTILE_CONT",
          "APPROX_PERCENTILE_DISC",
          "CentroidAggregate",
          "EnvelopeAggregate",
          "ConvexHullAggregate"
        ),
        supportedNormalizationTypes = c(
          "boxCox",
          "field",
          "inverse",
          "log",
          "naturalLog",
          "percentOfTotal",
          "squareRoot"
        )
      ),
      advancedEditingCapabilities = list(
        supportedSqlFormatsInCalculate = "standard",
        supportsAsyncApplyEdits = TRUE,
        supportsReturnEditResults = TRUE,
        supportsApplyEditsbyUploadID = TRUE,
        supportedApplyEditsUploadIDFormats = "JSON"
      ),
      infoInEstimates = "count",
      useStandardizedQueries = TRUE,
      allowGeometryUpdates = TRUE,
      hasAttachments = FALSE,
      viewSourceHasAttachments = FALSE,
      htmlPopupType = "esriServerHTMLPopupTypeNone",
      hasMetadata = TRUE,
      hasM = FALSE,
      hasZ = FALSE,
      objectIdField = "OBJECTID",
      uniqueIdField = list(
        name = "OBJECTID",
        isSystemMaintained = TRUE
      ),
      globalIdField = "GlobalID",
      typeIdField = "",
      collation = list(
        locale = "neutral",
        caseSensitive = FALSE,
        accentSensitive = TRUE
      ),
      fields = structure(
        list(
          name = c(
            "OBJECTID",
            "Bander",
            "Writer",
            "Event_ID",
            "Temperature_initial",
            "Cloud_cover",
            "Precipitation",
            "GlobalID",
            "Wind_speed_final_km_hr"
          ),
          type = c(
            "esriFieldTypeOID",
            "esriFieldTypeString",
            "esriFieldTypeString",
            "esriFieldTypeString",
            "esriFieldTypeDouble",
            "esriFieldTypeString",
            "esriFieldTypeString",
            "esriFieldTypeGlobalID",
            "esriFieldTypeDouble"
          ),
          alias = c(
            "OBJECTID",
            "Bander",
            "Writer",
            "Event ID",
            "Initial temperature",
            "Cloud cover",
            "Precipitation",
            "GlobalID",
            "Final wind speed (km/hr)"
          ),
          sqlType = c(
            "sqlTypeOther",
            "sqlTypeOther",
            "sqlTypeOther",
            "sqlTypeOther",
            "sqlTypeOther",
            "sqlTypeOther",
            "sqlTypeOther",
            "sqlTypeOther",
            "sqlTypeOther"
          ),
          nullable = c(FALSE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, FALSE, TRUE),
          editable = c(FALSE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, FALSE, TRUE),
          domain = list(
            NULL,
            list(
              type = "codedValue",
              name = "Bird trapping event_Bander_7651fc9c-4c20-4932-85e4-b53febc3b404",
              codedValues = structure(
                list(
                  name = c(
                    "ILxxx",
                    "NOxxx",
                    "RFxxx",
                    "KFxxx",
                    "KHxxx",
                    "MLxxx",
                    "ENxxx",
                    "OTxxx"
                  ),
                  code = c("IL", "NO", "RF", "KF", "KH", "ML", "EN", "OT")
                ),
                row.names = c(NA, 8L),
                class = "data.frame"
              )
            ),
            list(
              type = "codedValue",
              name = "Bird trapping event_Writer_3b01a58b-b708-4225-8156-323a056aa046",
              codedValues = structure(
                list(
                  name = c(
                    "ILxxx",
                    "NOxxx",
                    "RFxxx",
                    "KFxxx",
                    "KHxxx",
                    "MLxxx",
                    "ENxxx",
                    "OTxxx"
                  ),
                  code = c("IL", "NO", "RF", "KF", "KH", "ML", "EN", "OT")
                ),
                row.names = c(NA, 9L),
                class = "data.frame"
              )
            ),
            NULL,
            list(
              type = "range",
              name = "196a6c923a3-layer-16_Temperature_initial_d253d6d3-08eb-4e4d-9a06-1817faf563c1",
              range = c(-10L, 50L)
            ),
            list(
              type = "codedValue",
              name = "Bird trapping event_Cloud_cover_542fe1d4-b416-42af-8859-a81077868657",
              mergePolicy = "esriMPTDefaultValue",
              splitPolicy = "esriSPTDefaultValue",
              codedValues = structure(
                list(
                  name = c(
                    "clear",
                    "partial cloud",
                    "cloudy, then clear",
                    "clear, then cloudy",
                    "overcast",
                    "unknown"
                  ),
                  code = c(
                    "clear",
                    "partial_cloud",
                    "cloudy_then_clear",
                    "clear_then_cloudy",
                    "overcast",
                    "unknown"
                  )
                ),
                row.names = c(NA, 6L),
                class = "data.frame"
              )
            ),
            list(
              type = "codedValue",
              name = "Bird trapping event_Precipitation_1e04fd22-9ada-4418-88dc-e4d883b32be3",
              mergePolicy = "esriMPTDefaultValue",
              splitPolicy = "esriSPTDefaultValue",
              codedValues = structure(
                list(
                  name = c(
                    "no rain",
                    "drizzle",
                    "light rain",
                    "intermittent rain",
                    "rain",
                    "unknown"
                  ),
                  code = c(
                    "no_rain",
                    "drizzle",
                    "light_rain",
                    "intermittent_rain",
                    "rain",
                    "unknown"
                  )
                ),
                row.names = c(NA, 6L),
                class = "data.frame"
              )
            ),
            NULL,
            NULL
          ),
          defaultValue = c(NA, NA, NA, NA, NA, NA, NA, NA, NA),
          length = c(NA, 256L, 256L, 100L, NA, 256L, 256L, 38L, NA)
        ),
        row.names = c(NA, 9L),
        class = "data.frame"
      ),
      indexes = structure(
        list(
          name = c("PK__BIRD_TRA__F4B70D85488DFBDF", "FDO_GlobalID"),
          fields = c("OBJECTID", "GlobalID"),
          isAscending = c(TRUE, TRUE),
          isUnique = c(TRUE, TRUE),
          description = c("", ""),
          indexType = c("Attribute", "Attribute")
        ),
        row.names = 1:2,
        class = "data.frame"
      ),
      dateFieldsTimeReference = list(
        timeZone = "UTC",
        respectsDaylightSaving = FALSE
      ),
      preferredTimeReference = NULL,
      types = NULL,
      templates = structure(
        list(
          name = "Bird_trapping_event",
          description = "",
          drawingTool = "esriFeatureEditToolNone",
          prototype = list(list(
            attributes = list(
              Event_notes = NULL,
              ParentGlobalID = NULL,
              Date_time_arrival = NULL,
              Bander = NULL,
              Writer = NULL,
              Event_ID = NULL,
              Temperature_initial = NULL,
              Temperature_final = NULL,
              Cloud_cover = NULL,
              Precipitation = NULL,
              Total_captures = NULL,
              Total_tags = NULL,
              Wind_speed_initial_km_hr = NULL,
              Wind_speed_final_km_hr = NULL
            )
          ))
        ),
        row.names = 1L,
        class = "data.frame"
      ),
      supportedQueryFormats = "JSON, geoJSON, PBF",
      supportedAppendFormats = "sqlite,geoPackage,shapefile,filegdb,featureCollection,geojson,csv,excel,jsonl,featureService,pbf",
      supportedAppendSourceFilterFormats = "sqlite,geoPackage,shapefile,filegdb,featureService",
      supportedExportFormats = "csv,shapefile,sqlite,geoPackage,filegdb,featureCollection,geojson,kml,excel",
      supportedConvertFileFormats = "JSON,PBF",
      supportedConvertContentFormats = "LayerEditCollection,FeatureCollection",
      supportedContingentValuesFormats = "JSON, PBF",
      supportedSyncDataOptions = 260L,
      hasStaticData = FALSE,
      maxRecordCount = 2000L,
      standardMaxRecordCount = 32000L,
      tileMaxRecordCount = 8000L,
      maxRecordCountFactor = 1L,
      capabilities = "Query",
      viewDefinitionQuery = "OBJECTID = 5",
      definitionQuery = "OBJECTID = 5",
      url = "XXXXXXXXXXXXXXXX"
    ),
    class = "Table",
    query = list()
  )

  feature_layer_data <- structure(
    list(
      OBJECTID = 13,
      Bander = "IL",
      Writer = "ML",
      Event_ID = "NB03_20250520",
      Temperature_initial = 11.2,
      Cloud_cover = "partial_cloud",
      Precipitation = "no_rain",
      GlobalID = "7a11e0b7-eb25-457b-b401-29a7ace5f336",
      Wind_speed_final_km_hr = 0
    ),
    row.names = c(NA, -1L),
    class = "data.frame"
  )

  feature_layer_arc

  encode_field_values(
    feature_layer_data,
    feature_layer_arc,
    field = NULL
  )
})
