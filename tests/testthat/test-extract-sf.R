context("test-extract-sf")

draw <- structure(list(type = "FeatureCollection", 
                       features = list(
                           structure(list(type = "Feature", properties = structure(list(`_leaflet_id` = 171L, 
                                                                                        feature_type = "rectangle"),
                                                                                   .Names = c("_leaflet_id",  "feature_type")),
                                          geometry = structure(list(type = "Polygon",  coordinates = list(list(list(-5.844727, 52.214339), 
                                                                                                               list( -5.844727, 59.866883),
                                                                                                               list(8.129883, 59.866883), 
                                                                                                               list(8.129883, 52.214339),
                                                                                                               list(-5.844727, 52.214339)))),
                                                               .Names = c("type",   "coordinates"))),
                                     .Names = c("type", "properties", "geometry")), 
                           structure(list(type = "Feature", properties = structure(list(`_leaflet_id` = 185L, 
                                                                                        feature_type = "marker"),
                                                                                   .Names = c("_leaflet_id",  "feature_type")),
                                          geometry = structure(list(type = "Point", 
                                                                    coordinates = list( -2.8125, 60.608542)),
                                                               .Names = c("type", "coordinates"))),
                                     .Names = c("type",     "properties", "geometry")),
                           structure(list(type = "Feature",
                                          properties = structure(list(`_leaflet_id` = 200L, feature_type = "polyline"), 
                                                                 .Names = c("_leaflet_id", "feature_type")), 
                                          geometry = structure(list(type = "LineString",
                                                                    coordinates = list(list(-7.602539, 51.618017), 
                                                                                       list(-5.449219, 50.652943),
                                                                                       list(-5.844727, 49.525208))),
                                                               .Names = c("type", "coordinates"))),
                                     .Names = c("type", "properties", "geometry")))), .Names = c("type", "features"))

out_sf <- readRDS(system.file("testdata", "out_sf.rds",
                              package = "sedMaps"))

test_that("loaded and drawn collated successfully",
          {expect_equal(collate_extr_shapes(sf, draw, leaflet_groups = c("draw", "loaded")), out_sf %>% mutate(id = 1:15))})


tmp <- tempdir()
out_dir <- file.path(tmp, "out")
dir.create(out_dir)
on.exit(rm(out_dir))

extr_sedmap_data(rst, sf, 
                 select_rst = NULL, select_sf = NULL,
                 output = c("summaries"),
                 fun = c("mean", "min", "max", "median", "sd"),
                 out_dir = out_dir,
                 rst_out_format = "stack",
                 select_sf_csv = FALSE,
                 attributes = get("attributes", envir = as.environment(".GlobalEnv")),
                 varnames = varnames)

test_that("summaries calculated correctly", {
    expect_equal(list.files(out_dir, recursive = T), c("metadata/attributes.csv", "metadata/extraction_sf.geojson", 
                                                       "sedmaps_summaries.csv"))
    expect_equal(names(readr::read_csv(file.path(out_dir, "sedmaps_summaries.csv"))),
                 c("id", "stat", names(rst)))
})
