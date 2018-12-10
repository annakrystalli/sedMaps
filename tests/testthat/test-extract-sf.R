context("test-extract-sf")

rst <- raster::stack(system.file("testdata", "raster", "sed_maps.grd",
                          package = "sedMaps"))

sf <- readRDS(system.file("testdata", "sf", "jncc_regional_seas.rds",
                                package = "sedMaps"))


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


c("values", "raster")





tmp <- tempdir()
out_dir <- file.path(tmp, "out")
dir.create(out_dir)

extr_sedmap_data(rst, sf, 
                 select_rst = NULL, select_sf = NULL,
                 output = c("summaries"),
                 fun = c("mean", "min", "max", "median", "sd"),
                 out_dir = out_dir)

test_that("multiplication works", {
  expect_equal(2 * 2, 4)
})
