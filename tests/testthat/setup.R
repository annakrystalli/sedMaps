library(dplyr)

rst <- raster::stack(system.file("testdata", "raster", "sed_maps.grd",
                                 package = "sedMaps"))
sf <- readRDS(system.file("testdata", "sf", "jncc_regional_seas.rds",
                          package = "sedMaps"))
varnames <- readRDS(here::here("sedmap_shiny/data/raster/varnames.rds"))
print(varnames)
load_spice(here::here("sedmap_shiny/data/metadata"))
