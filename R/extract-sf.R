extr_sedmap_data <- function(rst, sf, 
                             select_rst = NULL,
                             select_sf = NULL,
                             output = "summaries",
                             fun = c("mean", "min", "max"),
                             sf_crs = FALSE){
    #on.exit(rm(tmp))
    
    if(sf_crs){
        rst <- raster::projectRaster(rst, crs = sf::st_crs(sf)$proj4string)
    }else{
        sf <- sf::st_transform(sf, raster::projection(rst))
    }
    
    if(!is.null(select_rst)){
        select_rst <- match.arg(select_rst, 
                                names(rst),
                                several.ok = TRUE)
        rst <- raster::subset(rst, select_rst)
    }
    if(!is.null(select_sf)){
        select_sf <- match.arg(select_sf, 
                               sf$id,
                               several.ok = TRUE)
        sf <- sf %>% filter(id %in% select_sf)
    }
    output <- match.arg(output, c("summaries", "values", "raster"))
    
    tmp <- tempdir()
    
    if("summaries" %in% output){
        extr_summaries(rst, sf, fun) %>%
            readr::write_csv(file.path(tmp, "sedmaps_summaries.csv"))
    }
    if("values" %in% output){
        extr_values(rst, sf) %>%
            readr::write_csv(file.path(tmp, "sedmaps_values.csv"))
    }
    if("raster" %in% output){
        extr_raster(rst, sf) %>%
            raster::writeRaster(filename="raster.grd",
                                overwrite=TRUE)
    }
    
}







extr_values <-  function(rst, sf)  {
    raster::extract(rst, sf, cellnumbers = T) %>%
        purrr::map2(sf$id, 
                    ~tibble::as_tibble(.x) %>%
                        mutate(id = .y)) %>%
        do.call(dplyr::bind_rows, .) %>% 
        dplyr::bind_cols(
            raster::xyFromCell(rst, .$cell) %>%
                tibble::as.tibble()) %>%
        select(id, cell, x, y, everything()) %>%
        arrange(as.numeric(id), cell)
    
}

extr_summaries <- function(rst, sf, 
                           fun = c("mean", "min", "max")){
    fun <- match.arg(fun, 
                     c("mean", "max", "min", "median", "sd"),
                     several.ok = TRUE)
    
    purrr::map_df(fun, 
                  ~raster::extract(
                      rst, sf, 
                      fun = .x, 
                      na.rm = T) %>%
                      tibble::as.tibble() %>% 
                      dplyr::mutate(id = sf$id, 
                                    stat = .x) %>%
                      select(id, stat, everything())) %>%
        arrange(as.numeric(id))
}


extr_raster <- function(rst, sf){    
    raster::stack(
        raster::rasterize(sf, rst) %>% 
            setNames("sf") %>%
            raster::ratify(),
        raster::mask(rst, sf))
}

#' Title
#'
#' @param feature 
#'
#' @return
#' @export
#'
#' @examples
drawFeature2sf <- function(feature){
    type <- feature$geometry$type
    id <- feature[["properties"]][["_leaflet_id"]]
    
    wkt <- switch (type,
                   "Polygon" = sf::st_polygon(
                       list(matrix(unlist(
                           feature$geometry$coordinates[[1]]),
                           ncol=2,
                           byrow=TRUE))),
                   "LineString" = sf::st_linestring(
                       matrix(unlist(
                           feature$geometry$coordinates),
                           ncol=2,
                           byrow=TRUE)),
                   "Point" = sf::st_point(unlist(
                           feature$geometry$coordinates)))
    
    sf::st_sf(id = feature[["properties"]][["_leaflet_id"]],
              descr = glue::glue(
                  'drawn {type} {id}'),
              geometry = sf::st_sfc(wkt, crs = 4326)) %>% 
        mutate(area = sf::st_area(.))
}