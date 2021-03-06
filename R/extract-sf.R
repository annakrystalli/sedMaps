#' Extract sedmap data
#'
#' @param rst sedmaps data raster stack
#' @param sf extraction simple feature
#' @param select_rst raster layers to be extracted
#' @param select_sf sf layers to use for extraction
#' @param output character vector of extraction output formats
#' @param fun summary functions to apply for each data x extraction layer. `sd`
#'  ignored for points
#' @param sf_crs whether output data should be converted to the sf crs
#' @param out_dir path to output directory
#' @param rst_out_format whether raster data should be return as a `.grd` raster 
#' stack or individual geoTIFF layers
#' @param select_sf_csv whether to additionally include extraction sf metadata as in `.csv` format
#' @param attributes dataspice attributes table 
#' @param varnames varnames list
#'
#' @return outputs written out to `out_dir`.
#' @export
#'
#' @importFrom stats sd median
extr_sedmap_data <- function(rst, sf, 
                             select_rst = NULL,
                             select_sf = NULL,
                             output = c("summaries", "csv", "raster"),
                             fun = c("mean", "min", "max", "median", "sd"),
                             sf_crs = FALSE,
                             out_dir,
                             rst_out_format = c("stack", "tiff"),
                             select_sf_csv = FALSE,
                             attributes,
                             varnames){
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
        sf <- sf %>% dplyr::filter(id %in% select_sf)
    }
    
    output <- match.arg(output, several.ok = T)
    fun <- match.arg(fun, several.ok = T)
    rst_out_format <- match.arg(rst_out_format)
    
    
    
    if("summaries" %in% output){
        extr_summaries(rst, sf, fun) %>%
            readr::write_csv(path = file.path(out_dir, "sedmaps_summaries.csv"))
    }
    if("csv" %in% output){
        extr_values(rst, sf) %>%
            readr::write_csv(path = file.path(out_dir, "sedmaps_values.csv"))
    }
    if("raster" %in% output){
        extr_raster(rst, sf) %>%
            rst_export(format = rst_out_format, out_dir = out_dir)
    }
    
    # ---- create metadata ----
    dir.create(file.path(out_dir, "metadata"), showWarnings = FALSE)
    # extract and write attributes
    extr_attributes(rst, output, attributes, varnames) %>%
        readr::write_csv(path = file.path(out_dir, "metadata", "attributes.csv"))
    # write extraction sf as geojson
    sf::st_write(sf, file.path(out_dir, "metadata", "extraction_sf.geojson"))
    # write extraction metadata as csv if selected
    if(select_sf_csv){
        sf %>% st_set_geometry(NULL) %>%
            readr::write_csv(path = file.path(out_dir, "metadata", "extraction_sf.csv"))
    }
}

extr_attributes <- function(rst, output, attributes, varnames){
    output <- c(output, "sf")
    
    fileNames <- c(csv = "values",
      raster = "sed_maps.rds",
      summaries = "summaries",
      sf = "extraction_sf")[output]
    
    purrr::map_df(fileNames,
           ~dplyr::filter(attributes, stringr::str_detect(.data$fileName, .x))) %>%
        dplyr::distinct(.data$variableName, .data$description, .data$unitText) %>%
        dplyr::filter(!.data$variableName %in% inv_varnames(rst, varnames))
    
}

inv_varnames <- function(rst, varnames){
    names(rst)[!names(rst) %in% unlist(varnames)]
}


fun_choices <- function(){
    args(extr_sedmap_data) %>% as.list() %>% 
        .[["fun"]] %>% as.character() %>% .[-1]
}

extr_values <-  function(rst, sf)  {
    raster::extract(rst, sf, cellnumbers = T) %>%
        purrr::map2(sf$id, 
                    ~tibble::as_tibble(.x) %>%
                        dplyr::mutate(id = .y)) %>%
        do.call(dplyr::bind_rows, .) %>% 
        dplyr::bind_cols(
            raster::xyFromCell(rst, .$cell) %>%
                tibble::as.tibble()) %>%
        dplyr::select(id, cell, x, y, dplyr::everything()) %>%
        dplyr::arrange(as.numeric(id), cell)
    
}

extr_summaries <- function(rst, sf, 
                           fun){
    prop_data <- function(x, ...) length(na.omit(x))/length(x)
    fun <- match.arg(fun, choices = fun_choices(),
                     several.ok = TRUE)
    purrr::map_df(fun, 
                  ~suppressWarnings(raster::extract(
                      rst, sf, 
                      fun = .x, 
                      na.rm = T)) %>%
                      tibble::as.tibble() %>% 
                      dplyr::mutate(id = sf$id, 
                                    stat = .x) %>%
                      dplyr::select(id, stat, dplyr::everything())) %>%
        clean_non_finite() %>%
        dplyr::arrange(id)
}

extr_raster <- function(rst, sf){    
    # arrange sf
    sf <- dplyr::arrange(sf, id)
    
    # rasterise / ratify
    rat <- sf %>% dplyr::rename("ID" = id) %>% 
        sf::st_set_geometry(NULL) %>%
        dplyr::select(ID, descr)
    
    
    rst_sf <-  raster::rasterize(sf, rst[[1]]) %>% 
        stats::setNames("sf_id") %>%
        raster::ratify()
    levels(rst_sf) <- rat
    
    raster::stack(rst_sf, raster::mask(rst, sf)) 
}

#' Convert a drawn leaflet feature to sf
#'
#' @param feature drawn leaflet feature
#'
#' @return a simple feature object of the leaflet feature
#' @export
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
    
    sf::st_sf(source = "user",
              id = id,
              descr = glue::glue('Drawn Leaflet {type}: {id}'),
              geometry = sf::st_sfc(wkt, crs = 4326)) %>% 
        dplyr::mutate(area = sf::st_area(.))
}


#' Collate extraction shapes
#'
#' @param sf loaded in-built sf
#' @param draw list of drawn leaflet features 
#' @param leaflet_groups character vector of selected leaflet groups
#'
#' @return sf of input sf and draw features
#' @export
collate_extr_shapes <- function(sf, draw, leaflet_groups){
    if(!"draw" %in% leaflet_groups){draw <- NULL}
    if(!"loaded" %in% leaflet_groups){sf <- NULL}
    
    if(!is.null(sf)){
        sf_max_id <- max(as.numeric(sf$id))
    }else{sf_max_id <- 0}
    
    if(!is.null(draw)){
        draw_sf <- purrr::map(draw$features, 
                              ~drawFeature2sf(.x)) %>% 
            do.call(rbind, .) %>%
            dplyr::mutate(id = as.numeric(rownames(.)) + sf_max_id) 
    }else{
        draw_sf <- NULL
    }
    rbind(sf, draw_sf) %>%
        groom_sf()
}

groom_sf <- function(sf){
    if(is.null(sf)){sf}else{
        dplyr::mutate(sf, id = as.integer(id)) %>%
        dplyr::arrange(id)}
}


rst_export <- function(rst_out, 
                       format, 
                       out_dir){
    switch(format,
           "tiff" = {
               tiff_path <- file.path(out_dir, "raster")
               dir.create(tiff_path, showWarnings = FALSE)
               
               raster::writeRaster(rst_out, 
                                   filename = file.path(tiff_path, "sedmaps.tif"),
                                   bylayer = T, suffix = names(rst_out))},
           "stack" = {
               raster::writeRaster(rst_out, 
                                   filename = file.path(out_dir, 
                                                        "sedmaps_stack.grd"))
           })
}

non_finite2na <- function(x){
    x[!is.finite(x)] <- NA
    x}

clean_non_finite <- function(df){
    purrr::map_if(df, is.numeric, ~non_finite2na(.x)) %>%
        tibble::as_tibble() %>% dplyr::mutate(id = as.integer(id))
}

