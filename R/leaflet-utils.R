#' Plot leaflet basemap
#'
#' @param rst sedMaps `rasterStack`
#'
#' @return
#' @export
#'
#' @examples
lflt_basemap <- function(rst, basemap = "Esri.OceanBasemap"){
    
    xt <- raster::extent(rst)
    # plot
    leaflet() %>% 
        addTiles()  %>% 
        fitBounds(xt@xmin , xt@ymin , xt@xmax , xt@ymax) %>%
        addProviderTiles(providers[[basemap]])
}

#' Plot selected rst layer 
#'
#' Plot selected rst layer reactively using leafletProxy
#' @param rst sedMaps `rasterStack`
#' @param varname selected sedMaps `rasterStack` layer
#' @param option selected `viridis` palette option
#' @param label legend label. Defaults to `varname`
#' @param opacity layer opacity
#'
#' @return
#' @export
#'
#' @examples
lflt_rst_selected <- function(rst, varname = NULL,
                              option = "A", label = NULL,
                              opacity = 0.8){
    
    if(is.null(varname)){ varname <- names(rst)[1]}else{
        varname <- match.arg(varname, choices = names(rst))}
    
    # ---- subset-rst ----
    r <- raster::subset(rst, subset = varname)
    
    # ---- get palette ----
    pal <- leaflet::colorNumeric(viridis::viridis(100, option = option), 
                                 raster::values(r),
                                 na.color = "transparent")
    # ---- get-contour ----
    contour  <- raster::rasterToContour(r) 
    
    # ---- set-label -----
    if(is.null(label)) label <- varname
    
    leaflet::leafletProxy(mapId = "leaflet") %>%
        leaflet::clearGroup(group = "raster") %>%
        leaflet::addRasterImage(r, colors = pal, 
                                opacity = opacity,
                                layerId = "rst",
                                group = "raster") %>%
        leaflet::addPolylines(color = "white", 
                              data = contour,
                              weight = 0.5,
                              group = "raster") %>%
        leaflet::addLegend(pal = pal, 
                           values = raster::values(r),
                           label = label,
                           title = label,
                           layerId = "legend",
                           group = "raster") 
    
}
#' Add sf vector selection layer
#'
#' @param map leaflet map object
#' @param sf sf object containing vector data
#' @param pal_f character vector of colors
#' @param ... additional variables passed to `addPolylines()`.
#'
#' @return a leaflet map object with sf vector data overlaid
#' @export
#'
#' @examples
lflt_sf <- function(map, sf, pal_f =  topo.colors(10), ...){
    selected <- prep_sf(sf, ids = NULL, pal_f) 
    
    leaflet::leafletProxy(mapId = "leaflet") %>%
        leaflet::addPolygons( data = selected,
                              layerId = ~as.character(id),
                              group = "loaded",
                              opacity = 0.6, 
                              color = "white",
                              fillOpacity = 0.2,
                              weight = 2, 
                              ...) %>%
        leaflet::addLayersControl(overlayGroups = c('draw', "loaded"), options =
                             layersControlOptions(collapsed=TRUE))
}



#' Highlight selected sf polygons
#'
#' @param sf sf object containing vector data
#' @param ids character vector of selected `id` ids
#' @param pal_f character vector of colors
#' @param ... additional variables passed to `addPolylines()`.
#'
#' @return 
#' @export
#'
#' @examples
lflt_sf_selected <- function(sf, ids, pal_f =  topo.colors(10), ...){
    
    selected <- prep_sf(sf, ids, pal_f) %>%
        dplyr::filter(select == TRUE)
    
    leaflet::leafletProxy(mapId = "leaflet") %>%
        leaflet::addPolygons( data = selected,
                              layerId = ~id,
                              group = "draw",
                              opacity = 1, 
                              color = "blue",
                              fillOpacity = 0.85,
                              weight = 4, 
                              ...)
}

lflt_factpal <- function(sf, pal_f = topo.colors(10)){
    leaflet::colorFactor(pal_f, as.factor(sf$id))
}

prep_sf <- function(sf, ids = NULL, pal_f =  topo.colors(10)){
    
    factpal <- lflt_factpal(sf, pal_f)
    sf %>% sf::st_transform(sf, crs = 4326) %>% 
        dplyr::mutate(select = if(is.null(ids)){
            FALSE}else{
                if(ids[1] == "all"){ids <- sf$id}
                id %in% ids},
            fillOpacity = dplyr::case_when(
                select == TRUE ~ 0.75,
                select == FALSE ~ 0.3),
            color = "white",
            weight = dplyr::case_when(
                select == TRUE ~ 2.8,
                select == FALSE ~ 1.3),
            opacity = dplyr::case_when(
                select == TRUE ~ 1,
                select == FALSE ~ 0.5))
}


update_click_selection <- function(click.list, click){
    click.list$id <- click$id
    if(click$id %in% click.list$ids){
        click.list$ids[click.list$ids != click$id]
    }else{
        c(click.list$ids, click$id)
    }
}