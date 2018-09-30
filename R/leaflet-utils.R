#' Title
#'
#' @param rst 
#' @param varname 
#' @param basemap 
#' @param option 
#' @param label 
#' @param opacity 
#'
#' @return
#' @export
#'
#' @examples
lflt_plot <- function(rst, varname = NULL, basemap = "Esri.OceanBasemap",
                      option = "A", label = NULL,
                      opacity = 0.8){
    
    if(is.null(varname)){ varname <- names(rst)[1]}else{
        varname <- match.arg(varname, choices = names(rst))}
    print(var)
    
    if(is.null(label)) label <- varname
    # subset var
    r <- raster::subset(rst, subset = varname)

    # get palette
    pal <- leaflet::colorNumeric(viridis::viridis(100, option = option), 
                                 raster::values(r),
                                 na.color = "transparent")
    # plot
    leaflet() %>% 
        addTiles()  %>% 
        addProviderTiles(providers[[basemap]]) %>%
        addRasterImage(r, colors = pal, opacity = opacity) %>%
        lflt_contour(r) %>%
        addLegend(pal = pal, 
                  values = raster::values(r),
                  label = varname,
                  title = varname) 
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
lflt_sf <- function(map, sf = NULL, pal_f =  topo.colors(10), ...){
    if(is.null(sf)){map}else{
        sf <- prep_sf(sf)
        factpal <- lflt_factpal(sf, pal_f)
        map %>%
            mapview::addFeatures(data = sf, 
                                 layerId = ~LABEL,
                                 opacity = 0.8, 
                                 color = ~factpal(as.factor(LABEL)),
                                 ...)
    }
}



#' Highlight selected sf polygons
#'
#' @param sf sf object containing vector data
#' @param ids character vector of selected `LABEL` ids
#' @param pal_f character vector of colors
#' @param ... additional variables passed to `addPolylines()`.
#'
#' @return 
#' @export
#'
#' @examples
lflt_sf_selected <- function(sf, ids, pal_f =  topo.colors(10), ...){
    
    factpal <- lflt_factpal(sf, pal_f)
    selected_shapes <- prep_sf(sf) %>% 
        filter(LABEL %in% ids)
    
    leaflet::leafletProxy(mapId = "leaflet") %>%
        leaflet::addPolygons( data = selected_shapes,
                      layerId = ~LABEL,
                      opacity = 0.92, 
                      color = ~factpal(as.factor(LABEL)), 
                      ...)
}

lflt_cor <- function(rst, varname1 = NULL, varname2 = NULL,
                     basemap = "Esri.OceanBasemap",
                     option = "magma", label = NULL,
                     opacity = 0.8){
    if(is.null(varname1)){ varname1 <- names(rst)[1]}else{
        varname1 <- match.arg(varname1, choices = names(rst))}
    if(is.null(varname2)){ varname2 <- names(rst)[2]}else{
        varname2 <- match.arg(varname2, choices = names(rst))}
    
    r <- raster::layerStats(
        raster::subset(rst, subset = c(varname1, varname2)),
        #raster::subset(rst, subset = varname2)),
        'pearson', na.rm=T)
    
    # get palette
    pal <- leaflet::colorNumeric(viridis::viridis(100, option = option), 
                                 raster::values(r),
                                 na.color = "transparent")
    # plot
    leaflet()
        addTiles()  %>% 
        addProviderTiles(providers[[basemap]]) %>%
        addRasterImage(r, colors = pal, opacity = opacity) %>%
        lflt_contour(r) %>%
        addLegend(pal = pal, 
                  values = raster::values(r),
                  label = varname,
                  title = varname) 
}

#' add contour map derived from raster
#'
#' @param map a leaflet map widget
#' @param r a raster to contour
#' @param color colour for the contour lines
#' @param weight weight of contour lines.
#'
#' @return a leaflet map widgets with contours from raster image added.
#' @export
#'
#' @examples
lflt_contour <- function(map, r, color = "white", weight = 0.5){
    contour  <- raster::rasterToContour(r)
    addPolylines(map, color = color, 
                 data = contour,
                 weight = weight) 
}


lflt_factpal <- function(sf, pal_f = topo.colors(10)){
    leaflet::colorFactor(pal_f, as.factor(sf$LABEL))
}

prep_sf <- function(sf){
sf %>% sf::st_transform(sf, crs = 4326) 
}
