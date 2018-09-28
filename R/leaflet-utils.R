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
                      opacity = 0.8, sf = NULL){
    
    if(is.null(varname)){ varname <- names(rst)[1]}else{
        varname <- match.arg(varname, choices = names(rst))}
    print(var)
    
    if(is.null(label)) label <- varname
    # subset var
    r <- raster::subset(rst, subset = varname)
    #proj <- "+proj=tmerc +lat_0=49 +lon_0=-2 +k=0.9996012717 +x_0=400000 +y_0=-100000 +ellps=airy +towgs84=446.448,-125.157,542.06,0.15,0.247,0.842,-20.489 +units=m +no_defs "
    #raster::crs(r) <- proj
    
    
    
    # get palette
    pal <- leaflet::colorNumeric(viridis::viridis(100, option = option), 
                                 raster::values(r),
                                 na.color = "transparent")
    # plot
    leaflet(
        #options = c(crs = leafletCRS(proj4def = proj))
    ) %>% 
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
#' @param fillOpacity sf polygon fill colour
#' @param color sf polygon stroke colour
#'
#' @return a leaflet map object with sf vector data overlaid
#' @export
#'
#' @examples
lflt_sf <- function(map, sf = NULL, fillOpacity = 0.3, ...){
    if(is.null(sf)){map}else{
        factpal <- leaflet::colorFactor(topo.colors(10), as.factor(sf$LABEL))
        sf <- sf::st_transform(sf, crs = 4326) 
        map %>%
            leaflet::addPolygons(data = sf, 
                                 weight = 1, 
                                 smoothFactor = 0.5,
                                 opacity = 0.9, fillOpacity = fillOpacity,
                                 color = ~factpal(as.factor(LABEL)),
                                 highlightOptions = leaflet::highlightOptions(
                                                      weight = 3,
                                     bringToFront = TRUE), ...)
    }
}

lflt_cor <- function(rst, varname1 = NULL, varname2 = NULL,
                     basemap = "Esri.OceanBasemap",
                     option = "magma", label = NULL,
                     opacity = 0.8){
    if(is.null(varname1)){ varname1 <- names(rst)[1]}else{
        varname1 <- match.arg(varname1, choices = names(rst))}
    if(is.null(varname2)){ varname2 <- names(rst)[2]}else{
        varname2 <- match.arg(varname2, choices = names(rst))}
    
    # subset var(raster_stack, 'pearson', na.rm=T)
    r <- raster::layerStats(
        raster::subset(rst, subset = c(varname1, varname2)),
        #raster::subset(rst, subset = varname2)),
        'pearson', na.rm=T)
    #proj <- "+proj=tmerc +lat_0=49 +lon_0=-2 +k=0.9996012717 +x_0=400000 +y_0=-100000 +ellps=airy +towgs84=446.448,-125.157,542.06,0.15,0.247,0.842,-20.489 +units=m +no_defs "
    #raster::crs(r) <- proj
    
    
    
    # get palette
    pal <- leaflet::colorNumeric(viridis::viridis(100, option = option), 
                                 raster::values(r),
                                 na.color = "transparent")
    # plot
    leaflet(
        #options = c(crs = leafletCRS(proj4def = proj))
    ) %>% 
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

