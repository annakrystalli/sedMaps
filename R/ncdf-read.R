
#' Load a ncdf file into a rasterBrick
#'
#' @param path path to ncdf file
#'
#' @return a rasterBrick, one layer for each varname in the ncdf.
#' @export
#'
#' @examples
ncdf_stack <- function(path){
    ncdf <- ncdf4::nc_open(filename = path)
    v <- ncdf_varnames(ncdf)
    purrr::map(v, ~raster::raster(path, varname = .x, RAT = T)) %>%
        raster::stack() %>%
        setNames(v) 
}


#' Stack bands from a third dimension into a single rasterStack
#'
#' Stack bands from a third dimension (eg time, depth) into a single rasterStack.
#' @param path path to the ncdf file
#' @param varname single varname for which to extract bands
#' @param dimension name of dimension from which to extract bands. Check available dimensions with `ncdf_extra_dims()`.
#'
#' @return a stack of rasterLayers containing values for `varname`` in each dimensional band.
#' @export
#'
#' @examples
ncdf_dimstack <- function(path, varname = NULL, 
                          dimension){
    if(length(varname) > 1){stop("please supply single varname to dimstack")}
    ncdf <- ncdf4::nc_open(filename = path)
    
    varname <- match.arg(varname, choices = ncdf_varnames(ncdf))
    dimension <- match.arg(dimension, ncdf_dim(ncdf))
    
    dim_id <- ncdf_bands(ncdf, dimension)
    dim_unit <- ncdf_band_units(ncdf, dimension)
    
    dim_id %>% purrr::map(~raster::raster(path, band = .x)) %>% 
        raster::stack() %>% setNames(paste0(stringr::str_to_lower(dim_unit), 
                                            "_", dim_id))
}


