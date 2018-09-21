
#' Extract metadata from ncdf object
#'
#' @param ncdf ncdf object from `ncdf4::nc_open()`.
#' @param dimension the dimension from which to extracts information, See `ncdf_dim()`
#'
#' @return values returned vary according to `ncdf_*` function called:
#' * **`ncdf_varnames`** a vector of the varnames of each layer in the ncdf file.
#' * **`ncdf_ndims`** the number of dimensions in the ncdf.
#' * **`ncdf_dim`** a vector of ncdf dimension names
#' * **`ncdf_extra_dims`** a vector of dimension names additional to spatial lon/lat ($x$ & $y$) coordinates.
#' * **`ncdf_bands`** the bands of a specified dimension (pixel size).
#' * **`ncdf_band_units`** the units of the bands in a specified dimension.
#' @export
#'
#' @examples
ncdf_varnames <- function(ncdf){
    if(ncdf$ndims > 2){
        dims <- ncdf$dim %>% names()
        dim_extra <- ncdf_extra_dims(ncdf)
        warning("ncdf has ", ncdf$ndims, " dimensions, ", ncdf$ndims - 2,
                " additonal to spatial: \n ", dim_extra)
    }
    ncdf$var %>% names
}

#' @inherit ncdf_varnames
#' @export
ncdf_ndims <- function(ncdf){
    ncdf$ndims
}

#' @inherit ncdf_varnames
#' @export
ncdf_dim <- function(ncdf){
    ncdf$dim %>% names
}

#' @inherit ncdf_varnames
#' @export
ncdf_extra_dims <- function(ncdf){
        dims <- ncdf$dim %>% names()
        dims[!as.logical(dims %>% stringr::str_detect("[lL]on*") +
                                          dims %>% stringr::str_detect("[lL]at*"))]
}

#' @inherit ncdf_varnames
#' @export
ncdf_bands <- function(ncdf, dimension){
        ncdf$dim[[dimension]]$vals
}

#' @inherit ncdf_varnames
#' @export
ncdf_band_units <- function(ncdf, dimension = NULL){
    if(is.null(dimension)){stop("please specify dimension")}
    ncdf$dim[[dimension]]$units
}
