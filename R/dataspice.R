
#' Load `dataspice`` metadata tables
#' 
#' Load `dataspice`` metadata tables into the global environment
#' @param metadata_path  
#' @param table character string vector of names of valid `dataspice` metadata
#' tables. One of `"access"`, `"attributes"`, `"biblio"`, `"creators"`)
#'
#' @return tibble loaded into the global environment for each valid `table`.
#' @export
#'
#' @examples
#' \dontrun{
#' load_spice()
#' load_spice(table = "attributes")
#' }
load_spice <- function(metadata_path = here::here("data", "metadata"), 
                       table = NULL){
    if(is.null(table)){
        table <- c("access", "attributes", "biblio", "creators")
    }else{
        table <- match.arg(table,
                           c("access", "attributes", "biblio", "creators"),
                           several.ok = T)
    }
    if(!file.exists(metadata_path)){
        stop("metadata directory not found at: \n", metadata_path)  }  
   # assign to global environment
    purrr::map(table, ~assign_spice(.x, metadata_path))
    
}

assign_spice <- function(table, metadata_path){
    table <- match.arg(table,
                       c("access", "attributes", "biblio", "creators"))
    file_name <- glue::glue("{table}.csv")
    table_path <- file.path(metadata_path, file_name)
    if(!file.exists(table_path)){
        warning("table load fail: ", file_name, "not found at: \n", 
                metadata_path)    
    }                        
    assign(table, readr::read_csv(table_path), envir = .GlobalEnv)
}
