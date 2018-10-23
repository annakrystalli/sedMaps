#' Create layers panel
#'
#' @param panel 
#' @param mode 
#'
#' @return
#' @export
#'
#' @examples
panel_layers <- function(panel = c("sed", "dis"), 
                         mode = c("view", "extract"),
                         varnames = varnames){
    panel <- match.arg(panel)
    mode <- match.arg(mode)
    
    switch(mode,
           "view" = radioButtons(glue::glue("varname_{panel}"), label = h4("Select layer"),
                                 choices =  varnames[[panel]], 
                                 selected =  varnames[[panel]][1]),
           "extract" = checkboxGroupInput(glue::glue("varname_{panel}"), label = h4("Select layer"),
                                          choices =  varnames[[panel]], 
                                          selected =  NULL)
    )
}




#' Title
#'
#' @return
#' @export
#'
#' @examples
panel_extract_mode <- function(){
    shinyWidgets::radioGroupButtons("extract_mode", "Extract mode",
                                    choices = c("draw", "sf", "user_sf"), selected = NULL)
    
    # radioButtons()
}

#' Title
#'
#' @param extract_mode 
#'
#' @return
#' @export
#'
#' @examples
panel_select_sf <- function(extract_mode){
    
    switch(extract_mode,
           "draw" = {},
           "sf" = {
               shiny::wellPanel(selectInput("select_sf", "select simple features dataset",
                                            choices = list.files(here::here("data", "sf")),
                                            selected = NULL),
                                actionButton("load_sf", label = "load sf",
                                             icon = icon("layer-group",
                                                         lib = "font-awesome"),
                                             width = "100%"), 
                                style = "padding: 5px;")})
    
    
} 

#' Title
#'
#' @param panel 
#' @param mode 
#'
#' @return
#' @export
#'
#' @examples
select_button <- function(panel = c("layers", "extract"),
                          mode = c("select", "deselect")){
    panel <- match.arg(panel)
    mode <- match.arg(mode)
    switch(mode,
           "select" = {
               icon <- "draw-polygon"
               label <- "select all"
               color <- "#685177"},
           "deselect" = {
               icon <- "eraser"
               label <- "clear all"
               color <- "#311f3d"})
    
    print(icon)
    print(color)
    shinydashboard::box(
        width = 6,
        shiny::actionButton( inputId = glue::glue("{panel}_{mode}"),
                             icon = icon( name = icon),
                             label = label,
                             style = glue::glue("color: white; background-color: {color}; border-color: {color}")
        ))
}

#' Title
#'
#' @param panel 
#'
#' @return
#' @export
#'
#' @examples
select_box <- function(panel = "layers"){
    shiny::wellPanel(select_button(panel, "select"),
                     select_button(panel, "deselect"), 
                     style = "padding: 5px;")
}

