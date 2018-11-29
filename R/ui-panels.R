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

#' Select in-built sf wellpanel
#'
#' @return create in-built sf wellpanel
#' @export
panel_select_sf <- function(){
    shiny::wellPanel(
        shiny::helpText("Use in-built maritime boundary polygons to extract data"),
        shiny::selectInput("select_sf", "Select Maritime Boundaries",
                           choices = list.files("data/sf"),
                           selected = NULL),
        shiny::actionButton("load_sf", label = "Load",
                            icon = shiny::icon("upload"),
                            width = "100%"),
        hr(),
        actionButton("download", "Launch download panel"),
        style = "padding: 5px;"
    )
} 



#' Download modal panel
#'
#' @return create download modal panel
#' @export
#'
download_modal <- function(){
    argList <- args(extr_summaries) %>% as.list()
    
    shiny::modalDialog(
        shiny::h5("Selected layers"),
        shiny::verbatimTextOutput("layer_info"),
        shiny::verbatimTextOutput("polygon_info"),
        shiny::h5("summary stats"),
        shiny::checkboxGroupInput("sum_stats", 
                              label = "Choose summary statistics",
                              choices = argList$fun,
                              inline = TRUE),
        shiny::h5("raw data"),
        shiny::checkboxGroupInput("raw_data_format", 
                                  label = "data format", 
                                  choices = c("csv", "raster")),
        downloadButton("downloadData", label = "Download", class = NULL),
        title = "Download Data", footer = modalButton("Dismiss"),
        size = c("m", "s", "l"), easyClose = FALSE)
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


