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
           "view" = radioButtons(glue::glue("varname_{panel}"), label = h4("Select data layer"),
                                 choices =  varnames[[panel]], 
                                 selected =  varnames[[panel]][1]),
           "extract" = checkboxGroupInput(glue::glue("varname_{panel}"), label = h4("Select data layer"),
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
    choices <- list.files("data/sf") %>% 
        setNames(purrr::map_chr(., ~fileName_meta(.x)))

    shiny::wellPanel(
        shiny::selectInput("select_sf", "Select Maritime Boundaries",
                           choices = choices,
                           selected = NULL),
        shiny::helpText("Use in-built maritime boundary polygons to extract data. Alternatively, 
                        use side toolbar to draw custom extraction polygons"),
        shiny::actionButton("load_sf", label = "Load",
                            icon = shiny::icon("upload"),
                            width = "100%"),
        hr(),
        actionButton("download", "Launch download panel", width = "100%",
                     style = "background-color: #1F2956; font-style: strong;"),
        style = "padding: 5px; background-color: #53A9DC;"
    )
} 



#' Download modal panel
#'
#' @return create download modal panel
#' @export
#'
download_modal <- function(){
    
    shiny::modalDialog(
        shiny::strong(shiny::h5("Selected data layers")),
        shiny::textOutput("data_layer_info"),
        shiny::br(),
        shiny::strong(shiny::h5("Extraction vector layers")),
        shiny::h6("Selected extraction groups"),
        shiny::textOutput("extr_group_info"),
        shiny::h6("Selected extraction layers"),
        shiny::tableOutput("extr_layer_info"),
        shiny::br(),
        shiny::hr(),
        shiny::strong(shiny::h5("Summary statistics")),
        shiny::checkboxGroupInput("sum_stats", 
                              label = "Choose summary statistics",
                              choices = fun_choices(),
                              inline = TRUE),
        shiny::br(),
        shiny::hr(),
        shiny::strong(shiny::h5("Raw data")),
        shiny::checkboxGroupInput("raw_data_format", 
                                  label = "data format", 
                                  choices = c("csv", "raster"),
                                  inline = T),
        downloadButton("downloadData", label = "Download", class = NULL),
        title = "Download Data", footer = modalButton("Cancel"),
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




#' Get descriptive label
#'
#' Create a more descriptive label, including units where appropriate, for a
#' variable from information in the metadata table.
#' @param fileName the file for which metadata is to be returned
#' @param return_field character string. field to be returned for fileName
#'
#' @return a character string descriptive label of the variable
fileName_meta <- function(fileName, return_field = "name"){
    return_field <- match.arg(return_field, choices = names(access))
    
    if(!fileName %in% access$fileName){
        stop(fileName, " not a valid fileName")
    }
    access[access$fileName == fileName, return_field, drop = T]
}

