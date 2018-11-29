#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(sedMaps)
library(dplyr)
library(raster)
library(rgdal)
library(leaflet)
library(htmlwidgets)
library(htmltools)
library(leaflet.extras)
library(readr)

options(shiny.trace=TRUE)
#options(shiny.fullstacktrace=TRUE)

# ---- load_data ----
rst <- raster::stack("data/raster/sed_maps.grd")
varnames <- readRDS("data/raster/varnames.rds")
load_spice("data/metadata")

sf_files <- list.files("data/sf", full.names = T)



#system.file("data", sf_files[1], package = "sedMaps")
#sf <- readRDS(sf_files[1])

mode <- "view"
page_title <- "Sedimentary Environment Data Explorer"
# ---- Define-UI ----
ui <- fluidPage(theme = shinythemes::shinytheme("superhero"),
                
                includeCSS("www/styles.css"),
                
                # ---- Application-title ----
                navbarPage(windowTitle = page_title,
                    position = "fixed-top",
                           title =
                               div(
                                   div(
                                       id = "logo-merp",
                                       img(src = "merp-logo-long.png",
                                           height = 50,
                                           width = 250)
                                   ),
                                   page_title
                               ),
                           id = "mode", selected = "view",
                           tabPanel("view"),
                           tabPanel("extract")),
                
                
                # ---- Leaflet-plot ----
                leafletOutput("leaflet", width = "100%",  height="1000px"),
                
                # ---- layer-selection-panel ----
                absolutePanel(
                    top = 70, left = 20, width = 270,
                    draggable = TRUE,
                    wellPanel(
                        h3("Layers"),
                        tabsetPanel(id = "data", type = "tabs", selected = "sed",
                                    tabPanel("Sediment", value = "sed",
                                             hr(), 
                                             uiOutput("sed_panel")
                                    ),
                                    tabPanel("Disturbance",  value = "dis",
                                             hr(),
                                             uiOutput("dis_panel"))
                        )
                    )
                ),
                
                # ---- Map-tools ----
                absolutePanel(
                    bottom = 20, right = 20, width = 300,
                    draggable = TRUE,
                    wellPanel(h4("Map tools"),
                              hr(),
                              sliderInput("opacity",h5("Layer opacity"),
                                          min = 0,
                                          max = 1,
                                          value = 0.8),
                              selectInput("option", label = h5("Palette"), 
                                          choices = list("magma" = "A", "inferno" = "B",
                                                         "plasma" = "C", "viridis" = "D"), 
                                          selected = "A"),
                              selectInput("basemap", label = h5("Basemap"), 
                                          choices = leaflet::providers, 
                                          selected = "Esri.OceanBasemap")
                              
                    )
                ),
                absolutePanel(
                    top = 80, right = 10, width = 200,
                    draggable = FALSE,
                    uiOutput("select_sf"))
)



# ---- Define-server-logic ---- 
server <- function(input, output) {
    v <- reactiveValues(selected_varnames = NULL,
                        varname = NULL,
                        sf = NULL)
    click.list <- shiny::reactiveValues(ids = vector(),
                                        id = vector())
    draw.list <- shiny::reactiveValues()
    
    # ---- define-raster-reactives ----
    get_varname <- reactive({switch(input$data,
                                    "sed" = input$varname_sed,
                                    "dis" = input$varname_dis)})
    get_label <- reactive({switch(input$data,
                                  "sed" = input$varname_sed[length(input$varname_sed)],
                                  "dis" = input$varname_dis[length(input$varname_dis)])})
    
    get_last_varname <- reactive({
        v$selected_varnames <- update_selected_varnames(
            selected = v$selected_varnames,
            get_varname())
        v$selected_varnames[length(v$selected_varnames)]
    })
    
    # ---- basemap ----
    base_map <- shiny::reactive({
        lflt_basemap(rst, basemap = input$basemap) %>% 
            addFullscreenControl()
    })
    
    # ---- sf ----
    load_sf <- reactive({
        req(input$select_sf)
        leaflet::clearGroup(leaflet::leafletProxy(mapId = "leaflet"),
                            "sf") 
        
        readRDS(paste0("data/sf/", input$select_sf))
    })
    
    # add clicked sf layer to draw
    sf_add_layer <- reactive({
        req(v$sf)
        lflt_sf_selected(sf = v$sf, 
                         ids = input$leaflet_shape_click$id, 
                         label = glue::glue("{v$sf$id}: {v$sf$descr} +"))
    })
    
    # add all sf layers to draw
    sf_add_all_layers <- reactive({
        req(v$sf)
        lflt_sf_selected(sf = v$sf, ids = "all", 
                         label = glue::glue("{v$sf$id}: {v$sf$descr} +"))
    })
    
    sf_base_layer <- reactive({
        req(v$sf)
        leaflet::leafletProxy(mapId = "leaflet") %>%
        leaflet::clearGroup("sf") 
        
        lflt_sf(sf = v$sf, label = glue::glue("{v$sf$id}: {v$sf$descr} +"))
    })
    
    
    # ---- raster-layer ----
    raster_layer <- shiny::reactive({
        req(input$opacity)
        # get varname
        v$varname <- switch(input$mode,
                            "view" =  get_varname(),
                            "extract" = get_last_varname())
        # plot raster layer
        lflt_rst_selected(rst, 
                          varname = v$varname, 
                          label = v$varname, 
                          opacity = input$opacity, 
                          option = input$option)
    })
    
    # ---- define-selection-reactives ----
    get_drawn <- shiny::reactive({
        feature <- input$leaflet_draw_feature
        if(click$id %in% click.list$ids){
            click.list$ids[click.list$ids != click$id]
        }else{
            c(click.list$ids, click$id)
        }
    })
    
    # panel rendering reactives ----
    render_sed <- reactive({ panel_layers(panel = "sed", 
                                          mode = input$mode, 
                                          varnames = varnames)})
    render_dis <- reactive({ panel_layers(panel = "dis", 
                                          mode = input$mode, 
                                          varnames = varnames)})
    render_extract_mode <- reactive({
        panel_extract_mode()
    })
    render_select_sf <- reactive({
        req(input$mode)
        panel_select_sf()
    })
    # ---- extract-data ----
    extract_data <- reactive({
        
    })
    
    # ---- layer-UI ----
    output$sed_panel <- renderUI({render_sed()})
    output$dis_panel <- renderUI({render_dis()})
    
    # ---- basemap ----
    output$leaflet <- renderLeaflet({
        map <- base_map()
    })
    
    # ---- observeEvents ----        
    # ---- render-rasterLayer ----
    shiny::observeEvent({
        input$varname_sed
        input$varname_dis
        input$option
        input$opacity}, {
            req(input$opacity)
            req(input$option)
            req(input$varname_sed)
            raster_layer()
        })
    
    
    shiny::observeEvent(input$load_sf,{
        v$sf <- load_sf()
        click.list$ids <- NULL
        sf_base_layer()
    })
    
    shiny::observeEvent(input$leaflet_shape_click, {
        #print(click$id)
        #sf_add_layer()
    })
    
    shiny::observeEvent(input$mode, {
        v$selected_varnames <- NULL
        
        if(input$mode == "extract"){
            
            output$select_sf <- renderUI({render_select_sf()})
     
                leaflet::leafletProxy("leaflet") %>% 
                    addDrawToolbar(
                        targetGroup = 'draw',
                        position = "topright",
                        circleOptions = F,
                        circleMarkerOptions = F,
                        editOptions = editToolbarOptions(
                            selectedPathOptions = 
                                selectedPathOptions())) %>%
                    addLayersControl(overlayGroups = 'draw', options =
                                         layersControlOptions(collapsed=FALSE))
        }
        if(input$mode == "view"){
            output$select_sf <- NULL
            v$sf <- NULL
            
            leaflet::leafletProxy("leaflet") %>%
                removeDrawToolbar(clearFeatures = T) %>%
                clearGroup("draw")
        }
    })
    
    
    observeEvent(input$download,
                 {showModal(download_modal())})
    
    output$layer_info <-  renderPrint({
        v$selected_varnames
    })
    
    output$downloadData <- downloadHandler(
        filename = function() {
            paste("merp-", Sys.Date(), ".csv", sep="")
        },
        content = function(file) {
            write.csv(data, file)
        }
    )
    
    observeEvent({
        #input$leaflet_draw_new_feature
                 #input$leaflet_draw_deleted_features
                 input$leaflet_draw_all_features
                 #input$leaflet_draw_deletestop
                 }, {
        #dput(input$leaflet_draw_new_feature)
        #print(drawFeature2sf(input$leaflet_draw_new_feature),)
                     print(input$leaflet_sf)
        print(input$leaflet_groups, 2)
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
         

