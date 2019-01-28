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
library(shinyWidgets)
library(htmltools)
library(leaflet.extras)
library(readr)
library(sf)

options(shiny.trace=FALSE)
options(shiny.fullstacktrace=FALSE)

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
                                           height = 50),
                                       img(src = "tuos_blue_logo.png",
                                           height = 50)
                                   ),
                                   page_title,
                                   helpText("Access, visualise, summarise and extract raster maps of the north-west European Shelf sedimentary environment")
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
                        fluidRow(
                            column(8, h3("Layers")),
                            column(4, 
                                   # ---- Layer-preferences-dropdown ----
                                   shinyWidgets::dropdownButton(
                                       h4("Layer preferences"),
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
                                                   selected = "Esri.OceanBasemap"),
                                       # dropdown settings
                                       label = "Map preferences",
                                       circle = TRUE, status = "info", 
                                       icon = icon("gear"), width = "300px",
                                       right = FALSE, tooltip = TRUE))
                        ),
                        
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
                absolutePanel(
                    top = 80, right = 70, width = 280,
                    draggable = TRUE,
                    uiOutput("select_sf"))
)



# ---- Define-server-logic ---- 
server <- function(input, output, session) {
    v <- reactiveValues(selected_varnames = NULL,
                        varname = NULL,
                        sf = NULL,
                        out_sf = NULL,
                        selected_groups = NULL)
    
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
    get_selected_varnames <- reactive({
        c(input$varname_sed, input$varname_dis)
    })
    
    get_last_varname <- reactive({
        v$selected_varnames <- update_selected_varnames(
            selected = v$selected_varnames,
            get_selected_varnames())
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
        req(input$basemap)
        req(input$mode)
        req(input$opacity)
        req(input$option)
        req(input$varname_sed)
        
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
    shiny::observeEvent(input$basemap, {
        output$leaflet <- renderLeaflet({
            map <- base_map()
        })
        })
       
    # ---- render-rasterLayer ----
    shiny::observeEvent({
        input$varname_sed
        input$varname_dis
        input$basemap
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
    
    shiny::observeEvent(input$mode, {
        v$selected_varnames <- NULL
        
        if(input$mode == "extract"){
            
            output$select_sf <- renderUI({render_select_sf()})
            
            leaflet::leafletProxy("leaflet") %>% 
                addDrawToolbar(
                    targetGroup = 'draw',
                    position = "topright",
                    polylineOptions= F,
                    markerOptions = F, 
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
    
    observeEvent(input$download,{
        v$out_sf <- collate_extr_shapes(v$sf, 
                                        input$leaflet_draw_all_features,
                                        input$leaflet_groups)
    })
    
    output$data_layer_info <-  renderText({
        if(is.null(v$selected_varnames)){
            "No data layers selected. Defaults to ALL"
        }else{
            glue::glue_collapse(v$selected_varnames, sep = ", ")
        }
    })
    
    output$extr_group_info <-  renderText({
        v$selected_groups <- input$leaflet_groups[input$leaflet_groups != "raster"]
        if(length(v$selected_groups) == 0){
            "No extraction groups selected"
        }else{
            glue::glue_collapse(v$selected_groups, sep = ", ")
        }
    })
    
    output$extr_layer_info <-  renderTable({
        if(length(v$out_sf) == 0){
            "No extraction layers specified"
        }else{
            v$out_sf %>% dplyr::select(id, descr) %>% 
                sf::st_set_geometry(NULL)
        }
    })
    
    
    observeEvent(input$download,
                 if(length(v$out_sf) == 0){
                     shinyWidgets::sendSweetAlert(session = session, title = "Error", 
                                                  text = "No shapes to use for extraction specified", 
                                                  type = "error", btn_labels = "Ok", 
                                                  html = FALSE, closeOnClickOutside = TRUE)
                     }else{showModal(download_modal())})
    

    
    output$downloadData <- downloadHandler(
        filename = function() {
            paste("merp-sedmaps-out-", Sys.Date(), ".zip", sep="")
        },
        content = function(file) {
            tmp <- tempdir()
            out_dir <- file.path(tmp, "out")
            dir.create(out_dir)
            
            if(!is.null(input$sum_stats)){
                output <- c("summaries", input$raw_data_format)
            }else{output <- input$raw_data_format}
            
            extr_sedmap_data(rst, sf = v$out_sf, 
                             select_rst = v$selected_varnames, 
                             select_sf = NULL,
                             output = output,
                             fun = input$sum_stats,
                             out_dir = out_dir, 
                             rst_out_format = input$rst_out_format,
                             select_sf_csv = input$select_sf_csv,
                             attributes = attributes,
                             varnames = varnames)
            
            app_wd <- getwd()
            setwd(out_dir)
            zip(zipfile = file, files = list.files(".", recursive = T))
            setwd(app_wd)
            file.remove(list.files(out_dir, recursive = T, 
                                   full.names = T))
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
        dput(input$leaflet_draw_all_features)
    })
}

# Run the application 
shinyApp(ui = ui, server = server)


