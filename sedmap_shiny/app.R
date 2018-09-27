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
library(leaflet)
library(htmlwidgets)
library(htmltools)


sed_path <- here::here("data-raw", "nc", "sediment_properties.nc")
dis_path <- here::here("data-raw", "nc", "monthly_disturbance.nc")



rst <- list(
    "sed" = {ncdf_stack(sed_path)},
    "dis" = {ncdf_dimstack(dis_path, dimension = "Time") %>% 
            setNames(month.name[names(.) %>% 
                                    stringr::str_replace("month_", "") %>% 
                                    as.numeric()])})

varnames <- list(sed = names(rst[["sed"]]),
                 dis = names(rst[["dis"]]))

rst <- raster::stack(rst)

names(rst)
# Define UI for application that draws a histogram
ui <- fluidPage(theme = shinythemes::shinytheme("superhero"),
                
                # Application title
                titlePanel("Sedimentary Environment Data Explorer"),
                
                
                # Leaflet plot 
                leafletOutput("leaflet", width = "100%",  height="1000px"),
                
                # layer selection panel
                absolutePanel(
                    top = 70, left = 20, width = 270,
                    draggable = TRUE,
                    wellPanel(
                        h3("Layers"),
                        tabsetPanel(id = "data", type = "tabs", selected = "sed",
                                    tabPanel("Sediment", value = "sed",
                                             hr(),
                                             radioButtons("varname_sed", label = h4("Select layer"),
                                                          choices =  varnames[["sed"]], 
                                                          selected =  varnames[["sed"]][1])),
                                    tabPanel("Disturbance",  value = "dis",
                                             hr(),
                                             radioButtons("varname_dis", label = h5("Select layer"),
                                                          choices = varnames[["dis"]]), 
                                             selected = varnames[["dis"]][1])))),
                # Map tools
                absolutePanel(
                    bottom = 20, right = 20, width = 200,
                    draggable = TRUE,
                    wellPanel(opacity = 0.8,
                              h3("Map tools"),
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
                )
)



# Define server logic required to draw a histogram
server <- function(input, output) {
    #v <- reactiveValues(varname = input$varname_sed)
    get_varname <- reactive({switch(input$data,
                                    "sed" = input$varname_sed,
                                    "dis" = input$varname_dis)})
    output$leaflet <- renderLeaflet({
        
        lflt_plot(rst, varname = get_varname(), label = "Disturbance", 
                  opacity = input$opacity, option = input$option,
                  basemap = input$basemap) 
        
    })
}

# Run the application 
shinyApp(ui = ui, server = server)

