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

data <- "sed"

# input - basemap
basemaps <- c("Esri.WorldPhysical", "Esri.OceanBasemap")
basemap <- "Esri.WorldPhysical"


rst <- switch(data,
              "dis" = {ncdf_dimstack(dis_path, dimension = "Time") %>% 
                      setNames(month.name[names(.) %>% 
                                              stringr::str_replace("month_", "") %>% 
                                              as.numeric()])},
              "sed" = {ncdf_stack(sed_path)})


# Define UI for application that draws a histogram
ui <- fluidPage(theme = shinythemes::shinytheme("superhero"),
                
                # Application title
                titlePanel("Sedimentary Environment Data Explorer"),
                
                
                # Leaflet plot 
                leafletOutput("leaflet", width = "100%",  height="1000px"),
                
                absolutePanel(
                    top = 70, left = 20, width = 200,
                    draggable = TRUE,
                    wellPanel(
                        h3("Layers"), 
                        hr(),
                        # selectors
                        radioButtons("varname", label = h5("Select layer"),
                                     choices = names(rst), 
                                     selected = names(rst)[1]))),
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
    
    output$leaflet <- renderLeaflet({
        
        lflt_plot(rst, varname = input$varname, label = "Disturbance", 
                  opacity = input$opacity, option = input$option,
                  basemap = input$basemap) 
        
    })
}

# Run the application 
shinyApp(ui = ui, server = server)

