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


sed_path <- here::here("data-raw", "nc", "sediment_properties.nc")
dis_path <- here::here("data-raw", "nc", "monthly_disturbance.nc")

path <- dis_path

sed_rst <- ncdf_stack(sed_path)
dis_rst <- ncdf_dimstack(dis_path, dimension = "Time") %>% 
    setNames(month.name[names(.) %>% 
                            stringr::str_replace("month_", "") %>% 
                            as.numeric()])


raster::animate(dis_rst, pause=0.25,  n=10)

# input - varname
varname <- "SandPercent"

# input - basemap
basemaps <- c("Esri.WorldPhysical", "Esri.OceanBasemap")
basemap <- "Esri.WorldPhysical"






# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Old Faithful Geyser Data"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
         sliderInput("bins",
                     "Number of bins:",
                     min = 1,
                     max = 50,
                     value = 30)
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
         plotOutput("distPlot")
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
   
   output$map <- renderPlot({

       # subset var
       r <- raster::subset(sed_rst, subset = varname)
       
       # get palette
       pal <- colorNumeric(viridis::viridis(10), raster::values(r),
                           na.color = "transparent")
       # plot
       leaflet() %>% addTiles()  %>% 
           addProviderTiles(providers[[basemap]]) %>%
           addRasterImage(r, colors = pal, opacity = 0.8) %>%
           addLegend(pal = pal, 
                     values = values(r),
                     label = varname,
                     title = varname)
       
       
       
       
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

