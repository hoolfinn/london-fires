library(shiny)
library(tidyverse)
library(rgdal)
library(rgeos)
library(maptools)
library(RColorBrewer)


# Define UI 
ui <- fluidPage(
  
  mainPanel(
    # Application title
    h3(titlePanel("Fire risk in Greater London")),
    
    # Draw the plot
    fluidRow(
      column(6, h4("Chart 1: Map of fire locations - Jan 2012 to Aug 2015"), plotOutput("mapPlot")),
      column(6, h4("Chart 2: Fire risk model quartiles by electoral ward"),
             plotOutput("wardPlot"))
    ),
    
    
    # Sidebar with a slider input and radio buttons
    fluidRow(
      column(3,
             radioButtons("colour", h4("Type of plot:"),
                          c("Simple map" = "d", 
                            "Heatmap of 2D bin count" = "op1",
                            "Heatmap of 2D bin count - log scale" = "op2",
                            "2D kernel density estimation" = "op3")
             ),
             sliderInput("bins", 
                         h4("Number of bins (for Heatmap):"),
                         min = 1,
                         max = 200,
                         value = 100)
      ),
      column(3,
             radioButtons("propCats", 
                          label = h4("Property Categories"), 
                          choices = list("All" = "All", 
                                         "Dwelling" = "Dwelling", 
                                         "Non Residential" = "Non Residential",
                                         "Other Residential" = "Other Residential", 
                                         "Road Vehicle" = "Road Vehicle"),
                          selected = "All")
      ),
      column(3,
             numericInput("w1", 
                          label = h4("Dwelling fire weight"), 
                          value = 3),
             numericInput("w3", 
                          label = h4("Non-residential fire weight"), 
                          value = 1),
             numericInput("w5", 
                          label = h4("Vehicle fire weight"), 
                          value = 1)),
      column(3,
             numericInput("w2", 
                          label = h4("Dwelling fire severity weight"), 
                          value = 3),
             numericInput("w4", 
                          label = h4("Other residential fire weight"), 
                          value = 1))
      
      
    )
  )
)


# Define server logic required to draw a histogram
server <- function(input, output) {
  output$mapPlot <- renderPlot({
    
    # create borough map
    london_shp <- readShapePoly("./statistical-gis-boundaries-london/ESRI/London_Borough_Excluding_MHW.shp")
    london_map <- fortify(london_shp)
    london_map <- rename(london_map, Easting_rounded = long, Northing_rounded = lat)
    
    # load fire data
    fires <- readRDS("./Data/fires2.rds")
    fires$PropertyCategory <- factor(fires$PropertyCategory)
    N <- nrow(fires)
    
    if (input$propCats != "All"){
      fires <- subset(fires, PropertyCategory == input$propCats)
    } 
    
    
    # build base ggplot object
    p <- ggplot(data=fires, aes(x=Easting_rounded, y=Northing_rounded)) + 
      xlab("") + ylab("") + 
      theme_void() +
      theme(axis.ticks = element_blank(), axis.text.y = element_blank(), axis.text.x = element_blank())
    
    # depending on user input choice, draw correct plot
    if (input$colour == "op1") {
      p + stat_bin2d(bins=input$bins) + 
        scale_fill_gradient(low="skyblue", high="red") + 
        geom_path(data=london_map, aes(x = Easting_rounded, y = Northing_rounded, group=group)) # + 
      # ggtitle("Heatmap of 2D bin count")
      
    }
    else if (input$colour == "op2") {
      p + stat_bin2d(aes(fill=log10(..count..)), bins=input$bins) + 
        scale_fill_gradient(low="skyblue", high="red") + 
        geom_path(data=london_map, aes(x = Easting_rounded, y = Northing_rounded, group=group)) # + 
      # ggtitle("Heatmap of 2D bin count - log scale")
    }
    else if (input$colour == "op3") {
      
      # Color palette for the KDE plot
      rf <- colorRampPalette(rev(brewer.pal(11,'Spectral')))
      r <- rf(32)
      
      p + stat_density_2d(aes(fill=..level..), geom = "polygon", n=input$bins) +
        scale_fill_gradientn(colours=r) + 
        geom_path(data=london_map, aes(x = Easting_rounded, y = Northing_rounded, group=group)) + guides(fill=FALSE)
      # ggtitle("2D kernel density estimation") 
    }
    else {
      ggplot(london_map, aes(x = Easting_rounded, y = Northing_rounded, group=group)) + geom_path() + 
        geom_point(data=fires, aes(x=Easting_rounded, y=Northing_rounded, group=NULL), alpha=0.02 * (N / nrow(fires)), colour = "red") + 
        theme_void() +
        xlab("") + ylab("") +
        theme(axis.ticks = element_blank(), axis.text.y = element_blank(), axis.text.x = element_blank()) # +
      # ggtitle("Map of fires in London")
    }
    
  })
  
  output$wardPlot <- renderPlot({
    
    # load fire data
    map.data <- readRDS("./Data/mapData4.rds")
    
    # calculte fire risk based on input weights
    map.data$fr <- (input$w1 * map.data$ct_fires.d + input$w2 * 
                      map.data$av_workTime.d + input$w3 * 
                      map.data$ct_fires.nr + input$w4 * map.data$ct_fires.or + 
                      input$w5 * map.data$ct_fires.v) / 
      sum(input$w1, input$w2, input$w3, input$w4, input$w5, na.rm = T)
    
    # create quartiles
    map.data$quartile[!is.na(map.data$fr)] <- ntile(map.data$fr[!is.na(map.data$fr)], 4)
    map.data$quartile <- ordered(map.data$quartile)
    
    # load map data
    lnd.res <- readShapePoly("./London-wards-2014/London-wards-2014_ESRI/London_Ward_CityMerged.shp")
    lnd.res.map <- fortify(lnd.res, region = "GSS_CODE")
    lnd.res.map <- merge(lnd.res.map, map.data, by.x = "id", by.y = "GSS_CODE", all.x = T)
    
    # create colour palette
    pal <- colorRampPalette(c("white", "red"))(4)
    
    # draw ward plot
    ggplot(lnd.res.map, aes(x=long, y=lat, group=group, fill=quartile)) + #
      geom_polygon(colour="black") + 
      scale_fill_manual(values=pal) +
      xlab("") + ylab("") + 
      theme_minimal() +
      labs(fill="Fire risk\nQuartile") +
      theme(axis.ticks = element_blank(), axis.text.y = element_blank(), 
            axis.text.x = element_blank(), panel.background = element_blank(),
            panel.grid = element_blank())
    
  })
}


# Run the application 
shinyApp(ui = ui, server = server)

