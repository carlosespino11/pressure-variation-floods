library(shiny)
library(leaflet)
library(RColorBrewer)
library(raster)
library(lubridate)
library(dplyr)
library(maptools)
library(ggplot2)
library(ggthemes)
library(foreach)
# setwd("~/Documents/Columbia/Exploratory Data Analysis and Visualization/HW2/EDAV_Proect_NOAA/")

coastline = readShapeLines("ne_110m_coastline.shp")
countries = readShapeLines("ne_110m_admin_0_boundary_lines_land.shp")

load("floods.RData")
load("pressure.RData")
load("component.RData")


floods = floods %>% mutate(severity = as.numeric(as.character(severity)),
                           began = ymd(began),
                           ended = ymd(ended),
                           date_interval = interval(began, ended),
                           began_year = year(began),
                           began_month = month(began))


orig_date = ymd("1985-01-01")
day = 1
date = orig_date + days(day)


get_date = function(day) orig_date + days(day)

get_component = function(sel_year, sel_component, online =FALSE){
  if(online){
    indexes = unlist(date_map %>%  dplyr::filter(year==selL_year) %>% dplyr::select(index))
    phi_df = foreach(i=indexes, .combine='rbind') %do% as.vector(phi[,,i])
    prcomp(phi_df,center = TRUE,scale. = TRUE)$rotation[,sel_component]
  }else{
    unlist(components %>% filter(year==sel_year, component==sel_component) %>% 
             select(X1:X2160))
  }
}

date_map = data.frame(index= 1:dim(phi)[3], date = get_date(1:dim(phi)[3])) %>%
  mutate(year = year(date))

long_fix= ifelse(long>180,long-360,long)


pal_severity <- colorFactor("YlOrRd", factor(floods$severity),
                            na.color = "transparent")

ui <- bootstrapPage(
  tags$head(
    includeCSS("styles.css")
  ),
  tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
  leafletOutput("map", width = "100%", height = "100%"),
  absolutePanel(top = 10, right = 10, id = "controls", class = "panel panel-default", 
                fixed = TRUE, draggable = TRUE,height = "800px",
                sliderInput("date", "Pick a year", 
                            min=1985, max=2015, value=2010,
                            sep="", animate=TRUE, dragRange = FALSE),
                selectInput("component", "Pick a component", 
                            choices = 1:2, selected = 1),
                plotOutput("floodsMonth", height = 200),
                plotOutput("magnitudeMonth", height = 200),
                plotOutput("cause", height = 200)
  ),
  absolutePanel(top = 10, left = 25,headerPanel("Pressure Variation vs. Floods"))
)

server <- function(input, output, session) {
  # Reactive expression to subset data
  filteredData <- reactive({
    
    selected_date = as.numeric(input$date)
    comp_num = as.numeric(input$component)
    
    
    pca = abs(get_component(selected_date, comp_num))
    
    new = data.frame(phi = pca, 
                     x = rep(long_fix, 15), 
                     y = rep(lat, times=rep(144,15))) 
    coordinates(new) = ~x+y
    r2 = rasterFromXYZ(new)
    proj4string(r2) <- CRS("+proj=longlat +ellps=WGS84")
    
    p = rasterToPolygons(r2)
    
    filtered_floods = floods %>% dplyr::filter(began_year== selected_date, !is.na(severity))
    
    return(list('poly' = p, 'floods' = filtered_floods))
    
  })
  
  output$map <- renderLeaflet({
    # Aaspects of the map that  won't need to change dynamically
    leaflet() %>%
      addTiles(options=tileOptions(continuousWorld= TRUE))%>%
      setView(0,42, 2)
    
  })
  
  # Incremental changes to the map
  observe({
    pal <- colorNumeric(rev(brewer.pal(10, "RdYlBu")), filteredData()$poly$layer,
                        na.color = "transparent")
    
    
    leafletProxy("map", data= filteredData()$poly) %>%
      clearControls() %>%
      clearShapes() %>%
      addPolygons(color = ~pal(layer),stroke = FALSE, fillOpacity = 0.6) %>%
      addLegend(pal = pal, 
                values = filteredData()$poly$layer,
                position = "bottomleft",
                title = "Variation") %>%
      addCircles(lng =~centroid.x, lat = ~centroid.y, weight = .5, color = "black",
                 stroke = TRUE,
                 radius = ~500*sqrt(affected.sq.km),
                 fillColor= ~pal_severity(severity),
                 popup = ~paste("Casualties: ",dead,"<br> Affected sq. km.: ",affected.sq.km ,
                                "<br>Severity: ", severity,
                                "<br>Displaced: ", displaced,
                                "<br>Magnitude: ", magnitude,
                                "<br>Main cause: ", main.cause,
                                sep=""),
                 fillOpacity = .7 ,data = filteredData()$floods) %>%
      addLegend(pal = pal_severity,
                values = filteredData()$floods$severity,
                position = "bottomleft",
                title = "severity") %>%
      addPolylines(data = coastline, color = "black", weight = .5) %>%
      addPolylines(data = countries, color = "black", weight = .5)
    
    output$floodsMonth <- renderPlot({
      ggplot(data = filteredData()$floods %>% filter(!is.na(began_month))) + 
        geom_bar(aes(x=factor(began_month))) + 
        theme_fivethirtyeight() + 
        labs(title = "Floods by month", x = "month")
      
      
    })
    output$magnitudeMonth <- renderPlot({
      magnitude = filteredData()$floods %>% 
        group_by(began_month) %>% 
        summarize(magnitude = mean(magnitude)) 
      
      ggplot(data = magnitude %>% filter(!is.na(began_month))) + 
        geom_line(aes(x=began_month, y = magnitude)) + 
        scale_x_continuous(breaks=c(1:12))+
        theme_fivethirtyeight() +
        labs(title = "Mean Magnitude by Month", x = "month")
      
    })
    output$cause <- renderPlot({
      cause = filteredData()$floods %>% filter(!is.na(main.cause))%>%
        mutate(main.cause = tolower(as.character(main.cause))) %>%
        group_by(main.cause) %>% 
        summarize(n = n())%>%
        na.omit() %>%
        arrange(desc(n))
      # %>%
      
      cause = cause[1:5,]
      cause$main.cause = factor(cause$main.cause, levels=rev(cause$main.cause))
      ggplot(cause) + 
        geom_bar(aes(x=main.cause, y = n), stat="identity") + 
        theme_fivethirtyeight() +
        coord_flip()+
        labs(title = "Main causes of flood", x = "")
      
    })
  })
}

shinyApp(ui, server)
