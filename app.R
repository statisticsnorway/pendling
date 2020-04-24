# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
options(encoding="utf-8")

library(PxWebApiData) # For collecting in data from statbank
library(shiny)
library(leaflet)
library(leaflet.extras)
library(dplyr)
library(htmlwidgets)
library(ggplot2)     # for plotting
library(grDevices)   # for windowsFont function
library(stringr)
library(shinydashboard)
source("Dotmap_Functions.R")


# Preset fixed variables
adjA <- 13000 # Factor for circle size adjustment
adjL <- 100   # Factor for line size 
antkom <- 20  # Number of possible connections
years_all <- c("2017", "2018", "2019") # Possible selectable years
circ_size <- list("Liten" = 16000, "Middels" = 24000, "Stor" = 64000)

# set intial kommune values for choices to NULL
geodata <- NULL


# Define UI for application 
ui <- dashboardPage(
  dashboardHeader(title = "Pendlingsstrømmer", titleWidth = 280),
  dashboardSidebar(
    width = 280,
    selectInput("year", 
                label = "Velg år",
                choices = years_all,
                selected = years_all[length(years_all)]),
    
    selectizeInput("kommuneid", "Velg en kommune", 
                   choices = geodata$komm_shape$NAVN,
                   selected = NULL,
                   options = list(maxItems = 1, maxOptions = 4, 
                                  placeholder = "Skriv inn kommunenavn",
                                  onInitialize = I('function() { this.setValue(""); }')
                                  )
                   ),
    sliderInput("n","Vis antall kommuner", 1, antkom, value = 8, step = 1),
    selectInput("adjA",
                label = "Juster sirkelstørrelse",
                choices = circ_size,
                selected = 24000),
    br(),
    actionButton("reset", "Reset"),
    br(),
    br(),
    br(),
    HTML('<left><img src="ssb-logo.png", width = "200"></left>')
  ),
    
  dashboardBody(
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
    ),
    tabsetPanel(
      id = "display_panel",
      tabPanel("Bosted",
               column(8,
                      leafletOutput("map", height = 500)),
                      
                # Placement and spec. for the plot on right
                column(4, offset = 0, style='padding:0px;',
                      h1("Hvor arbeider sysselsatte personer i..."),
                      h2(uiOutput("selected_komm")), # not textOutput
                      plotOutput("plot")
                      )
               ),
                      
      tabPanel("Arbeidssted", 
               column(8,
                      leafletOutput("map_arb", height = 500)),
               
               # Placement and spec. for the plot on right
               column(4, offset = 0, style='padding:0px;',
                      h1("Hvor bor sysselsatte personer i..."),
                      h2(uiOutput("selected_komm_arb")), #not textOutput
                      #tags$head(tags$style("#selected_komm_arb{color: #274247; font-size: 16px;}")), # Open Sans not working font-family: 'Open Sans', regular;})), 
                      plotOutput("plot_arb")
               )
    )
  ),
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
  )
)
)


#### Define server function for running the output ####
server <- function(input, output, session){
  
  # Set up reactive values
  data_of_click <- reactiveValues(clickedMarker = NULL) # For clicked kommune - bosted
  data_of_click_arb <- reactiveValues(clickedMarker = NULL) # For clicked kommune - arbsted
  geodata <- reactiveValues(komm_shape = NULL)    # For kommune boundary polygons
  geodata <- reactiveValues(komm_punkt = NULL)    # For kommune center points (with data attached)
  statdata <- reactiveValues()                    # For the statistical data values
  circdata <- reactiveValues()                    # For the circle sizes (bosted)
  circdata_arb <- reactiveValues()                # For the circle sizes (arbeidssted)
    
  # Spesifications for base map - bosted
  output$map <- renderLeaflet({
    leaflet(data = geodata$komm_shape) %>%
      addProviderTiles(providers$OpenStreetMap.HOT,
                       options = providerTileOptions(opacity = 0.4)) %>%  
      addMiniMap(tiles = providers$OpenStreeMap.Mapnik,
                 toggleDisplay = TRUE,
                 width = 80, height = 100,
                 zoomLevelFixed = 2) %>%
      setView(lng=11.00, lat=59.50, zoom = 9) %>%
      addLegend(position=c("topright"), colors=c("#83C1E9","#006CB6", "#F16539"), 
                labels=c("Befolkningen 15-74 år", "Syssesatte personer i kommunen", "Sysselsattes arbeidssted"),
                opacity = 0.6)
  })
  
  # Spesifications for base map - arbeidssted
  output$map_arb <- renderLeaflet({
    leaflet(data = geodata$komm_shape) %>%
      addProviderTiles(providers$OpenStreetMap.HOT,
                       options = providerTileOptions(opacity = 0.4)) %>%  
      addMiniMap(tiles = providers$OpenStreeMap.Mapnik,
                 toggleDisplay = TRUE,
                 width = 80, height = 100,
                 zoomLevelFixed = 2) %>%
      setView(lng=11.00, lat=59.50, zoom = 9) %>%
      addLegend(position=c("topright"), colors=c("#F16539","#006CB6"), 
                labels=c("Syssesatte personer i kommunen", "Sysselsattes bosted"),
                opacity = 0.6)
  })

  
  # Reload new kommune boundaries file when year is change, update kommune name choices
  observeEvent(input$year, {

    # update shape file and save dynamically
    geo <- Load_geo_data(input$year, package = FALSE)
    geodata$komm_shape <- geo[[2]]
    geodata$komm_punkt <- geo[[1]]
    
    # update dropdown/searchable list
    updateSelectizeInput(session, "kommuneid", label = NULL, choices = geodata$komm_shape$NAVN,
                         selected = NULL, options = list(), server = FALSE)
    
    # Update statistical data for year change - same for both bosted og arbsted
      ds <- Load_stat_data(input$year, geodata$komm_punkt)
      statdata$befolk <- ds[[1]]
      statdata$syss <- ds[[2]]
      statdata$arbb <- ds[[3]]
      statdata$pend <- ds[[4]]
      statdata$mat_pop <- ds[[5]]
      
    # remove points
    data_of_click$clickedMarker$id <- NULL
    data_of_click_arb$clickedMarker$id <- NULL
})
  
    # Replot map with new kommune boundaries for new year - bosted
    observeEvent(list(input$display_panel, input$year), {
        proxy <- leafletProxy("map") %>%
          clearGroup(group = "kommuner") %>%
          addPolygons(data = geodata$komm_shape, fillColor = "#ffffff", color="#274247", 
                    weight = 0.5, smoothFactor = 0.5,
                    opacity = 0.8, fillOpacity = 0.6, label = geodata$komm_shape$NAVN,
                    highlightOptions = highlightOptions(color = "#000000", 
                                                        weight = 1, bringToFront = FALSE),
                    layerId = ~NR,
                    group = "kommuner")
        proxy
    })

  # Replot map with new boundaries (arbsted) for change in year or tab
    observeEvent(list(input$display_panel, input$year), {
    proxy_arb <- leafletProxy("map_arb") %>%
    clearGroup(group = "kommuner_arb") %>%
    addPolygons(data = geodata$komm_shape, fillColor = "#ffffff", color="#4b7272", 
                weight = 0.5, smoothFactor = 0.5,
                opacity = 0.8, fillOpacity = 0.3, label = geodata$komm_shape$NAVN,
                highlightOptions = highlightOptions(color = "#274247", 
                                                    weight = 1, bringToFront = FALSE),
                layerId = ~NR,
                group = "kommuner_arb")
    proxy_arb
  })
  
  # Create observed event for clicking on a kommune on the map - bosted
  observeEvent(input$map_shape_click, {
    data_of_click$clickedMarker <- input$map_shape_click
  })
  
  # Create observed event for clicking on a kommune on the map - arbsted
  observeEvent(input$map_arb_shape_click, {
    data_of_click_arb$clickedMarker <- input$map_arb_shape_click
  })
  
  # Create event for text input of kommune name with flyTo
  observeEvent(input$kommuneid, {
    name <- input$kommuneid
    
    #bosted
    data_of_click$clickedMarker$id <- geodata$komm_shape$NR[match(name, geodata$komm_shape$NAVN)]
    selectedKomm <- geodata$komm_punkt[geodata$komm_punkt$NR == data_of_click$clickedMarker$id, ]
    leafletProxy("map") %>%
      flyTo(lng = selectedKomm$lng, lat = selectedKomm$lat, zoom = 9)
    
    #arbsted
    data_of_click_arb$clickedMarker$id <- geodata$komm_shape$NR[match(name, geodata$komm_shape$NAVN)]
    selectedKomm <- geodata$komm_punkt[geodata$komm_punkt$NR == data_of_click_arb$clickedMarker$id, ]
    leafletProxy("map_arb") %>%
      flyTo(lng = selectedKomm$lng, lat = selectedKomm$lat, zoom = 9)
  })
  
  # Create event for click of reset button
  observeEvent(input$reset, {
    data_of_click$clickedMarker$id <- NULL
    data_of_click_arb$clickedMarker$id <- NULL 
    
    leafletProxy("map") %>%
      clearGroup(group = "circles") %>%
      removeShape(layerId = "line")
     
    leafletProxy("map_arb") %>%
      clearGroup(group = "circles_arb") %>%
      removeShape(layerId = "line")
  })
  
  
  # Observe whether adjA or strat/geo data has change and update circles sizes - bosted
  observe({
    circ <- Beregn_sirkel(as.numeric(input$adjA), statdata$befolk, statdata$syss, statdata$arbb, geodata$komm_punkt)
    circdata$pop1 <- circ[[1]]
    circdata$mat_pop <- circ[[2]]
    circdata$pop2 <- circ[[3]]
    circdata$pop11 <- circ[[4]]
    circdata$Region <- circ[[5]]
    
    circ_arb <- Beregn_sirkel_arbsted(as.numeric(input$adjA), statdata$arbb, geodata$komm_punkt)
    circdata_arb$pop_arb <- circ_arb[[1]]
  })
  
  
  # Create dynamic plot title text - bosted
  output$selected_komm <- renderUI({ #dont use renderText here as doesnt recognise øåæ
    kommid <- data_of_click$clickedMarker$id
    if (is.null(kommid)) {
      paste("Ingen kommune valgt")
    } else if (is.na(kommid)) {
      paste("Ingen kommune valgt")
    } else {
      komm_name <- geodata$komm_shape$NAVN[match(kommid, geodata$komm_shape$NR)]
      temp_num <- statdata$syss[match(kommid, statdata$syss$Region), "value"]
      HTML(paste0(komm_name, " kommune. ", input$year, "<br/>",
                  "Antall sysselsatte personer med bostedsadresse: ", temp_num, "<br/>",
                  "Arbeidssted:")
      )
    }
  })
  

  # Create dynamic plot title text - arbsted
  output$selected_komm_arb <- renderUI({ #dont use renderText here as doesnt recognise øåæ
    kommid <- data_of_click_arb$clickedMarker$id
    if (is.null(kommid)) {
      paste("Ingen kommune valgt")
    } else if (is.na(kommid)) {
      paste("Ingen kommune valgt")
    } else {
      kommune_navn <- geodata$komm_shape$NAVN[match(kommid, geodata$komm_shape$NR)]
      temp_num <- statdata$arbb[match(kommid, statdata$arbb$Region), "value"]
      HTML(paste0(kommune_navn, " kommune. ", input$year, "<br/>",
                  "Antall sysselsatte personer med arbeidsstedsadresse: ", temp_num, "<br/>",
                  "Bosted:"))
    }
  })
  
  
  # Create dynamic plot - bosted
  output$plot <- renderPlot({
    kommid <- data_of_click$clickedMarker$id #Numeric
    if (length(kommid) == 0){ plot(1, type="n", axes = F, xlab = "", ylab = "")} else {
      if (is.na(kommid)) { plot(1, type="n", axes = F, xlab = "", ylab = "")} else {
        Make_barplot(kommid, n = as.numeric(input$n), geodata$komm_shape, statdata$pend, antkom = antkom)
      }
    }
  }, bg = "transparent")
  
  # Create dynamic plot - arbsted
  output$plot_arb <- renderPlot({
    kommid <- data_of_click_arb$clickedMarker$id #Numeric
    if (length(kommid) == 0){ plot(1, type="n", axes = F, xlab = "", ylab = "")} else {
      if (is.na(kommid)) { plot(1, type="n", axes = F, xlab = "", ylab = "")} else {
        Make_barplot_arb(kommid, n = as.numeric(input$n), geodata$komm_shape, statdata$pend, antkom = antkom)
      }
    }
  }, bg = "transparent")
  
  
  #### Specify action with clicking a kommune - bosted ####
  observe({
    kommid <- as.character(data_of_click$clickedMarker$id)

    if (length(kommid) > 0) {  # check if not null 
      if (!is.na(kommid)) {    # check if not missing
        outdata <- Filter_data(kommid, n = input$n, adjA = as.numeric(input$adjA), scaleLine = FALSE, 
                               komm_punkt=geodata$komm_punkt, pend=statdata$pend, 
                               pop11=statdata$pop11, befolk=statdata$befolk, 
                               arbb=statdata$arbb)
        # outdata <- Filter_data("0101", n = 5, adjA = 24000, scaleLine =FALSE, komm_punkt=komm_punkt2018, pend=pend, pop11=pop11, befolk=befolk, arbb=arbb) #for testing
        selectedKomm <- outdata[[2]][1,]
        selectedShape <- geodata$komm_shape[geodata$komm_shape$KOMM == kommid, ]
        topShape <- geodata$komm_shape[geodata$komm_shape$KOMM %in% outdata[[1]]$KOMM, ]
        #labs <- Add_popup(topShape, befolk=statdata$befolk, syss=statdata$syss, pend=statdata$pend)
        
        proxy <- leafletProxy("map") %>%
          
        # remove old circles
        clearGroup(group = "circles") %>%
          
        # Add population for chosen circle - dark blue
        addCircles(data = selectedKomm, lat = ~lat, lng=~lng, 
    #               radius = circdata$pop1[as.numeric(geodata$komm_punkt$KOMM[statdata$mat_pop]) == as.numeric(kommid)], 
                    radius = circdata$pop1[as.numeric(circdata$Region) == as.numeric(kommid)], 
    
                    stroke = F, color = "#83C1E9", fillOpacity = 0.5, 
                   group = "circles"
        ) %>%
          
          # Add employed population in selected kommune
          addCircles(data = selectedKomm,  lat = ~lat, lng=~lng, 
                     radius = circdata$pop2[as.numeric(circdata$Region) == as.numeric(kommid)], 
                     stroke = F, color = "#006CB6", fillOpacity = 0.5, 
                     group = "circles"
          ) %>%
          
          # Add circles for employed living and working in selected kommune
          addCircles(data = outdata[[2]], lat =~lat, lng = ~lng,
                     radius = outdata[[5]], 
                     color = "#F16539", stroke = F, fillOpacity = 1, 
                     group = "circles"
          ) %>%
          
          # Add total employment in other top commute kommune - not working?
          addCircles(data = outdata[[1]],  lat = ~lat, lng=~lng, 
                     radius = outdata[[7]], 
                     stroke = F, color = "#F16539", fillOpacity = 0.2, 
                     group = "circles"
          ) %>%
          
          # Add communing lines
          addPolylines(data = outdata[[3]], lng = ~lng, lat = ~lat, 
                       group = ~group, 
                       weight = outdata[[6]] * adjL, # width of lines
                       color = "#F16539", stroke = TRUE, opacity = 0.6,
                       layerId = ~type
          ) 

        proxy
      }
    }
  })
  
  #### Specify action with clicking a kommune - arbsted ####
  observe({
    kommid <- as.character(data_of_click_arb$clickedMarker$id)
    
    if (length(kommid) > 0) {  # check if not null 
      if (!is.na(kommid)) {    # check if not missing
        outdata <- Filter_data_arb(kommid, n = input$n, 
                                   adjA = as.numeric(input$adjA), 
                                   scaleLine = FALSE, 
                                   komm_punkt=geodata$komm_punkt, 
                                   pend=statdata$pend, 
                                   pop_arb=statdata$arbb)
        # outdata <- Filter_data_arb("0101", n = 5, adjA = 24000, scaleLine =FALSE, komm_punkt=komm_punkt2018, pend=pend, pop_arb=arbb) #for testing
        selectedKomm <- outdata[[2]][1,]
        selectedShape <- geodata$komm_shape[geodata$komm_shape$KOMM == kommid, ]
        topShape <- geodata$komm_shape[geodata$komm_shape$KOMM %in% outdata[[1]]$KOMM, ]
        #labs <- Add_popup(topShape, befolk=statdata$befolk, syss=statdata$syss, pend=statdata$pend)
      
        radius_select <- circdata_arb$pop_arb[as.numeric(statdata$arbb$Region) == as.numeric(kommid)]
        
        proxy <- leafletProxy("map_arb") %>%
          
          # remove old circles
          clearGroup(group = "circles_arb") %>%
          
          # Add employed population working in own kommune
          addCircles(data = selectedKomm,  lat = ~lat, lng=~lng, 
                     radius = radius_select, 
                     stroke = F, color = "#F16539", fillOpacity = 0.8, 
                     group = "circles_arb"
          ) %>%
    
          #Add circles for employed living and working in selected top kommune
          addCircles(data = outdata[[2]], lat =~lat, lng = ~lng,
                     radius = outdata[[5]], 
                     color = "#006CB6", stroke = F, fillOpacity = 0.8, 
                     group = "circles_arb"
          ) %>%
          
          # Add communing lines
          addPolylines(data = outdata[[3]], lng = ~lng, lat = ~lat, 
                       group = ~group, 
                       weight = outdata[[6]] * adjL, # width of lines
                       color = "#006CB6", stroke = TRUE, opacity = 0.6,
                       layerId = ~type
          ) 
        
        proxy
      }
    }
  })

  
}

#### Run app ####
shinyApp(ui, server)
