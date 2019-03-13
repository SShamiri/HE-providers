library(shiny)
library(shinyjqui)
library(shinydashboard)
library(shinydashboardPlus)
library(shinyAce)
library(styler)
library(shinyWidgets)
library(tidyverse)
library(rmapshaper)
library(geojsonio)
library(sp)
library(sf)
library(leaflet)
library(readxl)
library(DT)
library(plotly)
#source('dropdownMenuCustom_fn.r',local = TRUE)
source("global.R", local=TRUE)
### Data
#setwd('C:/Users/Samuel/Documents/AppDev/HE_providers/app/HE_porviders')
mapData = read_rds('data/mapdat.rds')

###################
dashboard_header = dashboardHeaderPlus(
  fixed = TRUE,
  title = "Universities",
  dropdownMenuCustom(type = "messages",
                     customSentence = customSentence,
                     icon = icon("info-circle"),
                     badgeStatus = NULL,
                     messageItem(from = 'Samuel Shamiri', href = "mailto:samuelshamiri@yahoo.com",message = "",icon = icon("envelope")),
                     messageItem(from = 'Source code', href = "https://github.com/SShamiri",message = "",icon = icon("fab fa-github-square", "fa-2x")),
                     messageItem(from = 'LinkedIn', href = "https://au.linkedin.com/pub/samuel-shamiri/2a/701/530",message = "",icon = icon("linkedin-square", "fa-2x"))
  ),
  tags$li(a(href = '',img(src = 'DNA2.jpg',
                title = "Samuel Shamiri", height = "30px"),
                style = "padding-top:10px; padding-bottom:10px;"),
                class = "dropdown")
  
)
## sidebar
sidebar =  dashboardSidebar(
  sidebarMenu(
    br(),
    div(img(src="DNA2.jpg",height="100px")),br(),p("Samuel Shamiri"),
    a(href= "https://twitter.com/SamuelShamiri",target="_blank",icon("twitter-square", "fa-2x")),
    a(href= "https://au.linkedin.com/pub/samuel-shamiri/2a/701/530",target="_blank",icon("linkedin-square", "fa-2x")),
    a(href= "https://github.com/SShamiri",target="_blank",icon("fab fa-github-square", "fa-2x") )
    
   
  )
) 

## body
dashboarbody = dashboardBody(
  
  setShadow("dropdown-menu"),
  
  shiny::tags$head(
    # shiny::includeCSS(
    #   system.file("css", "qtcreator_dark.css", package = "shinydashboardPlus")
    # ),
    # shiny::includeScript(
    #   system.file("js", "highlight.pack.js", package = "shinydashboardPlus")
    # )
    
    shiny::tags$style(
      rel = "stylesheet",
      type = "text/css",
      href = "https://cdnjs.cloudflare.com/ajax/libs/highlight.js/9.12.0/styles/qtcreator_dark.min.css"
    ),
    
    shiny::tags$script(
      src = "https://cdnjs.cloudflare.com/ajax/libs/highlight.js/9.12.0/highlight.min.js"
    )
    
  ),
  br(), br(),
  fluidRow( column(width = 8,
                   box(
                     width = 12, status = "info", solidHeader = TRUE,
                     title = "Australian Universities",
                     leafletOutput("map", height="600px")
                   )),
            column(width = 4,
                   box(width = 12, status = "warning", solidHeader = TRUE,
                       title = "Completion Rate",
                       fluidRow(
                         # # A static infoBox
                         # infoBox("New Orders", 10 * 2, icon = icon("credit-card"),width = 6),
                         # # Dynamic infoBoxes
                         # infoBoxOutput("progressBox",width = 6)
                         p(HTML(paste0('Table A institution and Table B institution commencing 
                                       bachelor students over a six year period, 2012-201',
                                       a(href = 'https://docs.education.gov.au/node/51501', '[reference]',
                                         target="_blank")))),style='padding:10px;'
                         
                         
                       ),
                       fluidRow(
                         # infoBox("New Orders", 10 * 2, icon = icon("credit-card"), fill = TRUE,width = 6),
                         # infoBoxOutput("progressBox2",width = 6)
                         plotlyOutput('PlotCohort'),style='padding:5px;'
                         
                       ),
                       fluidRow(
                         infoBoxOutput("approvalBox",width = 6),
                         infoBoxOutput("approvalBox2",width = 6)
                       )
                       
            )
                         )
  ),
  fluidRow(
    box(
      width = 12, status = "info",
      title = "University campuses",
      DTOutput('tbl'))
  )
  
)


## title
title = "shinyDashboardPlus"

##
ui = dashboardPagePlus(dashboard_header,sidebar,dashboarbody ,title)

server = function(input, output) {
  # create a reactive value that will store the click position
  data_of_click = reactiveValues(clickedMarker=NULL)
  
  # Leaflet map with 2 markers
  output$map = renderLeaflet({
    leaflet(mapData$univCoord) %>% addTiles() %>% 
      addPolygons(data = mapData$elect,
                  color = "#444444", weight = 1, smoothFactor = 0.5,
                  opacity = 1.0, fillOpacity = 0.5,
                  fillColor = ~mapData$pal(State),
                  label = mapData$elect$Elect_div,
                  highlightOptions = highlightOptions(color = "white", weight = 2,
                                                      bringToFront = TRUE)) %>%
      addMarkers(~longitude, ~latitude, popup = ~as.character(display_name), 
                 label = ~as.character(query),layerId = ~query)
  })
  
  # store the click
  observeEvent(input$map_marker_click,{
    data_of_click$clickedMarker <- input$map_marker_click
  })
  
  ## Boxes
  uni_rate = reactive({
    uni_rate = data_of_click$clickedMarker$id
    if(is.null(uni_rate)){ 
      tmp = data.frame(`2011_2016` = 'select a uni',`2012_2017` = 'select a uni',stringsAsFactors = FALSE)
    } else{
      tmp = mapData$Comprate %>% filter(query==data_of_click$clickedMarker$id)
    }
    return(tmp)
  })
  
  
  output$approvalBox <- renderInfoBox({
    if(is.numeric(uni_rate()$`2011_2016`)) {
      infoBox(
        "2011-2016",value = uni_rate()$`2011_2016` , 
        # icon = icon("thumbs-up", lib = "glyphicon"),
        icon = if (uni_rate()$`2011_2016` > uni_rate()$`2012_2017`) 
          icon("arrow-circle-o-up") else icon("arrow-circle-o-down"),
        color =  "purple"
      ) 
    } else{
      infoBox(
        "2011-2016",value = tags$p('select a university on the map', style = "font-size: 70%;"),
        color =  "purple"
      ) 
    }
  })
  
  output$approvalBox2 <- renderInfoBox({
    if(is.numeric(uni_rate()$`2011_2016`)) {
      infoBox(
        "2012-2017", uni_rate()$`2012_2017`, 
        #icon = icon("thumbs-up", lib = "glyphicon"),
        icon = if ( uni_rate()$`2012_2017` > uni_rate()$`2011_2016`) 
          icon("arrow-circle-o-up") else icon("arrow-circle-o-down"),
        color =  "purple"#, fill = TRUE
      ) 
    } else{
      infoBox(
        "2012-2017", value = tags$p('select a university on the map', style = "font-size: 70%;"),
        color =  "purple"#, fill = TRUE
      ) 
    }
  })
 
  
  
  
  # render DT for uni campuses
  output$tbl = DT::renderDataTable({
    uni_selected = data_of_click$clickedMarker$id
    if(is.null(uni_selected)){ 
      datatable(mapData$univCoord %>% 
                  select(Campus = display_name, State,electorate, longitude,latitude)) 
    } else{
      mapData$univCoord %>% filter(query==data_of_click$clickedMarker$id) %>%
        select(Campus = display_name, State,electorate, longitude,latitude)
    }
    
  })
  
  
  # plot
  output$PlotCohort = renderPlotly({
    p = ggplot(data=mapData$domestic_overseas, aes(x=Cohort, y=Rate, fill=Students)) +
      geom_bar(colour="black", stat="identity",
               position=position_dodge(),
               size=.3) +                        # Thinner lines
      xlab("") + ylab("%") +
      guides(fill=FALSE) +
      theme_bw() 
    
    
    p <- ggplotly(p) %>%
      layout(legend = list( orientation = "h",  x = 0.3,y =1.11, font = list(
        family = "sans-serif",
        size = 8,
        color = "#000") )) %>% config(displaylogo = FALSE, collaborate = FALSE)
    p
  })
  
}

shinyApp(ui,server)  
