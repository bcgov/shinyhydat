# Copyright 2018 Province of British Columbia
# 
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
# 
# http://www.apache.org/licenses/LICENSE-2.0
# 
# Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and limitations under the License.

# fix adding years to plots
# water year month to plot
# Add "Download" text to buttons
# RT Display long-term mean
# add column to meta with dates, so filter for dates   left_join(hy_stn_data_range(), by = "STATION_NUMBER") %>% 



default_station <- "08NM116"

prov_list <- c("AB","BC","SK","MB","ON","QC","NB","NS","PE","NL","YT","NT","NU") 
  #"BC"

library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(dplyr) ## >0.7.0 dplyr
library(tidyr)
library(leaflet)
library(tidyhydat)
library(plotly)
library(httr)
library(DT)
library(shinycssloaders)

options(shiny.sanitize.errors = TRUE)

## Create a dataframe of all station metadata and a list of all stations
stations_all <- hy_stations(prov_terr_state_loc = prov_list) %>%  #
  left_join(hy_agency_list(), by = c("CONTRIBUTOR_ID" = "AGENCY_ID")) %>% 
  rename("CONTRIBUTOR" = AGENCY_EN) %>% 
  left_join(hy_agency_list(), by = c("OPERATOR_ID" = "AGENCY_ID")) %>%  
  rename("OPERATOR" = AGENCY_EN) %>% 
  left_join(hy_datum_list(), by = c("DATUM_ID" = "DATUM_ID")) %>% 
  rename("DATUM" = DATUM_EN) %>% 
  mutate(REGIONAL_OFFICE_ID = as.integer(REGIONAL_OFFICE_ID)) %>% 
  left_join(hy_reg_office_list(), by = c("REGIONAL_OFFICE_ID" = "REGIONAL_OFFICE_ID")) %>% 
  rename("REGIONAL_OFFICE" = REGIONAL_OFFICE_NAME_EN) %>% 
  left_join(hy_stn_regulation(), by = "STATION_NUMBER") %>% 
  select(STATION_NUMBER, STATION_NAME, PROV_TERR_STATE_LOC, HYD_STATUS, LATITUDE, LONGITUDE, DRAINAGE_AREA_GROSS, RHBN, 
         REAL_TIME, REGULATED, CONTRIBUTOR, OPERATOR, REGIONAL_OFFICE, DATUM)
station_parameters <- hy_stn_data_range() %>% filter(DATA_TYPE == "Q" | DATA_TYPE == "H")  %>% 
  select(STATION_NUMBER, DATA_TYPE) %>% spread(DATA_TYPE, DATA_TYPE) %>% 
  mutate(PARAMETER=ifelse(is.na(H), "Flow", ifelse(is.na(Q), "Level", paste("Flow and Level"))))
stations_all <- left_join(stations_all, station_parameters %>% select(STATION_NUMBER, PARAMETER), by = "STATION_NUMBER") 

stations_list <- as.list(unique(stations_all$STATION_NUMBER))


#######################################################################################
### # Set up the user-interface
#######################################################################################

ui <- dashboardPage(
  dashboardHeader(title="shinyhydat"),
  dashboardSidebar(
    fluidPage(
      br(),
      h5("Select or type your hydrometric station number then click the 'Select' button to choose."),
      uiOutput("stnSelect"),
      actionButton("selectStation","Select"),
      hr(),
      h5("About:"),
      h5("This app extracts hydrometric discharge and water level data from the HYDAT database and displays station metadata, 
         historical data, and real-time data, if available. A locally saved SQLite HYDAT database file is required."),
      br(),
      h4("HYDAT versions:"),
      textOutput("local_HYDAT_vers"),
      textOutput("online_HYDAT_vers"),br(),br(),
      textOutput("test")#,
      #actionButton("downloadHYDAT","Download HYDAT")  #NEED TO MOVE QUESTION FROM CONSOLE TO WINDOW
    )
    
    
  ),
  dashboardBody(
    fluidPage(
      tabBox(width = 12,
             tabPanel("Station Listings",
                      fluidRow(column(width = 8,
                                      helpText("Search for a station by entering all or part of a station name, number, or 
                                               other categories. To view station information and hydrometric data, click 
                                               on the row and view the other tabs. To search by map, go to the 'Stations 
                                               Map' tab and click on the marker of your desired station. Each map or table 
                                               selection will replace the previous selection."),
                                      helpText("The table below (filtered or not) can be downloaded as a .csv file with the 
                                               download button the right. To display stations listed below (filtered or not) 
                                               on the 'Stations Map' tab, click the button to the right. To clear any 
                                               filters on the map, clear all filters in the table and re-click the button.")),
                               column(width=2,
                                      downloadButton('downloadStationsOut', 'Download Filtered Table'),br(),br(),
                                      actionButton('stationsMapAdd', 'Show Filtered Stations on Map')
                               )),
                      br(),
                      shinycssloaders::withSpinner(DT::dataTableOutput("allstationsTableOut"))
                      
             ),
             tabPanel("Stations Map",
                      br(),
                      fluidPage(column(width=8,
                                       # tags$style(type = "text/css", "#map {height: calc(100vh - 170px) !important;}"),
                                       shinycssloaders::withSpinner(leafletOutput("map",width="100%",height="650px"))),
                                column(width=4,
                                       box(width=12,status = "primary",
                                           h4("Map Settings"),br(),
                                           selectInput("mapColor","Station colour:",
                                                       choices = c("None","Station Status"="HYD_STATUS",
                                                                   "Reference (RHBN)"="RHBN",
                                                                   "Real-time"="REAL_TIME",
                                                                   "Regulated"="REGULATED",
                                                                   "Data Type"="PARAMETER"),
                                                       selected = "None"),
                                           checkboxInput("mapSelected","Display selected station",
                                                         value = TRUE),
                                           checkboxInput("mapRadius","Adjust size of marker to drainage basin area",
                                                         value = FALSE)
                                       )))),
             tabPanel("Station Info",
                      br(),
                      fluidRow(column(width = 6,
                                      box(width=12,title="Station Information",status = "primary",solidHeader = TRUE,
                                          shinycssloaders::withSpinner(tableOutput("metaTable")))),
                               column(width = 6,
                                      box(width=12,background="light-blue",
                                          shinycssloaders::withSpinner(leafletOutput("stnmap")))))),
             tabPanel("Historical Data",
                      fluidRow(column(width=4,
                                      selectInput("histView",
                                                  label="Historical data type to view:",
                                                  choices = list("Long-term","Annual","Monthly", "Daily"))),
                               column(width=8,
                                      uiOutput("histYears"))),
                      fluidPage(hr()),
                      # Historical Long-term
                      conditionalPanel(
                        condition = "input.histView == 'Long-term'",
                        fluidRow(column(width=6,h4(textOutput("ltplot.title"))),
                                 column(width=3, 
                                        downloadButton('download.ltData', 'Complete Daily Dataset'))),
                        br(),
                        fluidRow(
                          tabBox(width = 12,
                                 tabPanel("Graph",
                                          fluidRow(column(width=12,
                                                          shinycssloaders::withSpinner(plotlyOutput('ltplot')))),
                                          br(),br(),
                                          fluidRow(box(width = 6,title = "Graph Options",status = "primary",
                                                       fluidRow(column(width=5,
                                                                       hr(),
                                                                       uiOutput("ltParam"),
                                                                       checkboxInput("ltlog", 
                                                                                     label = "Log scale on 'Discharge' axis", 
                                                                                     value= FALSE)),
                                                                column(width=2),
                                                                column(width=5,
                                                                       hr(),
                                                                       uiOutput("paramSymbol")))
                                          ))
                                 ),
                                 tabPanel("Table",
                                          fluidRow(column(width=9,
                                                          DT::dataTableOutput("ltTable")),
                                                   column(width=3,
                                                          br(),br(),br(),
                                                          h4("Symbols"),h5("E = Estimate"),h5("A = Partial Day"),
                                                          h5("B = Ice conditions"),h5("D = Dry"),h5("R = Revised"))
                                          )
                                 )
                          )
                        )),
                      # Historical Annual
                      conditionalPanel(
                        condition = "input.histView == 'Annual'",
                        fluidRow(column(width=6,h4(textOutput("annualPlot.title"))),
                                 column(width=6, 
                                        downloadButton('download.annualData', 
                                                       label='Annual Summary Data'), 
                                        downloadButton('download.annualPeakData', 
                                                       label='Annual Peak Data'))),
                        br(),
                        fluidRow(
                          tabBox(width = 12,
                                 tabPanel("Graph",
                                          fluidRow(column(width=12)),
                                          fluidRow(column(width=12,
                                                          shinycssloaders::withSpinner(plotlyOutput('annualPlot')))),
                                          h5("* Mean values produced only for years of complete data"),
                                          br(),br(),
                                          fluidRow(box(width = 6,title = "Graph Options",status = "primary",
                                                       fluidRow(column(width=5,hr(),
                                                                       uiOutput("annualParam"), 
                                                                       checkboxInput("annuallog", 
                                                                                     label = "Log scale on primary Y-axis", 
                                                                                     value= FALSE)),
                                                                column(width=2),
                                                                column(width=5,hr(),
                                                                       uiOutput("annualStat"),
                                                                       uiOutput("annInstStat")))
                                          ))
                                 ),
                                 tabPanel("Table",
                                          fluidRow(column(width=7,
                                                          DT::dataTableOutput("annualTable"))))
                          )
                        )
                      ),
                      # Historical Monthly
                      conditionalPanel(
                        condition = "input.histView == 'Monthly'",
                        fluidRow(column(width=6,h4(textOutput("monthPlot.title"))),
                                 column(width=6, 
                                        downloadButton('download.monthData', 'Monthly Summary Data'),
                                        downloadButton('download.HYDATmonthData', 'Monthly HYDAT Data'))),
                        br(),
                        fluidRow(
                          tabBox(width = 12,
                                 tabPanel("Graph",
                                          conditionalPanel(
                                            condition = "input.monthPlotType == 'Continuous'",
                                            fluidRow(column(width=12,
                                                            shinycssloaders::withSpinner(plotlyOutput('monthPlot'))))),
                                          conditionalPanel(
                                            condition = "input.monthPlotType == 'Box Plot'",
                                            fluidRow(column(width=12,
                                                            shinycssloaders::withSpinner(plotlyOutput('monthPlot2')),
                                                            h5("*** LIST WHAT THE BOXES ARE")))),
                                          h5("* Missing dates ignored"),
                                          br(),br(),
                                          fluidRow(box(width = 9,title = "Graph Options",status = "primary",
                                                       fluidRow(column(width=3,hr(),
                                                                       uiOutput("monthParam"),
                                                                       selectInput("monthPlotType","Type of graph:",
                                                                                   choices = c("Continuous","Box Plot"),
                                                                                   selected = "Continuous"),
                                                                       checkboxInput("monthlog", 
                                                                                     label = "Log scale on primary Y-axis", 
                                                                                     value= FALSE)),
                                                                column(width=1),
                                                                conditionalPanel(
                                                                  condition = "input.monthPlotType == 'Continuous'",
                                                                  column(width=4,hr(),
                                                                         uiOutput("monthStat"),
                                                                         checkboxInput("monthMaxMin",
                                                                                       label = "Display Maximum-Minimum range",
                                                                                       value=TRUE),
                                                                         checkboxInput("month90",
                                                                                       label = "Display 5-95 percentile range",
                                                                                       value=TRUE),
                                                                         checkboxInput("month50",
                                                                                       label = "Display 25-75 percentile range",
                                                                                       value=TRUE))),
                                                                column(width=1),
                                                                conditionalPanel(
                                                                  condition = "input.monthPlotType == 'Continuous'",
                                                                  column(width=3,hr(),
                                                                         uiOutput("monthYear"),
                                                                         uiOutput("monthYearStat"))))))
                                 ),
                                 tabPanel("Table",
                                          fluidRow(column(width=12,selectInput("monthTableType",
                                                                               "Select monthly data type to view:",
                                                                               choices = c("Long-term statistics for each month"=1,
                                                                                           "Monthly statistics from all years"=2)))),
                                          conditionalPanel(
                                            condition = "input.monthTableType == 1",
                                            fluidRow(column(width=12, materialSwitch("monthTableSwitch",
                                                                                     "Switch rows and columns",
                                                                                     value=FALSE,right=TRUE, 
                                                                                     status = "primary"))),
                                            fluidRow(column(width=12, DT::dataTableOutput("monthTable")))),
                                          conditionalPanel(
                                            condition = "input.monthTableType == 2",
                                            fluidRow(column(width=8, DT::dataTableOutput("monthHYDATTable")))
                                          )
                                 )
                          )
                        )
                      ),
                      # Historical daily
                      conditionalPanel(
                        condition = "input.histView == 'Daily'",
                        fluidRow(column(width=6,h4(textOutput("dailyPlot.title"))),
                                 column(width =6, 
                                        downloadButton('download.dailySummaryData', 'Daily Summary Data'))),
                        br(),
                        fluidRow(
                          tabBox(width = 12,
                                 tabPanel("Graph",
                                          fluidRow(column(width=12,
                                                          shinycssloaders::withSpinner(plotlyOutput('dailyPlot')))),
                                          br(),br(),
                                          fluidRow(box(width=9,title = "Graph Options",status = "primary",
                                                       fluidRow(column(width=3,hr(),
                                                                       uiOutput("dailyParam"),
                                                                       checkboxInput("dailylog", 
                                                                                     label = "Log scale on primary Y-axis",
                                                                                     value= FALSE)),
                                                                column(width=1),
                                                                column(width=4,hr(),
                                                                       uiOutput("dailyStat"),
                                                                       checkboxInput("dailyMaxMin",
                                                                                     label = "Display Maximum-Minimum range",
                                                                                     value=TRUE),
                                                                       checkboxInput("daily90",
                                                                                     label = "Display 5-95 percentile range",
                                                                                     value=TRUE),
                                                                       checkboxInput("daily50",
                                                                                     label = "Display 25-75 percentile range",
                                                                                     value=TRUE)),
                                                                column(width=1),
                                                                column(width=3,hr(),
                                                                       uiOutput("dailyYear")))
                                          ))
                                 ),
                                 tabPanel("Table",
                                          fluidRow(column(width=12,DT::dataTableOutput("dailyTable")))
                                 )
                                 
                          )))
             ),
             # Real-time data
             tabPanel("Real-time Data",
                      #fluidRow(column(6)),
                      conditionalPanel(
                        condition = "output.rtExists=='No'",
                        textOutput("noRT")),
                      conditionalPanel(
                        condition = "output.rtExists=='Yes'",
                        fluidRow(column(6,h4(textOutput("rtplot.title"))),
                                 column(6, downloadButton('download.rtData', 'Real-time Data'))),
                        br(),
                        fluidRow(
                          tabBox(width = 12,
                                 tabPanel("Graph",
                                          fluidRow(column(12,
                                                          shinycssloaders::withSpinner(plotlyOutput('rtplot')))),
                                          br(),br(),
                                          fluidRow(box(width =8,title = "Graph Options",status = "primary",
                                                       fluidRow(column(5,hr(),
                                                                       uiOutput("rtParam"),
                                                                       checkboxInput("rtlog", 
                                                                                     label = "Log scale on 'Discharge' axis", 
                                                                                     value= FALSE),
                                                                       checkboxInput("rtLTMEAN",
                                                                                     "Display long-term mean",
                                                                                     value=FALSE),
                                                                       checkboxInput("rtLTMEANPCT",
                                                                                     "Display percentage of long-term mean",
                                                                                     value=FALSE),
                                                                       numericInput("rtLTMEANPCTvalue",
                                                                                    "Percentage",
                                                                                    value=100,
                                                                                    min=0,
                                                                                    step=5)),
                                                                column(1),
                                                                column(6,hr(),
                                                                       checkboxInput("rtHistoric","Display historic daily data",value= FALSE),
                                                                       uiOutput("rtHistParam"),
                                                                       checkboxGroupInput("rtHistStats",
                                                                                          "Display the historic daily statistics:",
                                                                                          choices = list("Max-Min Range",
                                                                                                         "5-95 Percentile Range",
                                                                                                         "25-75 Percentile Range",
                                                                                                         "Mean","Median"),
                                                                                          selected = list("Max-Min Range",
                                                                                                          "5-95 Percentile Range",
                                                                                                          "25-75 Percentile Range"))
                                                                ))))),
                                 tabPanel("Table",
                                          fluidRow(column(width=6,
                                                          (DT::dataTableOutput("realtimeTable")))))))),
                      textOutput("rtExists"),tags$head(tags$style("#rtExists{color: white}")),
             ),
             # Station comparison
             tabPanel("Station Comparison",
                      h4("Under development"),icon("smile-o", lib = "font-awesome"))
      )
    )
  )
)

#######################################################################################
# Set up the server (where all the magic happens)
#######################################################################################

server <- function(input, output, session) {
  
  
  ### Check HYDAT version ###
  ###########################
  
  output$online_HYDAT_vers <- renderText({
    base_url <- "http://collaboration.cmc.ec.gc.ca/cmc/hydrometrics/www/"
    x <- httr::GET(base_url)
    new_hydat <- substr(gsub(
      "^.*\\Hydat_sqlite3_", "",
      httr::content(x, "text")
    ), 1, 8)
    paste0("Available: ", as.Date(new_hydat, "%Y%m%d"))
  })
  output$local_HYDAT_vers <- renderText({
    paste0("Local: ", as.Date(as.data.frame(hy_version())[,2]))
  })
  
  ### Setting up the Stations Listings ###
  ########################################
  
  allstationsTableReact <- reactive({
    
    stn.meta.HYDAT <- stations_all %>% filter(STATION_NUMBER == stations_list)
    
    stn.info <-stn.meta.HYDAT[ ,c(1:3,15,4,7:13)] %>% 
      mutate(DRAINAGE_AREA_GROSS = round(DRAINAGE_AREA_GROSS, 2)) %>% 
      rename("Station Number" = STATION_NUMBER, "Station Name" = STATION_NAME, "Prov/ Terr/ State" = PROV_TERR_STATE_LOC,
             "Station Status" = HYD_STATUS,"Parameter" = PARAMETER, "Drainage Area (sq km)" = DRAINAGE_AREA_GROSS, "Reference (RHBN)" = RHBN,
             "Real-Time" = REAL_TIME, "Contributor" = CONTRIBUTOR, "Operator" = OPERATOR, "Regional Office" = REGIONAL_OFFICE,
             "Regulated" = REGULATED)
  }) 
  
  output$allstationsTableOut <- DT::renderDataTable(
    allstationsTableReact(), 
    rownames = FALSE,
    selection = list(mode = "single"),
    filter = 'top',
    extensions = c("Scroller","ColReorder","Buttons"),
    options = list(scrollX = TRUE,
                   scrollY = 450, deferRender = TRUE, scroller = TRUE,
                   dom = 'Bfrtip', 
                   colReorder = TRUE,
                   buttons= list(list(extend = 'colvis', columns = c(1:10))))
  ) 
  
  proxy = DT::dataTableProxy('allstationsTableReact')   # Allows for updating the selected row from the select-widget and the map
  
  ## Format the station lists for download
  downloadStationsListReact <- reactive({
    list <- stations_all %>% 
      mutate(row = c(1:(n()))) %>% 
      filter(row %in% input$allstationsTableReact_rows_all) %>% 
      select(-row)
    list
  })
  
  
  output$downloadStationsOut <- downloadHandler(
    filename = function() {paste0("Stations_Table.csv")},
    content = function(file) {
      write.csv(downloadStationsListReact(), file, row.names = FALSE, na = "")
    })
  
  
  ### Setting the station ###
  ###########################
  
  # Create reactive values$station to be selected by the selectbox, data.table and map click
  values <- reactiveValues()
  values$station <- default_station
  
  output$test <- renderText({values$station})
  
  # Select station with widget
  output$stnSelect <- renderUI({
    selectizeInput("station", label = "Station Number:",
                   choices = stations_list, 
                   selected = values$station,
                   options = list(placeholder ="type station ID number", maxOptions = 2420 ))
  })
  
  # Select station by clicking on button for widget
  observeEvent(input$selectStation, {
    isolate(values$station <- input$station)
  })
  
  # Select station by clicking on row in datatable
  observeEvent(input$allstationsTableReact_rows_selected, {
    isolate(values$station <- as.data.frame(allstationsTableReact())[input$allstationsTableReact_rows_selected,1])
  })
  
  # Select station by clicking on station in map
  observeEvent(input$map_marker_click, {
    isolate(values$station <- input$map_marker_click$id)
    updateSelectizeInput(session, "station", selected = input$map_marker_click$id)
    proxy %>% DT::selectRows(which(stations_list == values$station))
  })
  
  
  ### Create the Stations Map  ###
  ################################.
  
  output$map <- renderLeaflet({
    leaflet(stations_all) %>% addTiles()%>% 
      addCircleMarkers(layerId = "selected",
                       data = filter(stations_all, STATION_NUMBER %in% values$station),
                       lng = ~LONGITUDE, lat = ~LATITUDE, 
                       color = "red", radius = 4) %>%
      addCircleMarkers(data= stations_all, lng = ~LONGITUDE, lat = ~LATITUDE, layerId = ~STATION_NUMBER, 
                       color = "blue", radius = 3, fillOpacity = .8, stroke = FALSE,
                       label = ~paste0(STATION_NAME, " (",STATION_NUMBER,") - ",HYD_STATUS)
                       #,clusterOptions = markerClusterOptions()
      )
  })
  
  ## Change the map mark colour a sizes based on the inputs
  observe({
    
    # set the radius size based on drainage area if selected
    if (input$mapRadius){
      rad <- log(stations_all[["DRAINAGE_AREA_GROSS"]])
      rad2 <- rad+.5
    } else {
      rad <- 3
      rad2 <- 4
    }
    
    # if a color category is chosen, add the colors and legend
    if (input$mapColor != "None") {
      colorBy <- input$mapColor
      colorData <- stations_all[[colorBy]]
      pal <- colorFactor(c("#4daf4a",
                           "#377eb8",
                           "#e41a1c",
                           "#984ea3"), 
                         unique(colorData))
      
      leg.title <- ifelse(input$mapColor == "HYD_STATUS","Station Status",
                          ifelse(input$mapColor == "RHBN","Reference (RHBN)",
                                 ifelse(input$mapColor == "REAL_TIME","Real-time",
                                        ifelse(input$mapColor == "REGULATED","Regulated",
                                               ifelse(input$mapColor == "PARAMETER","Data Type")))))
      
      leafletProxy("map") %>%
        clearMarkers()%>% 
        addCircleMarkers(data= stations_all, lng = ~LONGITUDE, lat = ~LATITUDE, layerId = ~STATION_NUMBER, 
                         color = pal(colorData), radius = rad, stroke = FALSE, fillOpacity = .8,
                         label = ~paste0(STATION_NAME, " (",STATION_NUMBER,") - ",HYD_STATUS)) %>%
        addLegend("topright", pal = pal, values = colorData, title = leg.title,
                  layerId = "colorLegend")
    } else {
      leafletProxy("map") %>%
        clearMarkers()%>% 
        clearControls() %>% 
        addCircleMarkers(data= stations_all, lng = ~LONGITUDE, lat = ~LATITUDE, layerId = ~STATION_NUMBER, 
                         color = "blue", radius = rad, stroke = FALSE, fillOpacity = .8,
                         label = ~paste0(STATION_NAME, " (",STATION_NUMBER,") - ",HYD_STATUS))
    }
    
    # add or remove the selected station (the size will change with the radius)
    if (input$mapSelected) {
      leafletProxy("map") %>%
        removeMarker(layerId = "selected") %>%
        addCircleMarkers(layerId = "selected",
                         data = filter(stations_all, STATION_NUMBER == values$station),
                         lng = ~LONGITUDE,lat = ~LATITUDE,
                         color = "red", radius = rad2)
    } else {
      leafletProxy("map")  %>%
        removeMarker(layerId = "selected")
    }
    
  })
  
  
  # Displayed the filtered stations on the map
  observeEvent(input$stationsMapAdd, {
    leafletProxy("map") %>%
      clearMarkers() %>%
      clearControls() %>% 
      addCircleMarkers(data= downloadStationsListReact(), 
                       lng = ~LONGITUDE, lat = ~LATITUDE, 
                       layerId = ~STATION_NUMBER, 
                       color = "blue", radius = 2,
                       label = ~paste0(STATION_NAME, " (",STATION_NUMBER,") - ",HYD_STATUS))
  })
  
  
  ### Displaying Station Information ###
  ######################################
  
  # Filter and format table for chosen station (used for rendering output and looking up parameters)
  metaData <- reactive({
    
    stn.meta.HYDAT <- stations_all %>% filter(STATION_NUMBER==values$station)
    
    stn.info <- stn.meta.HYDAT %>% 
      mutate("Historical Data Link" = paste0("https://wateroffice.ec.gc.ca/report/historical_e.html?stn=", STATION_NUMBER),
             "Real-Time Data Link" = ifelse(REAL_TIME == TRUE, paste0("https://wateroffice.ec.gc.ca/report/real_time_e.html?stn=", STATION_NUMBER),"No real-time data available."),
             DRAINAGE_AREA_GROSS = round(DRAINAGE_AREA_GROSS, 2),
             LATITUDE = round(LATITUDE, 6),
             LONGITUDE = round(LONGITUDE, 6)) %>% 
      select(c(1:3,15,5,6,4,8:10,7,11:14,16:17)) %>% 
      rename("Station Number" = STATION_NUMBER,"Station Name" = STATION_NAME,"Prov/Terr/State"=PROV_TERR_STATE_LOC,
             "Station Status" = HYD_STATUS,"Parameter" = PARAMETER, "Latitude" = LATITUDE, "Longitude" = LONGITUDE, "Drainage Area (sq km)" = DRAINAGE_AREA_GROSS,
             "Reference (RHBN)" = RHBN, "Real-Time" = REAL_TIME, "Regulated" = REGULATED, "Regional Office" = REGIONAL_OFFICE,
             "Contributor" = CONTRIBUTOR, "Operator" = OPERATOR, "Datum" = DATUM) %>% 
      gather("header","content",1:17)
    
    stn.info[is.na(stn.info)] <- ""
    stn.info
  })
  
  # Render table for output
  output$metaTable <- renderTable(metaData(),colnames = FALSE)
  
  # Render map of chosen station
  output$stnmap <- renderLeaflet({
    leaflet(stations_all) %>% addTiles() %>%
      setView(lng = as.numeric(metaData()[6,2]), lat = as.numeric(metaData()[5,2]), zoom = 9) %>% # set centre and extent of map
      addCircleMarkers(data = filter(stations_all, STATION_NUMBER %in% values$station), 
                       lng = ~LONGITUDE, lat = ~LATITUDE, color = "red", radius = 6) %>%
      addCircles(lng = ~LONGITUDE, lat = ~LATITUDE, weight = 1,
                 radius = 1, label = ~STATION_NAME, 
                 popup = ~paste(STATION_NAME, "<br>",
                                STATION_NUMBER, "<br>",
                                "DRAINAGE AREA = ", DRAINAGE_AREA_GROSS, "SQ. KM","<br>")
      )
  })
  
  
  ###################################################################################
  ### Historical Data 
  ###################################################################################
  
  # Extract daily discharge and water level data from HYDAT (used for long-term data,and monthly and daily statistics)
  dailyDataHYDAT <- reactive({
    
    # extract flow or water level data depending on what is available
    if (metaData()[4,2] == "Flow and Level") { # both Q and H
      daily.flow.HYDAT <- hy_daily_flows(station_number = values$station)
      daily.levels.HYDAT <- hy_daily_levels(station_number = values$station)
      daily.data <- rbind(daily.flow.HYDAT[ ,c(2:5)], daily.levels.HYDAT[ ,c(2:5)])
    } else if (metaData()[4,2] == "Flow") { # just Q
      daily.flow.HYDAT <- hy_daily_flows(station_number = values$station)
      daily.data <- daily.flow.HYDAT[ ,c(2:5)]
    } else if (metaData()[4,2] == "Level") { # just H
      daily.levels.HYDAT <- hy_daily_levels(station_number = values$station)
      daily.data <- daily.levels.HYDAT[ ,c(2:5)]
    }
    daily.data
  })
  
  dailyData <- reactive({
    
    daily.data <- dailyDataHYDAT()
    
    # fill in any date data gaps (including completing the first and last years of data)
    daily.data.temp <- as.data.frame(daily.data[0,])
    for (parameter in unique(daily.data$Parameter)) {
      daily.data.param <- daily.data %>% filter(Parameter == parameter)
      min.date <- as.Date((paste((as.numeric(format(min(daily.data.param$Date), '%Y'))), 01, 01, sep = "-")), "%Y-%m-%d")
      max.date <- as.Date((paste((as.numeric(format(max(daily.data.param$Date), '%Y'))), 12, 31, sep = "-")), "%Y-%m-%d")
      data.empty <- data.frame(Date = seq(min.date, max.date, by = "days"))
      data.temp <- merge(data.empty, daily.data.param, by = "Date", all = TRUE)
      data.temp$Parameter <- parameter
      daily.data.temp <- as.data.frame(rbind(daily.data.temp, data.temp))
    }
    daily.data <- daily.data.temp
  })
  
  # Determine the min and max years of the dataset
  histDates <- reactive({
    daily.flow.dates <- dailyData() %>% 
      summarize(minDate = as.numeric(format(min(Date), '%Y')),
                maxDate = as.numeric(format(max(Date), '%Y')))
  })
  
  # create the slider widget to filter data
  output$histYears <- renderUI({
    sliderInput("histYears",
                label = "Filter data between the following years:",
                min = histDates()$minDate,
                max = histDates()$maxDate,
                value = c(histDates()$minDate, histDates()$maxDate),
                sep = "")
  })
  
  
  ### Historical Long-term Data
  ###################################################################################
  
  # Select the parameters to plot
  output$ltParam <- renderUI({
    selectizeInput("ltParam",
                   label = "Display parameters:",
                   choices = as.list(unique(dailyData()$Parameter)),
                   selected = as.list(unique(dailyData()$Parameter)), 
                   multiple =TRUE)
  })
  
  # Select the data symbols of the paramters to plot
  output$paramSymbol <- renderUI({
    selectizeInput("paramSymbol", 
                   label = "Add data symbols:",
                   choices = as.list(unique(dailyData()$Parameter)),
                   multiple =TRUE)
  })
  
  # PLot title
  output$ltplot.title <- renderText({
    paste0("Long-term Daily Data Record - ", metaData()[2,2], " (", metaData()[1,2], ")")
  })
  
  # Plot the discharge axis on log-scale if checked
  ltplot.y <- reactive({
    if (input$ltlog) {list(title = "Discharge (cms)", type= "log")}
    else {            list(title = "Discharge (cms)")}
  })
  
  # Create the long-term plot
  output$ltplot <- renderPlotly({
    
    plot.data <- dailyData() %>% 
      mutate(Symbol = replace(Symbol, Symbol == "E", "Estimate"),
             Symbol = replace(Symbol, Symbol == "A", "Partial Day"),
             Symbol = replace(Symbol, Symbol == "B", "Ice conditions"),
             Symbol = replace(Symbol, Symbol == "D", "Dry"),
             Symbol = replace(Symbol, Symbol == "R", "Revised"),
             Symbol = replace(Symbol, is.na(Symbol), ""),
             Value = round(Value,3),
             Year = as.numeric(format(Date, '%Y'))) %>% 
      filter(Year >= input$histYears[1] & Year <= input$histYears[2]) # filter for the years from the slider
    
    
    if (length(input$ltParam)==2) { # Plot if both flow and water level
      plot <-  plot_ly() %>%
        add_lines(data = plot.data %>% filter(Parameter == "Flow"), x = ~Date, y = ~Value, name = "Discharge", hoverinfo = 'text',
                  text = ~paste('Discharge: ', Value, "cms", Symbol, '\nDate: ', Date),
                  line = list(color = 'rgba(31, 119, 180, 1)')) %>%
        add_lines(data = plot.data %>% filter(Parameter == "Level"), x = ~Date, y = ~Value, name = "Water Level", hoverinfo = 'text',
                  text = ~paste('Water Level: ', Value, "m", Symbol, '\nDate: ', Date), 
                  line = list(color = 'rgba(255, 127, 14, 1)'), yaxis = "y2") %>%
        layout(xaxis = list(title = "Date"),
               yaxis = ltplot.y(),
               yaxis2 = list(overlaying = "y", side = "right", title = "Water Level (m)"))} 
    else if (length(input$ltParam) == 1 & input$ltParam == "Flow") { # Plot if just flow
      plot <- plot_ly() %>%
        add_lines(data = plot.data %>% filter(Parameter == "Flow"), x = ~Date, y = ~Value, name = "Discharge", hoverinfo = 'text',
                  text = ~paste('Discharge: ', Value, "cms", Symbol, '\nDate: ', Date),
                  line = list(color = 'rgba(31, 119, 180, 1)')) %>%
        layout(xaxis = list(title = "Date"),
               yaxis = ltplot.y(),
               showlegend = TRUE)}
    else if (length(input$ltParam) == 1 & input$ltParam == "Level") { # Plot if just water level
      plot <-plot_ly() %>%
        add_lines(data = plot.data %>% filter(Parameter == "Level"), x = ~Date, y = ~Value, name = "Water Level", hoverinfo = 'text',
                  text = ~paste('Water Level: ', Value, "m", Symbol, '\nDate: ', Date), 
                  line = list(color = 'rgba(255, 127, 14, 1)')) %>%
        layout(xaxis = list(title = "Date"),
               yaxis = list(title = "Water Level (m)"),
               showlegend = TRUE)}
    
    # Add data symbol to plot if selected
    if (length(input$paramSymbol)>0) {
      plot <- plot %>% add_markers(data = plot.data %>% filter(Parameter %in% input$paramSymbol), x = ~Date, y = 0, 
                                   color = ~Symbol)}
    
    plot
  })
  
  # Create and render the long-term table to view
  ltTableOutput <- reactive({
    data <- dailyData() %>% mutate(Value=round(Value,3),
                                   Year=as.integer(format(Date,'%Y')),
                                   Month=as.character(format(Date,'%B'))) %>%
      select(Date,Year,Month,Parameter,Value,Symbol)
    
    if (metaData()[4,2]=="Flow and Level") { # both Q and H
      data.flow <- data %>% filter(Parameter=="Flow") %>% 
        mutate("Flow (cms)"=Value,"Flow Symbol"=Symbol)%>%
        select(-Parameter,-Value,-Symbol)
      data.level <- data %>% filter(Parameter=="Level") %>% 
        mutate("Water Level (m)"=Value,"Water Level Symbol"=Symbol)%>%
        select(-Parameter,-Value,-Symbol)
      table.data <- merge(data.flow,data.level,by=c("Date","Year","Month"),all=TRUE) 
    } else if (metaData()[4,2]=="Flow") { # just Q
      table.data <- data %>% 
        rename("Flow (cms)"=Value,"Flow Symbol"=Symbol) %>%
        select(-Parameter)
    } else if (metaData()[4,2]=="Level") { # just H
      table.data <- data %>% 
        rename("Water Level (m)"=Value,"Water Level Symbol"=Symbol) %>%
        select(-Parameter)
    }
    table.data
  })
  output$ltTable <- DT::renderDataTable(
    ltTableOutput(),
    rownames=FALSE,
    filter = 'top',
    extensions = c("Scroller"),
    options = list(scrollX = TRUE,
                   scrollY=600,deferRender = TRUE,scroller = TRUE,
                   columnDefs = list(list(className = 'dt-center', targets = 0:2)))
  ) 
  
  dailyDataOutput <- reactive({
    
    daily.data <- dailyDataHYDAT()
    
    # fill in any date data gaps (including completing the first and last years of data)
    daily.data.temp <- as.data.frame(daily.data[0,])
    for (parameter in unique(daily.data$Parameter)) {
      daily.data.param <- daily.data %>% filter(Parameter==parameter)
      data.empty <- data.frame(Date=seq(min(daily.data.param$Date), max(daily.data.param$Date), by="days"))
      data.temp <- merge(data.empty,daily.data.param,by="Date",all = TRUE)
      data.temp$Parameter <- parameter
      daily.data.temp <- as.data.frame(rbind(daily.data.temp,data.temp))
    }
    daily.data <- daily.data.temp
  })
  
  #Download data button
  output$download.ltData <- downloadHandler(
    filename = function() {paste0(metaData()[1,2]," - daily discharge.csv")},
    content = function(file) {
      write.csv(dailyDataOutput() %>% mutate(Station_Number=values$station) %>% 
                  select(Station_Number,Date,Parameter,Value,Symbol),
                file, row.names = FALSE, na="")
    })
  
  
  
  ### Historical Annual Data
  ###################################################################################
  
  # Extract monthly discharge and water level data from HYDAT
  annualData <- reactive({
    
    annual <- hy_annual_stats(station_number=values$station) %>% 
      filter(Parameter=="Flow" | Parameter == "Water Level") %>% 
      mutate(Parameter=replace(Parameter, Parameter=="Flow", "Flow"),
             Parameter=replace(Parameter, Parameter=="Water Level", "Level"))
    
    # Fill in missing years with NA
    annual.data <- annual[0,]
    for (param in unique(annual$Parameter)) {
      annual.param <- annual %>% filter(Parameter==param)
      all.years <- as.data.frame(rep(seq(from=min(annual.param$Year),to=max(annual.param$Year)),3))
      colnames(all.years) <- "Year"
      all.years$Sum_stat <- c(rep("MEAN",max(annual.param$Year)-min(annual.param$Year)+1),
                              rep("MAX",max(annual.param$Year)-min(annual.param$Year)+1),
                              rep("MIN",max(annual.param$Year)-min(annual.param$Year)+1))
      all.years <- merge(annual.param,all.years,by=c("Year","Sum_stat"), all=TRUE)
      all.years$Parameter <- param
      all.years$STATION_NUMBER <- values$station
      all.years$Symbol[is.na(all.years$Symbol)] <- ""
      annual.data <- rbind(annual.data,all.years)
    }
    # Filter for years from the slider
    annual.data <- annual.data %>% filter(Year >= input$histYears[1] & Year <= input$histYears[2])
    
    annual.data 
  })
  
  
  # Extract monthly instanteous peaks of discharge and water level data from HYDAT
  annInstData <- reactive({
    # Extract and format columns for displaying information
    annual.instant <- hy_annual_instant_peaks(station_number=values$station)
    
    annual.instant <- annual.instant %>% 
      mutate(Date=as.Date(paste(YEAR,MONTH,DAY,sep="-"),format="%Y-%m-%d"),
             Time=paste0(HOUR,":",ifelse(nchar(MINUTE)>1,paste(MINUTE),paste0(0,MINUTE))," ",TIME_ZONE),
             DateTime=paste0(Date," at ",Time),
             Symbol=replace(Symbol, is.na(Symbol), ""),
             Parameter=replace(Parameter, Parameter=="Flow", "Flow"),
             Parameter=replace(Parameter, Parameter=="Water Level", "Level"),
             Value=round(Value,3)) %>% 
      filter(YEAR >= input$histYears[1] & YEAR <= input$histYears[2])
    annual.instant
  })
  
  # Select the paramter to display
  output$annualParam <- renderUI({
    selectInput("annualParam",
                label = "Display parameter:",
                choices = as.list(unique(annualData()$Parameter)))
  })
  
  # Select the annual statistics to display
  output$annualStat <- renderUI({
    selectizeInput("annualStat", 
                   label = "Display annual statistic(s):",
                   choices = c("Mean"="MEAN","Maximum"="MAX","Minimum"="MIN"),
                   selected = c("Mean"="MEAN","Maximum"="MAX","Minimum"="MIN"),
                   multiple =TRUE)
  })
  
  # Select the annual instantaneous peaks to display
  output$annInstStat <- renderUI({
    selectizeInput("annInstStat", 
                   label = "Display annual instantaneous peak values:",
                   choices = c("Minimum"="MIN","Maximum"="MAX"),
                   multiple =TRUE)
  })
  
  
  
  # Create annual plot title
  output$annualPlot.title <- renderText({
    paste0("Annual Summary Statistics - ",metaData()[2,2]," (",metaData()[1,2],")")
  })
  
  # Plot the primary y-axis on log-scale if checked
  annualPlot.y <- reactive({
    if (input$annuallog) {list(title=ifelse(input$annualParam== "Flow","Discharge (cms)","Water Level (m)"),type= "log")}
    else {                list(title=ifelse(input$annualParam== "Flow","Discharge (cms)","Water Level (m)"))}
  })
  
  # Create the annual plot
  output$annualPlot <- renderPlotly({
    plot.data <- annualData() %>% filter(Parameter==input$annualParam) %>% mutate(Value=round(Value,3))
    
    plot <- plot_ly() %>% 
      layout(xaxis=list(title="Year"),
             yaxis=annualPlot.y(),
             showlegend = TRUE)
    
    # Add each annual statistic if selected
    if ("MAX" %in% input$annualStat){
      plot <- plot %>%  add_trace(data=plot.data %>% filter(Sum_stat=="MAX"),
                                  x= ~Year,y=~Value,name="Daily Maximum",mode = 'lines+markers',
                                  hoverinfo= 'text',text=~paste("Maximum: ",Value,
                                                                "\nOn",Date," ",Symbol),
                                  line=list(color='rgba(1,102,94, 1)'))}
    if ("MEAN" %in% input$annualStat){
      plot <- plot %>%  add_trace(data=plot.data %>% filter(Sum_stat=="MEAN"),
                                  x= ~Year,y=~Value,name="Daily Mean", mode = 'lines+markers',
                                  hoverinfo= 'text',text=~paste("Mean: ",Value),
                                  line=list(color='rgba(61,151,53, 1)'))}
    if ("MIN" %in% input$annualStat){
      plot <- plot %>%  add_trace(data=plot.data %>% filter(Sum_stat=="MIN"),
                                  x= ~Year,y=~Value,name="Daily Minimum",
                                  mode = 'lines+markers',
                                  hoverinfo= 'text',text=~paste("Minimum: ",Value,
                                                                "\nOn",Date," ",Symbol),
                                  line=list(color='rgba(140,81,10, 1)'))}
    
    # Add instantaneous peaks if selected
    if ("MAX" %in% input$annInstStat){
      plot <- plot %>% add_trace(data=annInstData() %>% filter(Parameter==input$annualParam & PEAK_CODE == "MAX"),
                                 x= ~YEAR,y= ~Value, name="Instanteous Maximum",mode = 'markers',
                                 marker=list(symbol=2,size=8,color='rgba(10,69,140,1)'), 
                                 hoverinfo= 'text',text=~paste("Instanteous Maximum: ",Value,
                                                               "\nOn",DateTime," ",Symbol))} 
    if ("MIN" %in% input$annInstStat){
      plot <- plot %>% add_trace(data=annInstData() %>% filter(Parameter==input$annualParam & PEAK_CODE == "MIN"),
                                 x= ~YEAR,y= ~Value, name="Instanteous Minimum",mode = 'markers',
                                 marker=list(symbol=2,size=8,color='rgba(140,16,10,1)'), 
                                 hoverinfo= 'text',text=~paste("Instanteous Minimum: ",Value,
                                                               "\nOn",DateTime," ",Symbol))} 
    plot
  })
  
  
  #Create and render table output
  output$annualTable <- DT::renderDataTable(
    annualData() %>%  select(-STATION_NUMBER) %>%  mutate(Value=round(Value,3),Time=NA) %>% 
      select(Year,Parameter,Sum_stat,Value,Date,Time,Symbol) %>% 
      bind_rows(annInstData() %>% select(YEAR,Parameter,PEAK_CODE,Value,Date,Time,Symbol) %>% 
                  mutate(PEAK_CODE=replace(PEAK_CODE, PEAK_CODE=="MAX", "INST. MAX"),
                         PEAK_CODE=replace(PEAK_CODE, PEAK_CODE=="MIN", "INST. MIN"),
                         Parameter=replace(Parameter, Parameter=="Flow", "Flow"),
                         Parameter=replace(Parameter, Parameter=="Water Level", "Level"),
                         Value=round(Value,3)) %>% 
                  rename("Year"=YEAR,"Sum_stat"=PEAK_CODE)),
    rownames=FALSE,
    selection=list(mode="single"),
    filter = 'top',
    colnames = c('Year', 'Parameter', 'Summary Statistic','Value','On Date','Time','Symbol'),
    extensions = c("Scroller","ColReorder","Buttons"),
    options = list(scrollX = TRUE,
                   scrollY=450,deferRender = TRUE,scroller = TRUE,
                   dom = 'Bfrtip', 
                   colReorder = TRUE,
                   buttons= list(list(extend='colvis',columns=c(0:6))),
                   columnDefs = list(list(className = 'dt-center', targets = 0:4)))
    
  )
  
  
  #Download data buttons
  output$download.annualData <- downloadHandler(
    filename = function() {paste0(metaData()[1,2]," - annual summary.csv")},
    content = function(file) {
      write.csv(annualData() %>% select(Station_Number=STATION_NUMBER,Year,Summary_Stat=Sum_stat,
                                        Parameter,Value,Date,Symbol),
                file, row.names = FALSE, na="")
    })  
  output$download.annualPeakData <- downloadHandler(
    filename = function() {paste0(metaData()[1,2]," - annual instantaneous peaks.csv")},
    content = function(file) {
      write.csv(annInstData() %>% select(Station_Number=STATION_NUMBER,Year=YEAR,Peak_Code=PEAK_CODE,
                                         Precision_Code=PRECISION_CODE,Month=MONTH,Day=DAY,Hour=HOUR,
                                         Minute=MINUTE,Time_Zone=TIME_ZONE,Parameter,Value,Symbol),
                file, row.names = FALSE, na="")
    })  
  
  
  
  ### Historical Monthly Data
  ###################################################################################
  
  # Calculate monthly data and render for printing
  # Possible to use data from HYDAT MONTHLY_FLOWS, and MONTHLY_LEVELS, but only MAX, MIN, MEAN provided
  # Values calculated here match with HYDAT values, where provided
  monthData <- reactive({
    daily.data <- dailyData()
    daily.data$Year <- as.numeric(format(daily.data$Date,'%Y'))
    daily.data$Month <- as.integer(format(daily.data$Date,'%m'))
    
    # FIlter data for select years
    daily.data <- daily.data %>% 
      filter(Year >= input$histYears[1] & Year <= input$histYears[2])
    
    # Summarize each month
    month.data <- daily.data %>% 
      group_by(Parameter,Month) %>% 
      summarize(Mean=round(mean(Value, na.rm=TRUE),3),
                Maximum=round(max(Value, na.rm=TRUE),3),
                MaxYear=Year[which.max(Value)],
                Minimum=round(min(Value, na.rm=TRUE),3),
                MinYear=Year[which.min(Value)],
                Median=round(median(Value, na.rm=TRUE),3),
                Percentile75=round(quantile(Value,.75, na.rm=TRUE),3),
                Percentile25=round(quantile(Value,.25,na.rm=TRUE),3),
                Percentile95=round(quantile(Value,.95,na.rm=TRUE),3),
                Percentile5=round(quantile(Value,.05,na.rm=TRUE),3)) %>% 
      gather(Stat,Value,3:12)
    
    month.data
  })
  
  # Calculate annual data and render for printing
  allmonthData <- reactive({
    
    daily.data <- dailyData()
    daily.data$Year <- as.numeric(format(daily.data$Date,'%Y'))
    daily.data$Month <- as.integer(format(daily.data$Date,'%m'))
    
    # FIlter data for select years
    daily.data <- daily.data %>% 
      filter(Year >= input$histYears[1] & Year <= input$histYears[2])
    
    # Summarize each month
    month.data <- daily.data %>% 
      group_by(Parameter,Year,Month) %>% 
      summarize(Mean=round(mean(Value, na.rm=TRUE),3),
                Maximum=round(max(Value, na.rm=TRUE),3),
                Minimum=round(min(Value, na.rm=TRUE),3),
                Median=round(median(Value, na.rm=TRUE),3),
                Percentile75=round(quantile(Value,.75, na.rm=TRUE),3),
                Percentile25=round(quantile(Value,.25,na.rm=TRUE),3),
                Percentile95=round(quantile(Value,.95,na.rm=TRUE),3),
                Percentile5=round(quantile(Value,.05,na.rm=TRUE),3)) %>% 
      gather(Sum_stat,Value,4:11)
    
    month.data
    
  })
  
  # Extract the HYDAT data for table and download button
  HYDATmonthData <- reactive({
    
    if (metaData()[4,2] == "Flow and Level") { # both Q and H
      monthly.flows.hydat <- hy_monthly_flows(station_number = values$station) %>%
        mutate(Parameter = "Flow")
      monthly.levels.hydat <- hy_monthly_levels(station_number = values$station) %>%
        select(-PRECISION_CODE) %>% mutate(Parameter = "Level")
      monthly.data <- rbind(monthly.flows.hydat,monthly.levels.hydat) %>%
        select(Parameter, YEAR, MONTH, Sum_stat, Value, Date_occurred)
    } else if (metaData()[4,2]=="Flow") { # just Q
      monthly.flows.hydat <- hy_monthly_flows(station_number=values$station) %>%
        mutate(Parameter="Flow")
      monthly.data <- monthly.flows.hydat %>%
        select(Parameter,YEAR,MONTH,Sum_stat,Value,Date_occurred)
    } else if (metaData()[4,2]=="Level") { # just H
      monthly.levels.hydat <- hy_monthly_levels(station_number=values$station) %>%
        select(-PRECISION_CODE) %>% mutate(Parameter="Level")
      monthly.data <- monthly.levels.hydat %>%
        select(Parameter,YEAR,MONTH,Sum_stat,Value,Date_occurred)
    }
    
    monthly.data <- as.data.frame(monthly.data)
    monthly.data
  })
  
  
  # Select the paramter to display
  output$monthParam <- renderUI({
    selectInput("monthParam", 
                label = "Display parameter:",
                choices = as.list(unique(monthData()$Parameter)))
  })
  
  # Select the monthly summary statiscs to display
  output$monthStat <- renderUI({
    selectizeInput("monthStat", 
                   label = "Select monthly statistic(s):",
                   choices =   c("Mean"="Mean","Maximum"="Maximum","Minimum"="Minimum","Median"="Median",
                                 "75th Percentile"="Percentile75","25th Percentile"="Percentile25", 
                                 "95th Percentile"="Percentile95", "5th Percentile"="Percentile5"),
                   multiple =TRUE,
                   selected = c("Mean","Median"))
  })
  
  #Select a year to display on the plot (with stat chosen in monthYearStat widget)
  output$monthYear <- renderUI({
    selectizeInput("monthYear", 
                   label = "Select year(s) to display:",
                   choices = as.list(unique(allmonthData()[allmonthData()$Parameter == input$monthParam,]$Year)),
                   multiple =TRUE)
  })
  
  # Select stat(s) of specific year (s) to display
  output$monthYearStat <- renderUI({
    selectizeInput("monthYearStat", 
                   label = "Select monthly statistic(s):",
                   choices = c("Mean"="Mean","Maximum"="Maximum","Minimum"="Minimum","Median"="Median",
                               "75th Percentile"="75th Percentile", "25th Percentile"="25th Percentile", 
                               "95th Percentile"="95th Percentile", "5th Percentile"="5th Percentile"),
                   multiple =TRUE)
  })
  
  #Render plot title
  output$monthPlot.title <- renderText({
    paste0("Monthly Summary Statistics - ",metaData()[2,2]," (",metaData()[1,2],")")
  })
  
  # Plot the primary-Yaxis on log-scale if checked
  monthlyPlot.y <- reactive({
    if (input$monthlog) {list(title=ifelse(input$monthParam== "Flow","Discharge (cms)","Water Level (m)"),type= "log")}
    else {               list(title=ifelse(input$monthParam== "Flow","Discharge (cms)","Water Level (m)"))}
  })
  
  #Create and render the monthly plot
  output$monthPlot <- renderPlotly({
    
    plot.data <- monthData() %>% spread(Stat,Value) %>% filter(Parameter==input$monthParam) %>% 
      mutate(MonthText=month.abb[Month])
    
    #Create the plot
    plot <- plot_ly() %>% 
      layout(xaxis=list(title="Month",tickvals = seq(1:12),ticktext = c("Jan","Feb","Mar","Apr","May","Jun","Jul",
                                                                        "Aug","Sep","Oct","Noc","Dec")),
             yaxis=monthlyPlot.y(),
             showlegend = TRUE)
    
    # Add ribbons if checked
    if (input$monthMaxMin){
      plot <- plot %>%  add_ribbons(data=plot.data,x= ~Month,ymin= ~Minimum, ymax= ~Maximum,name="Max-Min Range",
                                    color=I("lightblue1"),hoverinfo= 'text',
                                    text=~paste0('Maximum: ',Maximum,' (',MaxYear,')',
                                                 '\nMinimum: ',Minimum,' (',MinYear,')',
                                                 '\n',MonthText))}
    if (input$month90){
      plot <- plot %>%  add_ribbons(data=plot.data,x= ~Month,ymin= ~Percentile5, ymax= ~Percentile95,name="90% of Flows",
                                    color=I("lightblue2"),hoverinfo= 'text',
                                    text=~paste0('95th Percentile: ',Percentile95,
                                                 '\n5th Percentile: ',Percentile5,
                                                 '\n',MonthText))}
    if (input$month50){
      plot <- plot %>%  add_ribbons(data=plot.data,x= ~Month,ymin= ~Percentile25, ymax= ~Percentile75,name="50% of Flows",
                                    color=I("lightblue3"),hoverinfo= 'text',
                                    text=~paste0('75th Percentile: ',Percentile75,
                                                 '\n25th Percentile: ',Percentile25,
                                                 '\n',MonthText))}
    # Add lines if selected
    if ("Maximum" %in% input$monthStat){
      plot <- plot %>%  add_trace(data=plot.data,x= ~Month,y=~Maximum,name="Maximum",
                                  mode = 'lines+markers',line=list(dash = 'dot',color='rgba(1,102,94, 1)'),
                                  hoverinfo= 'text',text=~paste0('Maximum: ',Maximum,' (',MinYear,')',
                                                                 '\n',MonthText))}
    if ("Percentile95" %in% input$monthStat){
      plot <- plot %>%  add_trace(data=plot.data,x= ~Month,y=~Percentile95,name="95th Percentile",
                                  mode = 'lines+markers',line=list(dash = 'dot',color='rgba(53,151,143, 1)'),
                                  hoverinfo= 'text',text=~paste0('95th Percentile: ',Percentile95,
                                                                 '\n',MonthText))}
    if ("Percentile75" %in% input$monthStat){
      plot <- plot %>%  add_trace(data=plot.data,x= ~Month,y=~Percentile75,name="75th Percentile",
                                  mode = 'lines+markers',line=list(dash = 'dot',color='rgba(128,205,193, 1)'),
                                  hoverinfo= 'text',text=~paste0('75th Percentile: ',Percentile75,
                                                                 '\n',MonthText))}
    if ("Mean" %in% input$monthStat){
      plot <- plot %>%  add_trace(data=plot.data,x= ~Month,y=~Mean,name="Mean",
                                  mode = 'lines+markers',line=list(color='rgba(61,151,53, 1)'),
                                  hoverinfo= 'text',text=~paste0('Mean: ',Mean,
                                                                 '\n',MonthText))}
    if ("Median" %in% input$monthStat){
      plot <- plot %>%  add_trace(data=plot.data,x= ~Month,y=~Median,name="Median",
                                  mode = 'lines+markers',line=list(color='rgba(143,53,151, 1)'),
                                  hoverinfo= 'text',text=~paste0('Median: ',Median,
                                                                 '\n',MonthText))}
    if ("Percentile25" %in% input$monthStat){
      plot <- plot %>%  add_trace(data=plot.data,x= ~Month,y=~Percentile25,name="25th Percentile",
                                  mode = 'lines+markers',line=list(dash = 'dot',color='rgba(223,194,125, 1)'),
                                  hoverinfo= 'text',text=~paste0('25th Percentile: ',Percentile25,
                                                                 '\n',MonthText))}
    if ("Percentile5" %in% input$monthStat){
      plot <- plot %>%  add_trace(data=plot.data,x= ~Month,y=~Percentile5,name="5th Percentile",
                                  mode = 'lines+markers',line=list(dash = 'dot',color='rgba(191,129,45, 1)'),
                                  hoverinfo= 'text',text=~paste0('5th Percentile: ',Percentile5,
                                                                 '\n',MonthText))}
    if ("Minimum" %in% input$monthStat){
      plot <- plot %>%  add_trace(data=plot.data,x= ~Month,y=~Minimum,name="Minimum",
                                  mode = 'lines+markers',line=list(dash = 'dot',color='rgba(140,81,10, 1)'),
                                  hoverinfo= 'text',text=~paste0('Minimum: ',Minimum,' (',MinYear,')',
                                                                 '\n',MonthText))}
    
    
    # Add data from specific years and specific stats    
    year.data <- allmonthData() %>% mutate(Sum_stat=replace(Sum_stat, Sum_stat=="Percentile25", "25th Percentile"),
                                           Sum_stat=replace(Sum_stat, Sum_stat=="Percentile75", "75th Percentile"),
                                           Sum_stat=replace(Sum_stat, Sum_stat=="Percentile5", "5th Percentile"),
                                           Sum_stat=replace(Sum_stat, Sum_stat=="Percentile95", "95th Percentile"))
    plot <- plot %>% add_trace(data=year.data %>% filter(Parameter==input$monthParam & 
                                                           Sum_stat %in% input$monthYearStat & 
                                                           Year %in% input$monthYear),
                               x= ~Month,y= ~Value, color=~paste0(Year," ",Sum_stat),
                               mode = 'lines+markers',line=list(width=3),
                               hoverinfo= 'text',text=~paste0(Year," ",Sum_stat,": ",Value))
    
    plot
  })
  
  #Create and render the monthly plot
  output$monthPlot2 <- renderPlotly({
    
    daily.data <- dailyData() %>% filter(Parameter==input$monthParam) %>% mutate(Month=as.integer(format(Date,'%m')))
    month.data <- monthData() %>% spread(Stat,Value) %>% filter(Parameter==input$monthParam)
    
    #Create the plot
    plot <- plot_ly() %>% 
      add_trace(data=daily.data, x=~Month, y=~Value,type="box", name="Monthly Quartiles and Outliers") %>% 
      add_trace(data=month.data, x=~Month, y=~Mean,type='scatter',marker=list(symbol=4,size=10), name="Monthly Mean") %>% 
      layout(xaxis=list(title="Month",tickvals = seq(1:12),ticktext = c("Jan","Feb","Mar","Apr","May","Jun","Jul",
                                                                        "Aug","Sep","Oct","Noc","Dec")),
             yaxis=monthlyPlot.y(),
             showlegend = TRUE)
    
    plot
  })
  
  monthTableOutput <- reactive({
    table <- monthData() %>% mutate(Value=round(Value,3),
                                    Stat=replace(Stat, Stat=="Percentile75", "75th Percentile"),
                                    Stat=replace(Stat, Stat=="Percentile95", "95th Percentile"),
                                    Stat=replace(Stat, Stat=="Percentile5", "5th Percentile"),
                                    Stat=replace(Stat, Stat=="Percentile25", "25th Percentile")) %>% 
      spread(Month,Value) %>% 
      rename("Summary Statistic"=Stat,'Jan'="1",'Feb'="2",'Mar'="3",'Apr'="4",'May'="5",
             'Jun'="6",'Jul'="7",'Aug'="8",'Sep'="9",'Oct'="10",'Nov'="11",'Dec'="12")
    
    if (input$monthTableSwitch){
      table <- table %>% 
        gather(Month,Value,3:14) %>% 
        spread("Summary Statistic",Value) %>% 
        mutate(Month=as.factor(Month))
      levels(table$Month) <- c('Jan','Feb','Mar','Apr','May','Jun','Jul','Aug','Sep','Oct','Nov','Dec')
    }
    
    table
    
  })
  
  #Create and render table output
  output$monthTable <- DT::renderDataTable(
    monthTableOutput(),
    rownames=FALSE,
    selection=list(mode="single"),
    filter = 'top',
    extensions = c("Scroller"),
    options = list(scrollX = TRUE,
                   scrollY=450,deferRender = TRUE,scroller = TRUE,
                   columnDefs = list(list(className = 'dt-center', targets = "_all"))
    )
  )
  
  #Create and render table output
  output$monthHYDATTable <- DT::renderDataTable(
    HYDATmonthData() %>% rename(Year=YEAR,Month=MONTH,"Summary Statistic"=Sum_stat,"On Date"=Date_occurred) %>% 
      mutate(Month=month.abb[Month],
             Value=round(Value,3),
             Year=as.numeric(Year)),
    rownames=FALSE,
    selection=list(mode="single"),
    filter = 'top',
    extensions = c("Scroller","ColReorder","Buttons"),
    options = list(scrollX = TRUE,
                   scrollY=450,deferRender = TRUE,scroller = TRUE,
                   columnDefs = list(list(className = 'dt-center', targets = "_all")),
                   dom = 'Bfrtip', 
                   colReorder = TRUE,
                   buttons= list(list(extend='colvis',columns=c(0:5)))
    )
  )
  
  
  #Download data buttons
  output$download.monthData <- downloadHandler(
    filename = function() {paste0(metaData()[1,2]," - monthly summary.csv")},
    content = function(file) {
      write.csv(monthData() %>% mutate(Station_Number=values$station) %>% 
                  select(Station_Number,Parameter,Statistic=Stat,Value) %>% 
                  mutate(Statistic=replace(Statistic, Statistic=="Percentile5","5th Percentile"),
                         Statistic=replace(Statistic, Statistic=="Percentile25","25th Percentile"),
                         Statistic=replace(Statistic, Statistic=="Percentile75","75th Percentile"),
                         Statistic=replace(Statistic, Statistic=="Percentile95","95th Percentile")),
                file, row.names = FALSE, na="")
    }) 
  output$download.HYDATmonthData <- downloadHandler(
    filename = function() {paste0(metaData()[1,2]," - monthly summary HYDAT.csv")},
    content = function(file) {
      write.csv(HYDATmonthData() %>% mutate(Station_Number=values$station),file, row.names = FALSE, na="")
    })
  
  ### Historical Daily Summary Data
  ###################################################################################
  
  # Calculate daily data summaries and render for printing
  dailySummaryData <- reactive({
    daily.data <- dailyData()
    daily.data$Year <- as.numeric(format(daily.data$Date,'%Y'))
    daily.data$Day <- as.integer(format(daily.data$Date,'%j'))
    
    daily.data <- daily.data %>%filter(Year >= input$histYears[1] & Year <= input$histYears[2])
    
    #Calculate summary stats
    dailysummary <- daily.data %>% 
      group_by(Parameter,Day) %>% 
      filter(Day<366) %>% 
      summarize(Mean=round(mean(Value, na.rm=TRUE),3),
                Maximum=round(max(Value, na.rm=TRUE),3),
                MaxYear=Year[which.max(Value)],
                Minimum=round(min(Value, na.rm=TRUE),3),
                MinYear=Year[which.min(Value)],
                Median=round(median(Value, na.rm=TRUE),3),
                Percentile75=round(quantile(Value,.75, na.rm=TRUE),3),
                Percentile25=round(quantile(Value,.25,na.rm=TRUE),3),
                Percentile95=round(quantile(Value,.95,na.rm=TRUE),3),
                Percentile5=round(quantile(Value,.05,na.rm=TRUE),3))%>% 
      mutate(Date=as.Date(Day,origin = "1899-12-31"),
             MonthDay=as.character(format(Date,format="%b-%d"))) %>% #format date so all in one year for plotting
      gather(Stat,Value,3:12)
    dailysummary
  })
  
  # Select the paramter to plot
  output$dailyParam <- renderUI({
    selectInput("dailyParam", 
                label = "Display parameter:",
                choices = as.list(unique(dailySummaryData()$Parameter)))
  })
  
  # Get a list of years that have full daily datasets
  daily.yearslist <- reactive({
    data <- dailyData() %>% mutate(Year=as.factor(format(Date,'%Y'))) %>% 
      group_by(Parameter,Year) %>% summarise(n=sum(is.na(Value))) %>% 
      filter(n<365)
  }) 
  
  # Widget to select years of data to plot
  output$dailyYear <- renderUI({
    selectizeInput("dailyYear", 
                   label = "Select year(s) to display:",
                   choices = as.list(unique(daily.yearslist()[daily.yearslist()$Parameter == input$dailyParam,]$Year)),
                   multiple =TRUE)
  })
  
  # Select selected year(s) and its data to plot
  dailyYears <- reactive({
    daily.data <- dailyData()
    daily.data$Year <- as.factor(format(daily.data$Date,'%Y'))
    daily.data$Day <- as.integer(format(daily.data$Date,'%j'))
    
    daily.Year <- daily.data %>% 
      filter(Year %in% input$dailyYear & Day<366) %>% 
      mutate(Date=as.Date(Day,origin = "1899-12-31"))
    daily.Year
  })
  
  # Select the statistics to display on the plot
  output$dailyStat <- renderUI({
    selectizeInput("dailyStat", 
                   label = "Select daily statistic(s):",
                   choices = c("Mean"="Mean","Maximum"="Maximum","Minimum"="Minimum","Median"="Median",
                               "75th Percentile"="Percentile75", "25th Percentile"="Percentile25", 
                               "95th Percentile"="Percentile95", "5th Percentile"="Percentile5"),
                   multiple =TRUE,
                   selected = c("Mean","Median"))
  })
  
  # Render the title
  output$dailyPlot.title <- renderText({
    paste0("Daily Summary Statistics - ",metaData()[2,2]," (",metaData()[1,2],")")
  })
  
  # Plot the primary-Yaxis on log-scale if checked
  dailyPlot.y <- reactive({
    if (input$dailylog) {list(title=ifelse(input$dailyParam== "Flow","Discharge (cms)","Water Level (m)"),type= "log")} 
    else {               list(title=ifelse(input$dailyParam== "Flow","Discharge (cms)","Water Level (m)"))}
  })
  
  #Create and render the daily summary plot
  output$dailyPlot <- renderPlotly({
    
    plot.data <- dailySummaryData() %>% spread(Stat,Value)%>% filter(Parameter==input$dailyParam)
    
    #create the plot
    plot <- plot_ly() %>% 
      layout(xaxis=list(title="Day of Year",tickformat= "%b-%d"),
             yaxis=dailyPlot.y(),
             showlegend = TRUE)
    
    
    # Add ribbons if checked
    if (input$dailyMaxMin){
      plot <- plot %>%  add_ribbons(data=plot.data,x= ~Date,ymin= ~Minimum, ymax= ~Maximum,name="Max-Min Range",
                                    color=I("lightblue1"),
                                    hoverinfo= 'text',text=~paste0('Maximum: ',Maximum,' (',MaxYear,')',
                                                                   '\nMinimum: ',Minimum,' (',MinYear,')',
                                                                   '\n',MonthDay))}
    if (input$daily90){
      plot <- plot %>%  add_ribbons(data=plot.data,x= ~Date,ymin= ~Percentile95, ymax= ~Percentile5,name="90% of Flows",
                                    color=I("lightblue2"),
                                    hoverinfo= 'text',text=~paste0('95th Percentile: ',Percentile95,
                                                                   '\n5th Percentile: ',Percentile5,
                                                                   '\n',MonthDay))}
    if (input$daily50){
      plot <- plot %>%  add_ribbons(data=plot.data,x= ~Date,ymin= ~Percentile25, ymax= ~Percentile75,name="50% of Flows",
                                    color=I("lightblue3"),
                                    hoverinfo= 'text',text=~paste0('75th Percentile: ',Percentile75,
                                                                   '\n25th Percentile: ',Percentile25,
                                                                   '\n',MonthDay))}
    
    # Add lines if selected
    if ("Maximum" %in% input$dailyStat){
      plot <- plot %>%  add_lines(data=plot.data,x= ~Date,y=~Maximum,name="Maximum",
                                  line=list(color='rgba(1,102,94, 1)',width=2),
                                  hoverinfo= 'text',text=~paste0('Maximum: ',Maximum,' (',MaxYear,')',
                                                                 '\n',MonthDay))}
    if ("Percentile95" %in% input$dailyStat){
      plot <- plot %>%  add_lines(data=plot.data,x= ~Date,y=~Percentile95,name="95th Percentile",
                                  line=list(color='rgba(53,151,143, 1)',width=2),
                                  hoverinfo= 'text',text=~paste0('95th Percentile: ',Percentile95,
                                                                 '\n',MonthDay))}
    if ("Percentile75" %in% input$dailyStat){
      plot <- plot %>%  add_lines(data=plot.data,x= ~Date,y=~Percentile75,name="75th Percentile",
                                  line=list(color='rgba(128,205,193, 1)',width=2),
                                  hoverinfo= 'text',text=~paste0('75th Percentile: ',Percentile75,
                                                                 '\n',MonthDay))}
    if ("Mean" %in% input$dailyStat){
      plot <- plot %>%  add_lines(data=plot.data,x= ~Date,y=~Mean,name="Mean",
                                  line=list(color='rgba(61,151,53, 1)',width=2),
                                  hoverinfo= 'text',text=~paste0('Mean: ',Mean,
                                                                 '\n',MonthDay))}
    if ("Median" %in% input$dailyStat){
      plot <- plot %>%  add_lines(data=plot.data,x= ~Date,y=~Median,name="Median",
                                  line=list(color='rgba(143,53,151, 1)',width=2),
                                  hoverinfo= 'text',text=~paste0('Median: ',Median,
                                                                 '\n',MonthDay))}
    if ("Percentile25" %in% input$dailyStat){
      plot <- plot %>%  add_lines(data=plot.data,x= ~Date,y=~Percentile25,name="25th Percentile",
                                  line=list(color='rgba(223,194,125, 1)',width=2),
                                  hoverinfo= 'text',text=~paste0('25th Percentile: ',Percentile25,
                                                                 '\n',MonthDay))}
    if ("Percentile5" %in% input$dailyStat){
      plot <- plot %>%  add_lines(data=plot.data,x= ~Date,y=~Percentile5,name="5th Percentile",
                                  line=list(color='rgba(191,129,45, 1)',width=2),
                                  hoverinfo= 'text',text=~paste0('5th Percentile: ',Percentile5,
                                                                 '\n',MonthDay))}
    if ("Minimum" %in% input$dailyStat){
      plot <- plot %>%  add_lines(data=plot.data,x= ~Date,y=~Minimum,name="Minimum",
                                  line=list(color='rgba(140,81,10, 1)',width=2),
                                  hoverinfo= 'text',text=~paste0('Minimum: ',Minimum,' (',MinYear,')',
                                                                 '\n',MonthDay))}
    
    # Add data from specific years
    plot <- plot %>% add_lines(data=dailyYears() %>% filter(Parameter==input$dailyParam),
                               x= ~Date,y= ~Value, color=~Year,
                               hoverinfo= 'text',text=~paste0(Year,": ",round(Value,3),
                                                              '\n',as.character(format(Date,format="%b-%d"))))
    
    plot
    
  })
  
  
  #Create and render table output
  output$dailyTable <- DT::renderDataTable(
    dailySummaryData() %>% 
      mutate(Month=format(Date, format="%b"),
             Date=format(Date, format="%b-%d"),
             Value=round(Value,3),
             Stat=replace(Stat, Stat=="Percentile75", "75th Percentile"),
             Stat=replace(Stat, Stat=="Percentile95", "95th Percentile"),
             Stat=replace(Stat, Stat=="Percentile5", "5th Percentile"),
             Stat=replace(Stat, Stat=="Percentile25", "25th Percentile")) %>% 
      rename("Day of Year"=Day,"Summary Statistic"=Stat) %>% 
      spread("Summary Statistic",Value) %>% 
      select(Parameter,Month,"Day of Year",Date,"Minimum", "5th Percentile", "25th Percentile","Median","Mean",
             "75th Percentile","95th Percentile","Maximum"),
    rownames=FALSE,
    selection=list(mode="single"),
    filter = 'top',
    extensions = c("Scroller"),
    options = list(scrollX = TRUE,
                   scrollY=450,deferRender = TRUE,scroller = TRUE,
                   columnDefs = list(list(className = 'dt-center', targets = "_all")),
                   dom = 'Bfrtip', 
                   colReorder = TRUE,
                   buttons= list(list(extend='colvis',columns=c(0:11)))
    )
  )
  
  #Download data buttons
  output$download.dailySummaryData <- downloadHandler(
    filename = function() {paste0(metaData()[1,2]," - daily summary.csv")},
    content = function(file) {
      write.csv(dailySummaryData() %>% mutate(Station_Number=values$station),file, row.names = FALSE, na="")
    })  
  
  
  ###################################################################################
  ### Real-Time Data ###
  ###################################################################################
  
  #Check if real-time data can be downloaded (some stations with realtime dont have data to download)
  
  rt_available <- reactive({
    withProgress(message = "Checking for real-time data", {
      realtime.HYDAT <- realtime_dd(station_number = values$station)
      
      if(all(is.na(realtime.HYDAT$Value))){
        exist <- "No"
      } else {
        exist <- "Yes"
      }
      exist
    }
    )
  })
  
  output$rtExists <- reactive({
    rt_available()[1]
  })
  
  # Place note in real-time panel section if no real-time data
  output$noRT <- renderText({
    is_rt <- as.logical(dplyr::pull(metaData()[9,2]))
    if(!is_rt) {
      paste("*** Sorry, no real-time data is available for this station.")
    } else if(is_rt & rt_available() == "No") {
      paste("*** Sorry, no real-time is data available at this time.")
    }
  })
  
  # Extract real-time data from weblink and clip to dates
  realtimeData <- reactive({ 
    realtime.HYDAT <- realtime_dd(station_number = values$station)#values$station
    
    # Remove FLOW OR LEVEL if there either are all NA
    realtime.FLOW <- realtime.HYDAT %>% filter(Parameter=="Flow")
    if ((realtime.FLOW %>% summarise(n=sum(!is.na(Value))))==0){
      realtime.FLOW <- realtime.FLOW[0,]
    }
    realtime.LEVEL <- realtime.HYDAT %>% filter(Parameter=="Level")
    if ((realtime.LEVEL %>% summarise(n=sum(!is.na(Value))))==0){
      realtime.LEVEL <- realtime.LEVEL[0,]
    }
    
    realtime.data <- rbind(realtime.FLOW,realtime.LEVEL) %>% 
      mutate(DateTime=as.POSIXct(Date),
             Date=format(Date, format="%Y-%m-%d"),
             Time=strftime(DateTime, format="%H:%M:%S",usetz=TRUE))
    attributes(realtime.data$DateTime)$tzone <- Sys.timezone()
    realtime.data
    
  })
  
  # Choose the paramter to plot
  output$rtParam <- renderUI({
    selectizeInput("rtParam",
                   label="Display parameters:",
                   choices=as.list(unique(realtimeData()$Parameter)),
                   selected=as.list(unique(realtimeData()$Parameter)), 
                   multiple =TRUE)
  })
  
  # Plot the Discharge on log-scale if checked
  rtplot.y <- reactive({
    if (input$rtlog) {
      list(
        title= "Discharge (cms)",
        type= "log"
      )
    } else {
      list(
        title= "Discharge (cms)"
      )
    }
  })
  
  # Render the title for the plot
  output$rtplot.title <- renderText({
    paste0("Real-time Data - ",metaData()[2,2]," (",metaData()[1,2],")")
  })
  
  
  
  ## Create histric daily summaries to compare real-time data to (can't use
  ## daily summaries for hsitoric data as that needs to be filtered by years)
  rtHistoricData <- reactive({
    
    daily.data <- dailyData()
    daily.data$Day <- as.integer(format(daily.data$Date,'%j'))
    
    #Calculate summary stats
    dlydata <- daily.data %>% 
      group_by(Parameter,Day) %>% 
      filter(Day<366) %>% 
      summarize(Mean=round(mean(Value, na.rm=TRUE),3),
                Maximum=round(max(Value, na.rm=TRUE),3),
                Minimum=round(min(Value, na.rm=TRUE),3),
                Median=round(median(Value, na.rm=TRUE),3),
                Percentile75=round(quantile(Value,.75, na.rm=TRUE),3),
                Percentile25=round(quantile(Value,.25,na.rm=TRUE),3),
                Percentile95=round(quantile(Value,.95,na.rm=TRUE),3),
                Percentile5=round(quantile(Value,.05,na.rm=TRUE),3))
    
    rtdata <- realtimeData() %>% mutate(Day=as.integer(format(DateTime, format="%j")),
                                        MonthDay=as.character(format(DateTime,format="%b-%d")))
    data <- merge(rtdata,dlydata, by=c("Parameter","Day"),all.x = TRUE)
    data
  })
  
  # Create widget to choose parameter historical daily stats
  output$rtHistParam <- renderUI({
    selectInput("rtHistParam",
                "Select historic data paramater to display",
                choices =as.list(input$rtParam))
  })
  
  #Create and render the real time plot
  output$rtplot <- renderPlotly({
    
    plot <- plot_ly()
    
    ## Add historical daily data layers behind real-time data if select
    if (input$rtHistoric & (length(input$rtParam)==1 | input$rtHistParam=="Flow")){
      if ("Max-Min Range" %in% input$rtHistStats){
        plot <- plot %>%  add_ribbons(data=rtHistoricData() %>% filter(Parameter==input$rtHistParam),
                                      x= ~DateTime,ymin= ~Minimum, ymax= ~Maximum,name="Historic Max-Min Range",
                                      hoverinfo= 'text',text=~paste0('Maximum: ',Maximum,'\nMinimum: ',Minimum,
                                                                     '\n',MonthDay), 
                                      color=I("lightblue1"))}
      if ("5-95 Percentile Range" %in% input$rtHistStats){
        plot <- plot %>%  add_ribbons(data=rtHistoricData() %>% filter(Parameter==input$rtHistParam),
                                      x= ~DateTime,ymin= ~Percentile5, ymax= ~Percentile95,name="Historic 5-95 Percentile Range",
                                      hoverinfo= 'text',text=~paste0('95th Percentile: ',Percentile95,'\n5th Percentile: ',Percentile5,
                                                                     '\n',MonthDay),
                                      color=I("lightblue2"))}
      if ("25-75 Percentile Range" %in% input$rtHistStats){
        plot <- plot %>%  add_ribbons(data=rtHistoricData() %>% filter(Parameter==input$rtHistParam),
                                      x= ~DateTime,ymin= ~Percentile25, ymax= ~Percentile75,name="Historic 25-75 Percentile Range",
                                      hoverinfo= 'text',text=~paste0('75th Percentile: ',Percentile75,'\n25th Percentile: ',Percentile25,
                                                                     '\n',MonthDay),
                                      color=I("lightblue3"))}
      if ("Mean" %in% input$rtHistStats){
        plot <- plot %>%  add_lines(data=rtHistoricData() %>% filter(Parameter==input$rtHistParam),
                                    x= ~DateTime,y=~Mean,name="Historic Daily Mean",
                                    hoverinfo= 'text',text=~paste0('Mean: ',Mean,'\n',MonthDay),
                                    line=list(color='rgba(61,151,53, 1)'))}
      if ("Median" %in% input$rtHistStats){
        plot <- plot %>%  add_lines(data=rtHistoricData() %>% filter(Parameter==input$rtHistParam),
                                    x= ~DateTime,y=~Median,name="Historic Daily Median",
                                    hoverinfo= 'text',text=~paste0('Median: ',Median,'\n',MonthDay),
                                    line=list(color='rgba(143,53,151, 1)'))}
    } else if (input$rtHistoric & length(input$rtParam)==2 & input$rtHistParam=="Level"){
      if ("Max-Min Range" %in% input$rtHistStats){
        plot <- plot %>%  add_ribbons(data=rtHistoricData() %>% filter(Parameter==input$rtHistParam)
                                      ,x= ~DateTime,ymin= ~Minimum, ymax= ~Maximum,name="Historic Max-Min Range",
                                      hoverinfo= 'text',text=~paste0('Maximum: ',Maximum,'\nMinimum: ',Minimum,
                                                                     '\n',MonthDay),
                                      color=I("lightblue1"), yaxis = "y2")}
      if ("5-95 Percentile Range" %in% input$rtHistStats){
        plot <- plot %>%  add_ribbons(data=rtHistoricData() %>% filter(Parameter==input$rtHistParam),
                                      x= ~DateTime,ymin= ~Percentile5, ymax= ~Percentile95,name="Historic 5-95 Percentile Range",
                                      hoverinfo= 'text',text=~paste0('95th Percentile: ',Percentile95,'\n5th Percentile: ',Percentile5,
                                                                     '\n',MonthDay),
                                      color=I("lightblue2"), yaxis = "y2")}
      if ("25-75 Percentile Range" %in% input$rtHistStats){
        plot <- plot %>%  add_ribbons(data=rtHistoricData() %>% filter(Parameter==input$rtHistParam),
                                      x= ~DateTime,ymin= ~Percentile25, ymax= ~Percentile75,name="Historic 25-75 Percentile Range",
                                      hoverinfo= 'text',text=~paste0('75th Percentile: ',Percentile75,'\n25th Percentile: ',Percentile25,
                                                                     '\n',MonthDay),
                                      color=I("lightblue3"), yaxis = "y2")}
      if ("Mean" %in% input$rtHistStats){
        plot <- plot %>%  add_lines(data=rtHistoricData() %>% filter(Parameter==input$rtHistParam),
                                    x= ~DateTime,y=~Mean,name="Historic Daily Mean",
                                    hoverinfo= 'text',text=~paste('Mean: ',Mean,'\nDay: ',MonthDay),
                                    line=list(color='rgba(61,151,53, 1)'), yaxis = "y2")}
      if ("Median" %in% input$rtHistStats){
        plot <- plot %>%  add_lines(data=rtHistoricData() %>% filter(Parameter==input$rtHistParam),
                                    x= ~DateTime,y=~Median,name="Historic Daily Median",
                                    hoverinfo= 'text',text=~paste('Median: ',Median,'\nDay: ',MonthDay),
                                    line=list(color='rgba(143,53,151, 1)'), yaxis = "y2")}
    }
    
    # Add real-time data layers
    if (length(input$rtParam)==2) { #If both flow and water level
      plot <- plot %>%
        add_lines(data=realtimeData() %>% filter(Parameter=="Flow"),x= ~DateTime,y= ~Value, name="Instantaneous Discharge",
                  hoverinfo= 'text',text=~paste0('Inst. Discharge: ',Value,"cms",'\n',DateTime),
                  line=list(color='rgba(31, 119, 180, 1)')) %>% 
        add_lines(data=realtimeData() %>% filter(Parameter=="Level"),x= ~DateTime,y= ~Value, name="Instantaneous Water Level",
                  hoverinfo= 'text',text=~paste0('Inst. Water Level: ',Value,"m",'\n',DateTime), 
                  line=list(color='rgba(255, 127, 14, 1)'),yaxis = "y2") %>% 
        layout(xaxis=list(title="Date"),
               yaxis=rtplot.y(),
               yaxis2 = list(overlaying = "y",side = "right",title = "Water Level (m)"))}
    else if (length(input$rtParam)==1 & input$rtParam=="Flow") { #if just flow data
      plot <- plot %>%
        add_lines(data=realtimeData() %>% filter(Parameter=="Flow"),x= ~DateTime,y= ~Value, name="Instantaneous Discharge",
                  hoverinfo= 'text',text=~paste0('Inst. Discharge: ',Value,"cms",'\n',DateTime),
                  line=list(color='rgba(31, 119, 180, 1)')) %>% 
        layout(xaxis=list(title="Date"),
               yaxis=rtplot.y(),
               showlegend = TRUE)} 
    else if (length(input$rtParam)==1 & input$rtParam=="Level") { # if just level data
      plot <- plot %>%
        add_lines(data=realtimeData() %>% filter(Parameter=="Level"),x= ~DateTime,y= ~Value, name="Instantaneous Water Level",
                  hoverinfo= 'text',text=~paste0('Inst. Water Level: ',Value,"m",'\nDate/Time: ',DateTime),
                  line=list(color='rgba(255, 127, 14, 1)')) %>% 
        layout(xaxis=list(title="Date"),
               yaxis=list(title = "Water Level (m)"),
               showlegend = TRUE)} 
    if (input$rtLTMEAN){
      plot <- plot %>% 
        add_lines(x=c(min(realtimeData()$DateTime),max(realtimeData()$DateTime)),
                  y=mean(realtimeData() %>% filter(Parameter=="Flow") %>% select(Value),na.rm = TRUE))
    }
    
    
    plot
  })
  
  # Create table to display
  rtTableOutput <- reactive({
    data <- realtimeData() %>% select(DateTime,Parameter,Value) %>% 
      mutate(DateTime=format.Date(DateTime, method = 'toISOString'))
    
    
    if ("Flow" %in% data$Parameter & "Level" %in% data$Parameter) { # both Q and H
      data.flow <- data %>% filter(Parameter=="Flow") %>% 
        mutate("Flow (cms)"=Value)%>%
        select(-Parameter,-Value)
      data.level <- data %>% filter(Parameter=="Level") %>% 
        mutate("Water Level (m)"=Value)%>%
        select(-Parameter,-Value)
      table.data <- merge(data.flow,data.level,by=c("DateTime"),all=TRUE) 
    } else if ("Flow" %in% data$Parameter & !("Level" %in% data$Parameter)) { # just Q
      table.data <- data %>% 
        rename("Flow (cms)"=Value) %>%
        select(-Parameter)
    } else if (!("Flow" %in% data$Parameter) & "Level" %in% data$Parameter) { # just H
      table.data <- data %>% 
        rename("Water Level (m)"=Value) %>%
        select(-Parameter)
    }
    table.data
  })
  
  #Create and render table output
  output$realtimeTable <- DT::renderDataTable(
    rtTableOutput() %>% 
      arrange(desc(DateTime)) %>% 
      rename("Date and Time (local)"=DateTime),
    rownames=FALSE,
    selection=list(mode="single"),
    filter = 'top',
    extensions = c("Scroller"),
    options = list(scrollX = TRUE,
                   scrollY=450,deferRender = TRUE,scroller = TRUE,
                   columnDefs = list(list(className = 'dt-center', targets = "_all")),
                   dom = 'Bfrtip', 
                   colReorder = TRUE,
                   buttons= list(list(extend='colvis',columns="_all"))
    )
  )
  
  # Download button for data
  output$download.rtData <- downloadHandler(
    filename = function() {paste0(metaData()[1,2]," - real-time discharge.csv")},
    content = function(file) {
      write.csv(realtimeData(),file, row.names = FALSE, na="")
    })  
  
  
  ######################################################################
  ### Under Development  ###
  #####################################################################
  
  
  
  
  # Deal with later
  #observeEvent(input$downloadHYDAT, {
  #  download_hydat(dl_hydat_here = getwd())
  #})
  
  ######################################################################
  ######################################################################
  
  
}


shinyApp(ui = ui, server = server)