# Copyright 2017 Province of British Columbia
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



library(shiny) # 1.0.3 shiny
library(shinydashboard)
library(dplyr) ## >0.7.0 dplyr
library(tidyr)
library(leaflet)
library(tidyhydat)
library(plotly)
library(httr)


## Create a dataframe of all station metadata and a list of all stations
stations <- STATIONS(PROV_TERR_STATE_LOC = "BC") %>%  #c("AB","BC","SK","MB","ON","QC","NB","NS","PE","NL","YT","NT","NU")
  left_join(AGENCY_LIST(), by = c("CONTRIBUTOR_ID" = "AGENCY_ID")) %>% rename("CONTRIBUTOR"=AGENCY_EN) %>% 
  left_join(AGENCY_LIST(), by = c("OPERATOR_ID" = "AGENCY_ID")) %>%  rename("OPERATOR"=AGENCY_EN) %>% 
  left_join(DATUM_LIST(), by = c("DATUM_ID" = "DATUM_ID")) %>% rename("DATUM"=DATUM_EN) %>% 
  mutate(REGIONAL_OFFICE_ID = as.integer(REGIONAL_OFFICE_ID)) %>% 
  left_join(REGIONAL_OFFICE_LIST(), by = c("REGIONAL_OFFICE_ID" = "REGIONAL_OFFICE_ID")) %>% rename("REGIONAL_OFFICE"=REGIONAL_OFFICE_NAME_EN) %>% 
  left_join(STN_REGULATION(), by="STATION_NUMBER") %>% 
  select(STATION_NUMBER,STATION_NAME,PROV_TERR_STATE_LOC,HYD_STATUS,LATITUDE,LONGITUDE,DRAINAGE_AREA_GROSS,RHBN,
         REAL_TIME,REGULATED,CONTRIBUTOR,OPERATOR,REGIONAL_OFFICE,DATUM)
stations.list <- as.list(stations$STATION_NUMBER)

#######################################################################################
### # Set up the user-interface
#######################################################################################

ui <- dashboardPage(
  dashboardHeader(title="HYDAT Data Viewer"),
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
      textOutput("localHYDAT"),
      textOutput("onlineHYDAT"),br(),br(),
      textOutput("test")#,
      #actionButton("downloadHYDAT","Download HYDAT")  #NEED TO MOVE QUESTION FROM CONSOLE TO WINDOW
      )
    
    
  ),
  dashboardBody(
    fluidPage(
      tabBox("TITLE",width = 12,
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
                                      downloadButton('download.stations', 'Download Filtered Table'),br(),br(),
                                      actionButton('stationsMapAdd', 'Show Filtered Stations on Map')
                               )),
                      br(),
                      DT::dataTableOutput("allstationsTable")
                      
                                      ),
             tabPanel("Stations Map",
                      br(),
                      fluidPage(column(width=8,
                                       tags$style(type = "text/css", "#map {height: calc(100vh - 170px) !important;}"),
                                       leafletOutput("map")),
                                column(width=4,
                                       box(width=12,status = "primary",
                                           h5("Map Settings"),
                                           selectizeInput("mapProvince","Province:",
                                                          choices=c("AB","BC","SK","MB","ON","QC",
                                                                    "NB","NS","PE","NL","YT","NT","NU"),
                                                          selected="BC",
                                                          multiple=TRUE),
                                           selectizeInput("mapStatus","Status:",
                                                          choices=c("Active","Discontinued"),
                                                          selected=c("Active","Discontinued"),
                                                          multiple=TRUE),
                                           selectizeInput("mapReg","Regulation:",
                                                          choices=c("Natural","Regulated"),
                                                          selected=c("Natural","Regulated"),
                                                          multiple=TRUE),
                                           selectizeInput("mapRT","Real-time:",
                                                          choices=c("Yes","No"),
                                                          selected=c("Yes","No"),
                                                          multiple=TRUE),
                                           uiOutput("mapDA"))))),
             tabPanel("Station Info",
                      br(),
                      fluidRow(column(width = 6,
                                      box(width=12,title="Station Information",status = "primary",solidHeader = TRUE,
                                          tableOutput("metaTable"))),
                               column(width = 6,
                                      box(width=12,background="light-blue",
                                          leafletOutput("stnmap"))))),
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
                        fluidRow(column(width=3, 
                                        downloadButton('download.ltData', 'Complete Daily Dataset'))),
                        br(),
                        fluidRow(
                          tabBox(width = 12,
                                 tabPanel("Graph",
                                          fluidRow(column(width=12,
                                                          h4(textOutput("ltplot.title")))),
                                          fluidRow(column(width=12,plotlyOutput('ltplot'))),
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
                        fluidRow(column(width=12, 
                                        downloadButton('download.annualData', 
                                                       label='Annual Summary Data'), 
                                        downloadButton('download.annualPeakData', 
                                                       label='Annual Peak Data'))),
                        br(),
                        fluidRow(
                          tabBox(width = 12,
                                 tabPanel("Graph",
                                          fluidRow(column(width=12,h4(textOutput("annualPlot.title")))),
                                          fluidRow(column(width=12,plotlyOutput('annualPlot'))),
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
                                          h5("clean up table"),
                                          DT::dataTableOutput("annualTable"))
                          )
                        )
                      ),
                      # Historical Monthly
                      conditionalPanel(
                        condition = "input.histView == 'Monthly'",
                        fluidRow(column(width=3, 
                                        downloadButton('download.monthData', 'Monthly Summary Data'),
                                        downloadButton('download.HYDATmonthData', 'Monthly HYDAT Data'))),
                        br(),
                        fluidRow(
                          tabBox(width = 12,
                                 tabPanel("Graph",
                                          fluidRow(column(width=12,h4(textOutput("monthPlot.title")))),
                                          fluidRow(column(width=12,plotlyOutput('monthPlot'))),
                                          h5("* Missing dates ignored"),
                                          br(),br(),
                                          fluidRow(box(width = 9,title = "Graph Options",status = "primary",
                                                       fluidRow(column(width=3,hr(),
                                                                       uiOutput("monthParam"),
                                                                       checkboxInput("monthlog", 
                                                                                     label = "Log scale on primary Y-axis", 
                                                                                     value= FALSE)),
                                                                column(width=1),
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
                                                                                     value=TRUE)),
                                                                column(width=1),
                                                                column(width=3,hr(),
                                                                       uiOutput("monthYear"),
                                                                       uiOutput("monthYearStat")))))
                                 ),
                                 tabPanel("Table",
                                          h5("*** fix year and month stats names (Percentile25"),
                                          h5("*** ADD if statements plot code to specify colours"),
                                          h5("*** FIX TABLE BELOW"),
                                          DT::dataTableOutput("monthTable")
                                 )
                          )
                        )
                      ),
                      
                      conditionalPanel(
                        condition = "input.histView == 'Daily'",
                        fluidRow(column(width =3, 
                                        downloadButton('download.dailySummaryData', 'Daily Summary Data'))),
                        br(),
                        fluidRow(
                          tabBox(width = 12,
                                 tabPanel("Graph",
                                          fluidRow(column(width=12,h4(textOutput("dailyPlot.title")))),
                                          fluidRow(column(width=12,plotlyOutput('dailyPlot'))),
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
                                          h4("SELECTIZE BOX OR GROUPCHECKBOXES/RADIO BUTTONS TO SELECT WHATS ON GRAPH"),
                                          h4("add station number column to all outputted datasets")
                                 )
                                 
                          )))
             ),
             tabPanel("Real-time Data",
                      fluidRow(column(6, h4(textOutput("noRT")))),
                      textOutput("rtExists"),tags$head(tags$style("#rtExists{color: white}")),
                      conditionalPanel(
                        condition = "output.rtExists=='Yes'",
                        fluidRow(column(3, downloadButton('download.rtData', 'Real-time Data'))),
                        br(),
                        fluidRow(column(12,h4(textOutput("rtplot.title")))),
                        fluidRow(column(12,plotlyOutput('rtplot'))),
                        br(),br(),
                        fluidRow(box(width =4,title = "Graph Options",status = "primary",
                                     fluidRow(column(12,hr(),
                                                     uiOutput("rtParam"),
                                                     checkboxInput("rtlog", 
                                                                   label = "Log scale on 'Discharge' axis", 
                                                                   value= FALSE))))))),
             tabPanel("Station Comparison")
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
  
  output$onlineHYDAT <- renderText({
    base_url <- "http://collaboration.cmc.ec.gc.ca/cmc/hydrometrics/www/"
    x <- httr::GET(base_url)
    new_hydat <- substr(gsub(
      "^.*\\Hydat_sqlite3_", "",
      httr::content(x, "text")
    ), 1, 8)
    paste0("Available: ",as.Date(new_hydat, "%Y%m%d"))
    
  })
  output$localHYDAT <- renderText({
    paste0("Local: ",as.Date(as.data.frame(VERSION())[,2]))
    
  })
  
  ### Setting up the Stations Listings ###
  ########################################
  
  allstationsTable <- reactive({
    
    stn.meta.HYDAT <- stations %>% filter(STATION_NUMBER==stations.list)
    
    stn.info <-stn.meta.HYDAT[,c(1:4,7:13)] %>% 
      mutate(DRAINAGE_AREA_GROSS=round(DRAINAGE_AREA_GROSS,2)) %>% 
      rename("Station Number"=STATION_NUMBER,"Station Name" =STATION_NAME,"Prov/ Terr/ State"=PROV_TERR_STATE_LOC,
             "Station Status"=HYD_STATUS, "Drainage Area (sq km)"=DRAINAGE_AREA_GROSS,"Reference (RHBN)"=RHBN,
             "Real-Time"=REAL_TIME,"Contributor"=CONTRIBUTOR,"Operator"=OPERATOR,"Regional Office"=REGIONAL_OFFICE,
             "Regulation"=REGULATED)
  }) 
  
  output$allstationsTable <- DT::renderDataTable(
    allstationsTable(), 
    rownames=FALSE,
    selection=list(mode="single"),
    filter = 'top',
    extensions = c("Scroller","ColReorder","Buttons"),
    options = list(scrollX = TRUE,
                   scrollY=450,deferRender = TRUE,scroller = TRUE,
                   dom = 'Bfrtip', 
                   colReorder = TRUE,
                   buttons= list(list(extend='colvis',columns=c(1:10))))
  ) 
  
  proxy = DT::dataTableProxy('allstationsTable')   # Allows for updating the selected row from the select-widget and the map
  
  
  ### Setting the station ###
  ###########################
  
  # Create reactive values$station to be selected by the selectbox, data.table and map click
  values <- reactiveValues()
  values$station <- "08HB048"#as.data.frame(stations)[1,1]
  
  output$test <- renderText({values$station})
  
  # Select station with widget
  output$stnSelect <- renderUI({
    selectizeInput("station", label = "Station Number:",
                   choices = stations.list, 
                   selected=values$station,
                   options = list(placeholder ="type station ID number",maxOptions = 2420 ))
  })
  
  # Select station by clicking on button for widget
  observeEvent(input$selectStation, {
    isolate(values$station <- input$station)
  })
  
  # Select station by clicking on row in datatable
  observeEvent(input$allstationsTable_rows_selected, {
    isolate(values$station <- as.data.frame(allstationsTable())[input$allstationsTable_rows_selected,1])
  })
  
  # Select station by clicking on station in map
  observeEvent(input$map_marker_click, {
    isolate(values$station <- input$map_marker_click$id)
    updateSelectizeInput(session, "station", selected=input$map_marker_click$id)
    proxy %>% DT::selectRows(which(stations.list == values$station))
  })
  
  
  ### Create the Stations Map  ###
  ################################
  
  output$map <- renderLeaflet({
    leaflet(stations) %>% addTiles() %>%
      #setView(lng = -125, lat = 54, zoom = 5) # set centre and extent of map
      addCircleMarkers(data= stations, lng = ~LONGITUDE, lat = ~LATITUDE, layerId = ~STATION_NUMBER, color = "blue", radius = 2,
                       label = ~paste0(STATION_NAME, " (",STATION_NUMBER,") - ",HYD_STATUS))
    
  })
  
  # Allows the selection of stations without redrawing the map
  observe({
    leafletProxy("map") %>%
      removeMarker(layerId="selected") %>%
      addCircleMarkers(layerId="selected",
                       data = filter(stations, STATION_NUMBER %in% values$station),
                       lng = ~LONGITUDE,lat = ~LATITUDE, 
                       color = "green", radius = 6)
  })
  
  # Displayed the chosen station on the map
  observeEvent(input$stationsMapAdd, {
    leafletProxy("map") %>%
      clearMarkers() %>%
      clearControls() %>% 
      addCircleMarkers(data= downloadStationsList(), 
                       lng = ~LONGITUDE, lat = ~LATITUDE, 
                       layerId = ~STATION_NUMBER, 
                       color = "blue", radius = 2,
                       label = ~paste0(STATION_NAME, " (",STATION_NUMBER,") - ",HYD_STATUS))
  })
  
  ## Format the station lists for download
  downloadStationsList <- reactive({
    list <- stations %>% 
      mutate(row=c(1:(n()))) %>% 
      filter(row %in% input$allstationsTable_rows_all) %>% 
      select(-row)
    list
  })
  output$download.stations <- downloadHandler(
    filename = function() {paste0("Stations_Table.csv")},
    content = function(file) {
      write.csv(downloadStationsList(),file, row.names = FALSE, na="")
    })
  
  
  ### Displaying Station Information ###
  ######################################
  
  # Filter and format table for chosen station (used for rendering output and looking up parameters)
  metaData <- reactive({
    
    stn.meta.HYDAT <- stations %>% filter(STATION_NUMBER==values$station)
    
    stn.info <- stn.meta.HYDAT %>% 
      mutate("Historical Data Link"=paste0("https://wateroffice.ec.gc.ca/report/historical_e.html?stn=",STATION_NUMBER),
             "Real-Time Data Link"=ifelse(REAL_TIME==TRUE, paste0("https://wateroffice.ec.gc.ca/report/real_time_e.html?stn=",STATION_NUMBER),"No real-time data available."),
             DRAINAGE_AREA_GROSS=round(DRAINAGE_AREA_GROSS,2),
             LATITUDE=round(LATITUDE,6),
             LONGITUDE=round(LONGITUDE,6)) %>% 
      rename("Station Number"=STATION_NUMBER,"Station Name" =STATION_NAME,"Prov/Terr/State"=PROV_TERR_STATE_LOC,
             "Station Status"=HYD_STATUS, "Latitude"=LATITUDE,"Longitude"=LONGITUDE,"Drainage Area (sq km)"=DRAINAGE_AREA_GROSS,
             "Reference (RHBN)"=RHBN,"Real-Time"=REAL_TIME,"Regulation"=REGULATED,"Regional Office"=REGIONAL_OFFICE,
             "Contributor"=CONTRIBUTOR,"Operator"=OPERATOR,"Datum"=DATUM) %>% 
      gather("header","content",1:16)
    
    stn.info[is.na(stn.info)] <- ""
    stn.info
  })
  
  # Render table for output
  output$metaTable <- renderTable(metaData(),colnames = FALSE)
  
  # Render map of chosen station
  output$stnmap <- renderLeaflet({
    leaflet(stations) %>% addTiles() %>%
      setView(lng = as.numeric(metaData()[6,2]), lat = as.numeric(metaData()[5,2]), zoom = 9) %>% # set centre and extent of map
      addCircleMarkers(data = filter(stations, STATION_NUMBER %in% values$station), 
                       lng = ~LONGITUDE, lat = ~LATITUDE, color = "red", radius = 6) %>%
      addCircles(lng = ~LONGITUDE, lat = ~LATITUDE, weight = 1,
                 radius = 1, label = ~STATION_NAME, 
                 popup = ~paste(STATION_NAME, "<br>",
                                STATION_NUMBER, "<br>",
                                "DRAINAGE AREA = ",DRAINAGE_AREA_GROSS, "SQ. KM","<br>")
      )
  })
  
  
  ###################################################################################
  ### Historical Data 
  ###################################################################################
  
  # Extract daily discharge and water level data from HYDAT (used for long-term data,and monthly and daily statistics)
  dailyData <- reactive({
    # check which data to extract
    check <- STN_DATA_RANGE(STATION_NUMBER=values$station) %>% filter(DATA_TYPE=="Q"|DATA_TYPE=="H")
    
    # extract flow or water level data depending on what is available
    if ("Q" %in% check$DATA_TYPE & "H" %in% check$DATA_TYPE) { # both Q and H
      daily.flow.HYDAT <- DLY_FLOWS(STATION_NUMBER=values$station)
      daily.levels.HYDAT <- DLY_LEVELS(STATION_NUMBER=values$station)
      daily.data <- rbind(daily.flow.HYDAT[,c(2:5)],daily.levels.HYDAT[,c(2:5)])
    } else if ("Q" %in% check$DATA_TYPE & !("H" %in% check$DATA_TYPE)) { # just Q
      daily.flow.HYDAT <- DLY_FLOWS(STATION_NUMBER=values$station)
      daily.data <- daily.flow.HYDAT[,c(2:5)]
    } else if (!("Q" %in% check$DATA_TYPE) & "H" %in% check$DATA_TYPE) { # just H
      daily.levels.HYDAT <- DLY_LEVELS(STATION_NUMBER=values$station)
      daily.data <- daily.levels.HYDAT[,c(2:5)]
    }
    
    # fill in any date data gaps (including completing the first and last years of data)
    daily.data.temp <- as.data.frame(daily.data[0,])
    for (parameter in unique(daily.data$Parameter)) {
      daily.data.param <- daily.data %>% filter(Parameter==parameter)
      min.date <- as.Date((paste((as.numeric(format(min(daily.data.param$Date),'%Y'))),01,01,sep="-")),"%Y-%m-%d")
      max.date <- as.Date((paste((as.numeric(format(max(daily.data.param$Date),'%Y'))),12,31,sep="-")),"%Y-%m-%d")
      data.empty <- data.frame(Date=seq(min.date, max.date, by="days"))
      data.temp <- merge(data.empty,daily.data.param,by="Date",all = TRUE)
      data.temp$Parameter <- parameter
      daily.data.temp <- as.data.frame(rbind(daily.data.temp,data.temp))
    }
    daily.data <- daily.data.temp
  })
  
  # Determine the min and max years of the dataset
  histDates <- reactive({
    daily.flow.dates <- dailyData() %>% 
      summarize(minDate=as.numeric(format(min(Date),'%Y')),
                maxDate=as.numeric(format(max(Date),'%Y')))
  })
  
  # create the slider widget to filter data
  output$histYears <- renderUI({
    sliderInput("histYears",
                label="Filter data between the following years:",
                min=histDates()$minDate,
                max=histDates()$maxDate,
                value=c(histDates()$minDate,histDates()$maxDate),
                sep = "")
  })
  
  
  ### Historical Long-term Data
  ###################################################################################
  
  # Select the parameters to plot
  output$ltParam <- renderUI({
    selectizeInput("ltParam",
                   label="Display parameters:",
                   choices=as.list(unique(dailyData()$Parameter)),
                   selected=as.list(unique(dailyData()$Parameter)), 
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
    paste0("Daily Data - ",metaData()[2,2]," (",metaData()[1,2],")")
  })
  
  # Plot the discharge axis on log-scale if checked
  ltplot.y <- reactive({
    if (input$ltlog) {list(title= "Discharge (cms)",type= "log")}
    else {            list(title= "Discharge (cms)")}
  })
  
  # Create the long-term plot
  output$ltplot <- renderPlotly({
    
    plot.data <- dailyData() %>% 
      mutate(Symbol=replace(Symbol, Symbol=="E", "Estimate"),
             Symbol=replace(Symbol, Symbol=="A", "Partial Day"),
             Symbol=replace(Symbol, Symbol=="B", "Ice conditions"),
             Symbol=replace(Symbol, Symbol=="D", "Dry"),
             Symbol=replace(Symbol, Symbol=="R", "Revised"),
             Symbol=replace(Symbol, is.na(Symbol), ""),
             Value=round(Value,3),
             Year=as.numeric(format(Date,'%Y'))) %>% 
      filter(Year >= input$histYears[1] & Year <= input$histYears[2]) # filter for the years from the slider
    
    
    if (length(input$ltParam)==2) { # Plot if both flow and water level
      plot <-  plot_ly() %>%
        add_lines(data=plot.data %>% filter(Parameter=="FLOW"),x= ~Date,y= ~Value, name="Discharge",
                  text=~paste('Discharge: ',Value,"cms",Symbol,'\nDate: ',Date)) %>%
        add_lines(data=plot.data %>% filter(Parameter=="LEVEL"),x= ~Date,y= ~Value, name="Water Level", yaxis = "y2",
                  text=~paste('Water Level: ',Value,"m",Symbol,'\nDate: ',Date)) %>%
        layout(xaxis=list(title="Date"),
               yaxis=ltplot.y(),
               yaxis2 = list(overlaying = "y",side = "right",title = "Water Level (m)"))} 
    else if (length(input$ltParam)==1 & input$ltParam=="FLOW") { # Plot if just flow
      plot <- plot_ly() %>%
        add_lines(data=plot.data %>% filter(Parameter=="FLOW"),x= ~Date,y= ~Value, name="Discharge",hoverinfo= 'text',
                  text=~paste('Discharge: ',Value,"cms",Symbol,'\nDate: ',Date)) %>%
        layout(xaxis=list(title="Date"),
               yaxis=ltplot.y(),
               showlegend = TRUE)}
    else if (length(input$ltParam)==1 & input$ltParam=="LEVEL") { # Plot if just water level
      plot <-plot_ly() %>%
        add_lines(data=plot.data %>% filter(Parameter=="LEVEL"),x= ~Date,y= ~Value, name="Water Level",
                  text=~paste('Water Level: ',Value,"m",Symbol,'\nDate: ',Date)) %>%
        layout(xaxis=list(title="Date"),
               yaxis=list(title = "Water Level (m)"),
               showlegend = TRUE)}
    
    # Add data symbol to plot if selected
    if (length(input$paramSymbol)>0) {
      plot <- plot %>% add_markers(data=plot.data %>% filter(Parameter %in% input$paramSymbol),x= ~Date,y= 0, color= ~Symbol)}
    
    plot
  })
  
  # Create and render the long-term table to view
  ltTableOutput <- reactive({
    data <- dailyData() %>% mutate(Value=round(Value,3),
                                   Year=as.integer(format(Date,'%Y')),
                                   Month=as.character(format(Date,'%B'))) %>%
      select(Date,Year,Month,Parameter,Value,Symbol)
    
    check <- STN_DATA_RANGE(STATION_NUMBER=values$station) %>% filter(DATA_TYPE=="Q"|DATA_TYPE=="H")
    
    if ("Q" %in% check$DATA_TYPE & "H" %in% check$DATA_TYPE) { # both Q and H
      data.flow <- data %>% filter(Parameter=="FLOW") %>% 
        mutate("Flow (cms)"=Value,"Flow Symbol"=Symbol)%>%
        select(-Parameter,-Value,-Symbol)
      data.level <- data %>% filter(Parameter=="LEVEL") %>% 
        mutate("Water Level (m)"=Value,"Water Level Symbol"=Symbol)%>%
        select(-Parameter,-Value,-Symbol)
      table.data <- merge(data2,data3,by=c("Date","Year","Month"),all=TRUE) 
    } else if ("Q" %in% check$DATA_TYPE & !("H" %in% check$DATA_TYPE)) { # just Q
      table.data <- data %>% 
        rename("Flow (cms)"=Value,"Flow Symbol"=Symbol) %>%
        select(-Parameter)
    } else if (!("Q" %in% check$DATA_TYPE) & "H" %in% check$DATA_TYPE) { # just H
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
  
  #Download data button
  output$download.ltData <- downloadHandler(
    filename = function() {paste0(metaData()[1,2]," - daily discharge.csv")},
    content = function(file) {
      write.csv(dailyData(),file, row.names = FALSE, na="")
    })
  
  
  
  ### Historical Annual Data
  ###################################################################################
  
  # Extract monthly discharge and water level data from HYDAT
  annualData <- reactive({
    
    annual <- ANNUAL_STATISTICS(STATION_NUMBER=values$station) %>% 
      filter(Parameter=="Flow" | Parameter == "Water Level") %>% 
      mutate(Parameter=replace(Parameter, Parameter=="Flow", "FLOW"),
             Parameter=replace(Parameter, Parameter=="Water Level", "LEVEL"))
    
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
    annual.instant <- ANNUAL_INSTANT_PEAKS(STATION_NUMBER=values$station)
    
    annual.instant <- annual.instant %>% 
      mutate(Date=as.Date(paste(YEAR,MONTH,DAY,sep="-"),format="%Y-%m-%d"),
             Time=paste0(HOUR,":",ifelse(nchar(MINUTE)>1,paste(MINUTE),paste0(0,MINUTE))," ",TIME_ZONE),
             DateTime=paste0("On ",Date," at ",Time),
             Symbol=replace(Symbol, is.na(Symbol), "")) %>% 
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
                   choices = c("Minimum"="MIN","Maximum"="MAX"),#as.list(unique(annInstData()[annInstData()$Parameter == input$annualParam,]$PEAK_CODE)),
                   multiple =TRUE)
  })
  
  
  
  # Create annual plot title
  output$annualPlot.title <- renderText({
    paste0("Annual ",ifelse(input$annualParam=="FlOW",paste("FLow"),paste("Water Level"))," Statistics - ",metaData()[2,2]," (",metaData()[1,2],")")
  })
  
  # Plot the primary y-axis on log-scale if checked
  annualPlot.y <- reactive({
    if (input$annuallog) {list(title=ifelse(input$annualParam== "FLOW","Discharge (cms)","Water Level (m)"),type= "log")}
    else {                list(title=ifelse(input$annualParam== "FLOW","Discharge (cms)","Water Level (m)"))}
  })
  
  # Create the annual plot
  output$annualPlot <- renderPlotly({
    plot.data <- annualData() %>% filter(Parameter==input$annualParam)
    
    plot <- plot_ly() %>% 
      layout(xaxis=list(title="Year"),
             yaxis=annualPlot.y(),
             showlegend = TRUE)
    
    # Add each annual statistic if selected
    if ("MAX" %in% input$annualStat){
      plot <- plot %>%  add_trace(data=plot.data %>% filter(Sum_stat=="MAX"),
                                  x= ~Year,y=~Value,name="Daily Maximum",
                                  mode = 'lines+markers',text=~paste("On",Date," ",Symbol),
                                  line=list(color='rgba(1,102,94, 1)'))}
    if ("MEAN" %in% input$annualStat){
      plot <- plot %>%  add_trace(data=plot.data %>% filter(Sum_stat=="MEAN"),
                                  x= ~Year,y=~Value,name="Daily Mean",
                                  mode = 'lines+markers',
                                  line=list(color='rgba(61,151,53, 1)'))}
    if ("MIN" %in% input$annualStat){
      plot <- plot %>%  add_trace(data=plot.data %>% filter(Sum_stat=="MIN"),
                                  x= ~Year,y=~Value,name="Daily Minimum",
                                  mode = 'lines+markers',text=~paste("On",Date," ",Symbol),
                                  line=list(color='rgba(140,81,10, 1)'))}
    
    # Add instantaneous peaks if selected
    if ("MAX" %in% input$annInstStat){
      plot <- plot %>% add_trace(data=annInstData() %>% filter(Parameter==input$annualParam & PEAK_CODE == "MAX"),
                                 x= ~YEAR,y= ~Value, name="Instanteous Maximum",mode = 'markers',
                                 marker=list(symbol=2,size=8,color='rgba(10,69,140,1)'), text=~paste0(DateTime," ",Symbol))} 
    if ("MIN" %in% input$annInstStat){
      plot <- plot %>% add_trace(data=annInstData() %>% filter(Parameter==input$annualParam & PEAK_CODE == "MIN"),
                                 x= ~YEAR,y= ~Value, name="Instanteous Minimum",mode = 'markers',
                                 marker=list(symbol=2,size=8,color='rgba(140,16,10,1)'), text=~paste0(DateTime," ",Symbol))}
    plot
  })
  
  #Create and render table output
  #include extremes somwehow?
  output$annualTable <- DT::renderDataTable({
    annualData()
  })
  
  #Download data buttons
  output$download.annualData <- downloadHandler(
    filename = function() {paste0(metaData()[1,2]," - annual summary.csv")},
    content = function(file) {
      write.csv(annualData(),file, row.names = FALSE, na="")
    })  
  output$download.annualPeakData <- downloadHandler(
    filename = function() {paste0(metaData()[1,2]," - annual instantaneous peaks.csv")},
    content = function(file) {
      write.csv(annInstData(),file, row.names = FALSE, na="")
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
      summarize(Mean=mean(Value, na.rm=TRUE),
                Maximum=max(Value, na.rm=TRUE),
                Minimum=min(Value, na.rm=TRUE),
                Median=median(Value, na.rm=TRUE),
                Percentile75=quantile(Value,.75, na.rm=TRUE),
                Percentile25=quantile(Value,.25,na.rm=TRUE),
                Percentile95=quantile(Value,.95,na.rm=TRUE),
                Percentile5=quantile(Value,.05,na.rm=TRUE)) %>% 
      gather(Stat,Value,3:10)
    
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
      summarize(Mean=mean(Value, na.rm=TRUE),
                Maximum=max(Value, na.rm=TRUE),
                Minimum=min(Value, na.rm=TRUE),
                Median=median(Value, na.rm=TRUE),
                Percentile75=quantile(Value,.75, na.rm=TRUE), 
                Percentile25=quantile(Value,.25,na.rm=TRUE),
                Percentile95=quantile(Value,.95,na.rm=TRUE),
                Percentile5=quantile(Value,.05,na.rm=TRUE)) %>% 
      gather(Sum_stat,Value,4:11)
    
    month.data
    
  })
  
  # Extract the HYDAT data for table and download button
  HYDATmonthData <- reactive({
    check <- STN_DATA_RANGE(HYDAT.path, STATION_NUMBER=values$station) %>% filter(DATA_TYPE=="Q"|DATA_TYPE=="H")
    
    if ("Q" %in% check$DATA_TYPE & "H" %in% check$DATA_TYPE) { # both Q and H
      monthly.flows.hydat <- MONTHLY_FLOWS(hydat_path = HYDAT.path, STATION_NUMBER=values$station) %>%
        mutate(Parameter="FLOW")
      monthly.levels.hydat <- MONTHLY_LEVELS(hydat_path = HYDAT.path, STATION_NUMBER=values$station) %>%
        select(-PRECISION_CODE) %>% mutate(Parameter="LEVEL")
      monthly.data <- rbind(monthly.flows.hydat,monthly.levels.hydat) %>%
        select(Parameter,YEAR,MONTH,Sum_stat,Value,Date_occurred)
    } else if ("Q" %in% check$DATA_TYPE & !("H" %in% check$DATA_TYPE)) { # just Q
      monthly.flows.hydat <- MONTHLY_FLOWS(hydat_path = HYDAT.path, STATION_NUMBER=values$station) %>%
        mutate(Parameter="FLOW")
      monthly.data <- monthly.flows.hydat %>%
        select(Parameter,YEAR,MONTH,Sum_stat,Value,Date_occurred)
    } else if (!("Q" %in% check$DATA_TYPE) & "H" %in% check$DATA_TYPE) { # just H
      monthly.levels.hydat <- MONTHLY_LEVELS(hydat_path = HYDAT.path, STATION_NUMBER=values$station) %>%
        select(-PRECISION_CODE) %>% mutate(Parameter="LEVEL")
      monthly.data <- monthly.levels.hydat %>%
        select(Parameter,YEAR,MONTH,Sum_stat,Value,Date_occurred)
    }
    
    monthly.data <- as.data.frame(monthly.data) %>%
      mutate(YEAR = as.factor(YEAR))
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
                   choices =   c("Mean"="Mean","Maximum"="Maximum","Minimum"="Minimum","Median"="Median","75th Percentile"="Percentile75",
                                 "25th Percentile"="Percentile25", "95th Percentile"="Percentile95", "5th Percentile"="Percentile5"),
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
                   choices = c("Mean"="Mean","Maximum"="Maximum","Minimum"="Minimum","Median"="Median","75th Percentile"="Percentile75", 
                               "25th Percentile"="Percentile25", "95th Percentile"="Percentile95", "5th Percentile"="Percentile5"),
                   multiple =TRUE)
  })
  
  #Render plot title
  output$monthPlot.title <- renderText({
    paste0("Monthly ",ifelse(input$monthParam=="FLOW",paste("Flow"),paste("Water Level")),
           " Statistics - ",metaData()[2,2]," (",metaData()[1,2],")")
  })
  
  # Plot the primary-Yaxis on log-scale if checked
  monthlyPlot.y <- reactive({
    if (input$monthlog) {list(title=ifelse(input$monthParam== "FLOW","Discharge (cms)","Water Level (m)"),type= "log")}
    else {               list(title=ifelse(input$monthParam== "FLOW","Discharge (cms)","Water Level (m)"))}
  })
  
  #Create and render the monthly plot
  output$monthPlot <- renderPlotly({
    
    plot.data <- monthData() %>% spread(Stat,Value) %>% filter(Parameter==input$monthParam)
    
    #Create the plot
    plot <- plot_ly() %>% 
      layout(xaxis=list(title="Month",tickvals = seq(1:12),ticktext = c("Jan","Feb","Mar","Apr","May","Jun","Jul",
                                                                        "Aug","Sep","Oct","Noc","Dec")),
             yaxis=monthlyPlot.y(),
             showlegend = TRUE)
    
    # Add ribbons if checked
    if (input$monthMaxMin){
      plot <- plot %>%  add_ribbons(data=plot.data,x= ~Month,ymin= ~Minimum, ymax= ~Maximum,name="Max-Min Range",color=I("lightblue1"))}
    #fillcolor="rgba(224,255,255,.7)",line=list(color='rgba(7, 164, 181, 0.05)'))}
    if (input$month90){
      plot <- plot %>%  add_ribbons(data=plot.data,x= ~Month,ymin= ~Percentile5, ymax= ~Percentile95,name="90% of Flows",color=I("lightblue2"))}
    # fillcolor="rgba(201,229,229,.7)",line=list(color='rgba(7, 164, 181, 0.05)'))}
    if (input$month50){
      plot <- plot %>%  add_ribbons(data=plot.data,x= ~Month,ymin= ~Percentile25, ymax= ~Percentile75,name="50% of Flows",color=I("lightblue3"))}
    #  fillcolor="rgba(179,204,204,.7)",line=list(color='rgba(7, 164, 181, 0.05)'))}
    # Add lines if selected
    if ("Maximum" %in% input$monthStat){
      plot <- plot %>%  add_trace(data=plot.data,x= ~Month,y=~Maximum,name="Maximum",
                                  mode = 'lines+markers',line=list(dash = 'dot',color='rgba(1,102,94, 1)'))}
    if ("Percentile95" %in% input$monthStat){
      plot <- plot %>%  add_trace(data=plot.data,x= ~Month,y=~Percentile95,name="95th Percentile",
                                  mode = 'lines+markers',line=list(dash = 'dot',color='rgba(53,151,143, 1)'))}
    if ("Percentile75" %in% input$monthStat){
      plot <- plot %>%  add_trace(data=plot.data,x= ~Month,y=~Percentile75,name="75th Percentile",
                                  mode = 'lines+markers',line=list(dash = 'dot',color='rgba(128,205,193, 1)'))}
    if ("Mean" %in% input$monthStat){
      plot <- plot %>%  add_trace(data=plot.data,x= ~Month,y=~Mean,name="Mean",
                                  mode = 'lines+markers',line=list(color='rgba(61,151,53, 1)'))}
    if ("Median" %in% input$monthStat){
      plot <- plot %>%  add_trace(data=plot.data,x= ~Month,y=~Median,name="Median",
                                  mode = 'lines+markers',line=list(color='rgba(143,53,151, 1)'))}
    if ("Percentile25" %in% input$monthStat){
      plot <- plot %>%  add_trace(data=plot.data,x= ~Month,y=~Percentile25,name="25th Percentile",
                                  mode = 'lines+markers',line=list(dash = 'dot',color='rgba(223,194,125, 1)'))}
    if ("Percentile5" %in% input$monthStat){
      plot <- plot %>%  add_trace(data=plot.data,x= ~Month,y=~Percentile5,name="5th Percentile",
                                  mode = 'lines+markers',line=list(dash = 'dot',color='rgba(191,129,45, 1)'))}
    if ("Minimum" %in% input$monthStat){
      plot <- plot %>%  add_trace(data=plot.data,x= ~Month,y=~Minimum,name="Minimum",
                                  mode = 'lines+markers',line=list(dash = 'dot',color='rgba(140,81,10, 1)'))}
    
    
    # Add data from specific years and specific stats    
    plot <- plot %>% add_trace(data=allmonthData() %>% 
                                 filter(Parameter==input$monthParam & Sum_stat %in% input$monthYearStat & Year %in% input$monthYear),
                               x= ~Month,y= ~Value, color=~paste0(Year," ",Sum_stat),mode = 'lines+markers',line=list(width=3))
    
    plot
  })
  
  #Create and render table output
  output$monthTable <- DT::renderDataTable({
    monthData()
  })
  #Create and render table output
  # output$monthHYDATTable <- DT::renderDataTable({
  #   HYDATmonthData()
  # })
  
  
  #Download data buttons
  output$download.monthData <- downloadHandler(
    filename = function() {paste0(metaData()[1,2]," - monthly summary.csv")},
    content = function(file) {
      write.csv(monthData(),file, row.names = FALSE, na="")
    }) 
  output$download.HYDATmonthData <- downloadHandler(
    filename = function() {paste0(metaData()[1,2]," - monthly summary HYDAT.csv")},
    content = function(file) {
      write.csv(HYDATmonthData(),file, row.names = FALSE, na="")
    })
  
  ### Historical Daily Summary Data
  ###################################################################################
  
  # Calculate daily data summaries and render for printing
  dailySummaryData <- reactive({
    daily.data <- dailyData()
    daily.data$Year <- as.numeric(format(daily.data$Date,'%Y'))
    daily.data$Day <- as.integer(format(daily.data$Date,'%j'))
    
    # Filter data for select years
    daily.data <- daily.data %>% 
      filter(Year >= input$histYears[1] & Year <= input$histYears[2])
    
    #Calculate summary stats
    dailysummary <- daily.data %>% 
      group_by(Parameter,Day) %>% 
      filter(Day<366) %>% 
      summarize(Mean=mean(Value, na.rm=TRUE),
                Maximum=max(Value, na.rm=TRUE),
                Minimum=min(Value, na.rm=TRUE),
                Median=median(Value, na.rm=TRUE),
                Percentile75=quantile(Value,.75, na.rm=TRUE),
                Percentile25=quantile(Value,.25,na.rm=TRUE),
                Percentile95=quantile(Value,.95,na.rm=TRUE),
                Percentile5=quantile(Value,.05,na.rm=TRUE))%>% 
      mutate(Date=as.Date(Day,origin = "1899-12-31")) %>% #format date so all in one year for plotting
      gather(Stat,Value,3:10)
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
                   choices = c("Mean"="Mean","Maximum"="Maximum","Minimum"="Minimum","Median"="Median","75th Percentile"="Percentile75", 
                               "25th Percentile"="Percentile25", "95th Percentile"="Percentile95", "5th Percentile"="Percentile5"),
                   multiple =TRUE,
                   selected = c("Mean","Median"))
  })
  
  # Render the title
  output$dailyPlot.title <- renderText({
    paste0("Daily ",
           ifelse(input$dailyParam=="FLOW",paste("Flow"),paste("Water Level")),
           " Statistics - ",metaData()[2,2]," (",metaData()[1,2],")")
  })
  
  # Plot the primary-Yaxis on log-scale if checked
  dailyPlot.y <- reactive({
    if (input$dailylog) {list(title=ifelse(input$dailyParam== "FLOW","Discharge (cms)","Water Level (m)"),type= "log")} 
    else {               list(title=ifelse(input$dailyParam== "FLOW","Discharge (cms)","Water Level (m)"))}
  })
  
  #Create and render the daily summary plot
  output$dailyPlot <- renderPlotly({
    
    plot.data <- dailySummaryData() %>% spread(Stat,Value)%>% filter(Parameter==input$dailyParam)
    
    #create the plot
    plot <- plot_ly() %>% 
      layout(xaxis=list(title="Day of Year"),#,tickformat= "%b-%d"
             yaxis=dailyPlot.y(),
             showlegend = TRUE)
    
    
    # Add ribbons if checked
    if (input$dailyMaxMin){
      plot <- plot %>%  add_ribbons(data=plot.data,x= ~Date,ymin= ~Minimum, ymax= ~Maximum,name="Max-Min Range",
                                    color=I("lightblue1"))}
    if (input$daily90){
      plot <- plot %>%  add_ribbons(data=plot.data,x= ~Date,ymin= ~Percentile95, ymax= ~Percentile5,name="90% of Flows",
                                    color=I("lightblue2"))}
    if (input$daily50){
      plot <- plot %>%  add_ribbons(data=plot.data,x= ~Date,ymin= ~Percentile25, ymax= ~Percentile75,name="50% of Flows",
                                    color=I("lightblue3"))}
    
    # Add lines if selected
    if ("Maximum" %in% input$dailyStat){
      plot <- plot %>%  add_lines(data=plot.data,x= ~Date,y=~Maximum,name="Maximum",line=list(color='rgba(1,102,94, 1)',width=2))}
    if ("Percentile95" %in% input$dailyStat){
      plot <- plot %>%  add_lines(data=plot.data,x= ~Date,y=~Percentile95,name="95th Percentile",line=list(color='rgba(53,151,143, 1)',width=2))}
    if ("Percentile75" %in% input$dailyStat){
      plot <- plot %>%  add_lines(data=plot.data,x= ~Date,y=~Percentile75,name="75th Percentile",line=list(color='rgba(128,205,193, 1)',width=2))}
    if ("Mean" %in% input$dailyStat){
      plot <- plot %>%  add_lines(data=plot.data,x= ~Date,y=~Mean,name="Mean",line=list(color='rgba(61,151,53, 1)',width=2))}
    if ("Median" %in% input$dailyStat){
      plot <- plot %>%  add_lines(data=plot.data,x= ~Date,y=~Median,name="Median",line=list(color='rgba(143,53,151, 1)',width=2))}
    if ("Percentile25" %in% input$dailyStat){
      plot <- plot %>%  add_lines(data=plot.data,x= ~Date,y=~Percentile25,name="25th Percentile",line=list(color='rgba(223,194,125, 1)',width=2))}
    if ("Percentile5" %in% input$dailyStat){
      plot <- plot %>%  add_lines(data=plot.data,x= ~Date,y=~Percentile5,name="5th Percentile",line=list(color='rgba(191,129,45, 1)',width=2))}
    if ("Minimum" %in% input$dailyStat){
      plot <- plot %>%  add_lines(data=plot.data,x= ~Date,y=~Minimum,name="Minimum",line=list(color='rgba(140,81,10, 1)',width=2))}
    
    # Add data from specific years
    plot <- plot %>% add_lines(data=dailyYears() %>% filter(Parameter==input$dailyParam),x= ~Date,y= ~Value, color=~Year)
    
    plot
    
  })
  
  
  #Download data buttons
  output$download.dailySummaryData <- downloadHandler(
    filename = function() {paste0(metaData()[1,2]," - daily summary.csv")},
    content = function(file) {
      write.csv(dailySummaryData(),file, row.names = FALSE, na="")
    })  
  
  
  ###################################################################################
  ### Real-Time Data ###
  ###################################################################################
  
  #Check if real-time data can be downloaded (some stations with realtime dont have data to download)
  output$rtExists <- reactive({
    if(is.null(realtime.HYDAT <- download_realtime_dd(STATION_NUMBER = values$station))){
      exist <- "No"
    } else {
      exist <- "Yes"
    }
    exist
  })
  
  # Place note in real-time panel section if no real-time data
  output$noRT <- renderText({
    if(metaData()[9,2]=="No") {
      paste("*** No real-time data available for this station.")
    } else if(metaData()[9,2]=="Yes" & is.null(realtime.HYDAT <- download_realtime_dd(STATION_NUMBER = values$station))) {
      paste("*** No real-time data available at this time.")
    }
    
  })
  
  # Extract real-time data from weblink and clip to dates
  realtimeData <- reactive({ 
    realtime.HYDAT <- download_realtime_dd(STATION_NUMBER = values$station)#values$station
    
    # Remove FLOW OR LEVEL if there either are all NA
    realtime.FLOW <- realtime.HYDAT %>% filter(Parameter=="FLOW")
    if ((realtime.FLOW %>% summarise(n=sum(!is.na(Value))))==0){
      realtime.FLOW <- realtime.FLOW[0,]
    }
    realtime.LEVEL <- realtime.HYDAT %>% filter(Parameter=="LEVEL")
    if ((realtime.LEVEL %>% summarise(n=sum(!is.na(Value))))==0){
      realtime.LEVEL <- realtime.LEVEL[0,]
    }
    
    realtime.data <- rbind(realtime.FLOW,realtime.LEVEL)
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
  
  #Create and render the real time plot
  output$rtplot <- renderPlotly({
    
    if (length(input$rtParam)==2) { #If both flow and water level
      plot <- plot_ly() %>% 
        add_lines(data=realtimeData() %>% filter(Parameter=="FLOW"),x= ~Date,y= ~Value, name="Discharge") %>% 
        add_lines(data=realtimeData() %>% filter(Parameter=="LEVEL"),x= ~Date,y= ~Value, name="Water Level", yaxis = "y2") %>% 
        layout(xaxis=list(title="Date"),
               yaxis=rtplot.y(),
               yaxis2 = list(overlaying = "y",side = "right",title = "Water Level (m)"))}
    else if (length(input$rtParam)==1 & input$rtParam=="FLOW") { #if just flow data
      plot <- plot_ly() %>% 
        add_lines(data=realtimeData() %>% filter(Parameter=="FLOW"),x= ~Date,y= ~Value, name="Discharge") %>% 
        layout(xaxis=list(title="Date"),
               yaxis=rtplot.y(),
               showlegend = TRUE)} 
    else if (length(input$rtParam)==1 & input$rtParam=="LEVEL") { # if just level data
      plot <- plot_ly() %>% 
        add_lines(data=realtimeData() %>% filter(Parameter=="LEVEL"),x= ~Date,y= ~Value, name="Water Level") %>% 
        layout(xaxis=list(title="Date"),
               yaxis=list(title = "Water Level (m)"),
               showlegend = TRUE)} 
    
    plot
  })
  
  # Download button for data
  output$download.rtData <- downloadHandler(
    filename = function() {paste0(metaData()[1,2]," - real-time discharge.csv")},
    content = function(file) {
      write.csv(realtimeData(),file, row.names = FALSE, na="")
    })  
  
  
  ######################################################################
  ### Under Development  ###
  #####################################################################
  
  output$mapDA <- renderUI({
    sliderInput("mapDA", label = "Drainage area (sqkm):", 
                min = min(stations$DRAINAGE_AREA_GROSS,na.rm = T), 
                max = max(stations$DRAINAGE_AREA_GROSS,na.rm = T), 
                value = c(min(stations$DRAINAGE_AREA_GROSS,na.rm = T), max(stations$DRAINAGE_AREA_GROSS,na.rm = T)))
  })
  
  
  # Deal with later
  #observeEvent(input$downloadHYDAT, {
  #  download_hydat(dl_hydat_here = getwd())
  #})
  
  ######################################################################
  ######################################################################
  
  
}


shinyApp(ui = ui, server = server)