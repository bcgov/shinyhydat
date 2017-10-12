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



### Shiny App Script ###
########################

library(shiny) # 1.0.3 shiny
library(shinydashboard)
library(dplyr) ## >0.7.0 dplyr
library(tidyr)
library(leaflet)
library(tidyhydat)
library(plotly)


### Set path to HYDAT
HYDAT.path <- "Hydat.sqlite3" 

## Create a dataframe of all station metadata and a list of all stations
stations <- STATIONS(HYDAT.path,
                     PROV_TERR_STATE_LOC = "BC") %>%  #c("AB","BC","SK","MB","ON","QC","NB","NS","PE","NL","YT","NT","NU")
  left_join(AGENCY_LIST(HYDAT.path), by = c("CONTRIBUTOR_ID" = "AGENCY_ID")) %>% rename("CONTRIBUTOR"=AGENCY_EN) %>% 
  left_join(AGENCY_LIST(HYDAT.path), by = c("OPERATOR_ID" = "AGENCY_ID")) %>%  rename("OPERATOR"=AGENCY_EN) %>% 
  left_join(DATUM_LIST(HYDAT.path), by = c("DATUM_ID" = "DATUM_ID")) %>% rename("DATUM"=DATUM_EN) %>% 
  mutate(REGIONAL_OFFICE_ID = as.integer(REGIONAL_OFFICE_ID)) %>% 
  left_join(REGIONAL_OFFICE_LIST(HYDAT.path), by = c("REGIONAL_OFFICE_ID" = "REGIONAL_OFFICE_ID")) %>% rename("REGIONAL_OFFICE"=REGIONAL_OFFICE_NAME_EN) %>% 
  left_join(STN_REGULATION(HYDAT.path), by="STATION_NUMBER") %>% 
  select(STATION_NUMBER,STATION_NAME,PROV_TERR_STATE_LOC,HYD_STATUS,LATITUDE,LONGITUDE,DRAINAGE_AREA_GROSS,RHBN,REAL_TIME,REGULATED,CONTRIBUTOR,OPERATOR,REGIONAL_OFFICE,DATUM)
stations.list <- as.list(stations$STATION_NUMBER)



# Set up the user-interface
ui <- dashboardPage(
  dashboardHeader(title="HYDAT Data Viewer"),
  dashboardSidebar(
    fluidPage(
      br(),
      uiOutput("stnSelect"),
      hr(),
      h5("About:"),
      h5("This app extracts hydrometric discharge and water level data from the HYDAT database and displays station metadata, historical data, and real-time data, if available. A locally saved SQLite HYDAT database file is required."),
      br(),
      h4("HYDAT versions:"),
      textOutput("localHYDAT"),
      textOutput("onlineHYDAT")#,
      #actionButton("downloadHYDAT","Download HYDAT")  #NEED TO MOVE QUESTION FROM CONSOLE TO WINDOW
    )
    
    
  ),
  dashboardBody(
    fluidPage(
      tabBox("TITLE",width = 12,
             tabPanel("Station Listings",
                      fluidRow(column(width = 8,
                                      helpText("Search for a station by entering all or part of a station name, number, or other categories. To view station information and hydrometric data, click on the row and view the other tabs. To search by map, go to the 'Stations Map' tab and click on the marker of your desired station. Each map or table selection will replace the previous selection."),
                                      helpText("The table below (filtered or not) can be downloaded as a .csv file with the download button the right. To display stations listed below (filtered or not) on the 'Stations Map' tab, click the button to the right. To clear any filters on the map, clear all filters in the table and re-click the button.")),
                               column(width=2,
                                      downloadButton('download.stations', 'Download Filtered Table'),br(),br(),
                                      actionButton('stationsMapAdd', 'Show Filtered Stations on Map')
                               )),
                      br(),
                      DT::dataTableOutput("allstationsTable")
                      
             ),
             tabPanel("Stations Map",
                      br(),
                      fluidPage(column(width=8,box(width=12,background="light-blue",
                                                   tags$style(type = "text/css", "#map {height: calc(100vh - 170px) !important;}"),
                                                   leafletOutput("map"))),
                                column(width=4,
                                       box(width=12,title="Station Information",status = "primary",solidHeader = TRUE,
                                           tableOutput("metaTable"))))
             ),
             tabPanel("Station Info",
                      br(),
                      h4("Station Information"),
                      fluidRow(column(width = 5),#tableOutput("metaTable")),
                               column(width = 7,box(width=12,background="light-blue",
                                                    leafletOutput("stnmap")
                               )))),
             tabPanel("Historical Data",
                      fluidRow(column(width=4,selectInput("histView",label="Historical data type to view:",choices = list("Long-term","Annual","Monthly", "Daily"))),
                               column(width=8,uiOutput("histYears"))),
                      fluidPage(tags$hr()),
                      # Historical Long-term
                      conditionalPanel(
                        condition = "input.histView == 'Long-term'",
                        fluidRow(column(3, downloadButton('download.ltData', 'Download Data'))),
                        br(),
                        fluidRow(
                          tabBox(width = 12,
                                 tabPanel("Graph",
                                          fluidRow(column(12,h4(textOutput("ltplot.title")))),
                                          fluidRow(column(12,plotlyOutput('ltplot'))),
                                          br(),br(),
                                          fluidRow(box(width = 6,title = "Graph Options",status = "primary",
                                                       fluidRow(column(5, uiOutput("ltParam"),
                                                                       checkboxInput("ltlog", label = "Log scale on primary Y-axis", value= FALSE)),
                                                                column(2),
                                                                column(5, uiOutput("paramSymbol")))
                                          ))
                                          
                                 ),
                                 tabPanel("Table",
                                          fluidRow(column(width=9,DT::dataTableOutput("ltTable")),
                                                   column(width=3,br(),br(),br(),h4("Symbols"),h5("E = Estimate"),h5("A = Partial Day"),
                                                          h5("B = Ice conditions"),h5("D = Dry"),h5("R = Revised"))
                                          )
                                 )
                          )
                        )),
                      # Historical Annual
                      conditionalPanel(
                        condition = "input.histView == 'Annual'",
                        fluidRow(column(3, downloadButton('download.annualData', 'Download Data'))),
                        br(),
                        fluidRow(
                          tabBox(width = 12,
                                 tabPanel("Graph",
                                          fluidRow(column(12,h4(textOutput("annualPlot.title")))),
                                          fluidRow(column(12,plotlyOutput('annualPlot'))),
                                          h5("* Mean values produced only for years of complete data"),
                                          br(),br(),
                                          fluidRow(box(width = 6,title = "Graph Options",status = "primary",
                                                       fluidRow(column(5,
                                                                       uiOutput("annualParam"), 
                                                                       checkboxInput("annuallog", label = "Log scale on primary Y-axis", value= FALSE)),
                                                                column(2),
                                                                column(5,uiOutput("annualStat"),
                                                                       uiOutput("annualInstantStat")))
                                          ))
                                 ),
                                 tabPanel("Table")
                          )
                        )
                      ),
                      # Historical Monthly
                      conditionalPanel(
                        condition = "input.histView == 'Monthly'",
                        fluidRow(column(3, downloadButton('download.monthData', 'Download Data'))),
                        br(),
                        fluidRow(
                          tabBox(width = 12,
                                 tabPanel("Graph",
                                          fluidRow(column(12,h4(textOutput("monthPlot.title")))),
                                          fluidRow(column(12,plotlyOutput('monthPlot'))),
                                          h5("* Missing dates ignored"),
                                          br(),br(),
                                          fluidRow(box(width = 9,title = "Graph Options",status = "primary",
                                                       fluidRow(column(3,uiOutput("monthParam"),
                                                                       checkboxInput("monthlog", label = "Log scale on primary Y-axis", value= FALSE)),
                                                                column(1),
                                                                column(4,
                                                                       uiOutput("monthStat"),
                                                                       checkboxInput("monthMaxMin","Display Maximum-Minimum Range",value=TRUE),
                                                                       checkboxInput("month90","Display Upper-Lower Quartile range",value=TRUE),
                                                                       checkboxInput("month50","Display Upper-Lower Quartile range",value=TRUE)),
                                                                column(1),
                                                                column(3,uiOutput("monthYear"),
                                                                       uiOutput("monthYearStat")))))
                                 ),
                                 tabPanel("Table",
                                          h5("*** ADD TABLE AND YEAR OF MAX VALUE and LIST THE YEARS INCLUDED"),
                                          h5("*** ADD if statements plot code to specify colours")
                                 )
                          )
                        )
                      ),
                      
                      conditionalPanel(
                        condition = "input.histView == 'Daily'",
                        fluidRow(column(3, downloadButton('download.dailySummaryData', 'Download Data'))),
                        br(),
                        fluidRow(
                          tabBox(width = 12,
                                 tabPanel("Graph",
                                          fluidRow(column(12,h4(textOutput("dailyPlot.title")))),
                                          fluidRow(column(12,plotlyOutput('dailyPlot'))),
                                          br(),br(),
                                          fluidRow(box(width = 9,title = "Graph Options",status = "primary",
                                                       fluidRow(column(3,uiOutput("dailyParam"),
                                                                       checkboxInput("dailylog", label = "Log scale on primary Y-axis", value= FALSE)),
                                                                column(1),
                                                                column(4,
                                                                       uiOutput("dailyStat"),
                                                                       checkboxInput("dailyMaxMin","Display Maximum-Minimum Range",value=TRUE),
                                                                       checkboxInput("daily90","Display 5-95 percentile range",value=TRUE),
                                                                       checkboxInput("daily50","Display 25-75 percentile range",value=TRUE)),
                                                                column(1),
                                                                column(3,uiOutput("dailyYear")))
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
                        fluidRow(column(3, checkboxInput("rtlog", label = "Plot Discharge axis on log scale", value= FALSE)),
                                 column(3, downloadButton('download.rtData', 'Download Data'))),
                        fluidRow(column(12,h4(textOutput("rtplot.title")))),
                        fluidRow(column(12,plotlyOutput('rtplot'))),
                        fluidRow(column(12,uiOutput("rtParam"))))),
             tabPanel("Station Comparison")
      )
    )
  )
)

######################################################################################################
######################################################################################################

# Set up the server (where all the magic happens)
server <- function(input, output, session) {
  
  #### HYDAT VERSION
  
  output$onlineHYDAT <- renderText({
    paste0("Available: ",as.Date(substr(gsub("^.*\\Hydat_sqlite3_","",RCurl::getURL("http://collaboration.cmc.ec.gc.ca/cmc/hydrometrics/www/")), 1,8), "%Y%m%d"))
    
  })
  output$localHYDAT <- renderText({
    paste0("Local: ",as.Date(as.data.frame(VERSION(HYDAT.path))[,2]))
    
  })
  
  
  
  ### Select station ###
  ######################
  output$stnSelect <- renderUI({
    selectizeInput("station", label = "Select or type your hydrometric station ID number:",choices = stations.list,options = list(placeholder ="type station ID number",maxOptions = 2420 ), selected=allstationsTable()[input$allstationsTable_rows_selected,1])
  })
  
  
  ### MetaData ###
  ################
  
  # Extract station metadata from HYDAT
  metaData <- reactive({
    
    stn.meta.HYDAT <- stations %>% filter(STATION_NUMBER==input$station)#input$station)
    
    stn.info <- stn.meta.HYDAT %>% 
      mutate("Historical Data Link"=paste0("https://wateroffice.ec.gc.ca/report/historical_e.html?stn=",STATION_NUMBER),
             "Real-Time Data Link"=ifelse(REAL_TIME==TRUE, paste0("https://wateroffice.ec.gc.ca/report/real_time_e.html?stn=",STATION_NUMBER),"No real-time data available."),
             DRAINAGE_AREA_GROSS=round(DRAINAGE_AREA_GROSS,2),
             LATITUDE=round(LATITUDE,6),
             LONGITUDE=round(LONGITUDE,6)) %>% 
      rename("Station Number"=STATION_NUMBER,"Station Name" =STATION_NAME,"Prov/Terr/State"=PROV_TERR_STATE_LOC,"Station Status"=HYD_STATUS,
             "Latitude"=LATITUDE,"Longitude"=LONGITUDE,"Drainage Area (sq km)"=DRAINAGE_AREA_GROSS,"Reference (RHBN)"=RHBN,"Real-Time"=REAL_TIME,
             "Regulation"=REGULATED,"Regional Office"=REGIONAL_OFFICE,"Contributor"=CONTRIBUTOR,"Operator"=OPERATOR,"Datum"=DATUM) %>% 
      gather("header","content",1:16)
    
    stn.info[is.na(stn.info)] <- ""
    stn.info
  })
  
  # Rander table for output
  output$metaTable <- renderTable(metaData(),colnames = FALSE)
  
  
  output$stnmap <- renderLeaflet({
    leaflet(stations) %>% addTiles() %>%
      setView(lng = as.numeric(metaData()[6,2]), lat = as.numeric(metaData()[5,2]), zoom = 9) %>% # set centre and extent of map
      addCircleMarkers(data = filter(stations, STATION_NUMBER %in% input$station), ~LONGITUDE, ~LATITUDE, color = "red", radius = 6) %>%
      addCircles(lng = ~LONGITUDE, lat = ~LATITUDE, weight = 1,
                 radius = 1, label = ~STATION_NAME, 
                 popup = ~paste(STATION_NAME, "<br>",
                                STATION_NUMBER, "<br>",
                                "DRAINAGE AREA = ",DRAINAGE_AREA_GROSS, "SQ. KM","<br>")
      )
    
  })
  
  
  ###################################################################################
  ### Historical Data ###
  ###################################################################################
  
  dailyData <- reactive({
    check <- STN_DATA_RANGE(HYDAT.path, STATION_NUMBER=input$station) %>% filter(DATA_TYPE=="Q"|DATA_TYPE=="H")
    
    if ("Q" %in% check$DATA_TYPE & "H" %in% check$DATA_TYPE) { # both Q and H
      daily.flow.HYDAT <- DLY_FLOWS(hydat_path = HYDAT.path, STATION_NUMBER=input$station)
      daily.levels.HYDAT <- DLY_LEVELS(hydat_path = HYDAT.path, STATION_NUMBER=input$station)
      daily.data <- rbind(daily.flow.HYDAT[,c(2:5)],daily.levels.HYDAT[,c(2:5)])
    } else if ("Q" %in% check$DATA_TYPE & !("H" %in% check$DATA_TYPE)) { # just Q
      daily.flow.HYDAT <- DLY_FLOWS(hydat_path = HYDAT.path, STATION_NUMBER=input$station)
      daily.data <- daily.flow.HYDAT[,c(2:5)]
    } else if (!("Q" %in% check$DATA_TYPE) & "H" %in% check$DATA_TYPE) { # just H
      daily.levels.HYDAT <- DLY_LEVELS(hydat_path = HYDAT.path, STATION_NUMBER=input$station)
      daily.data <- daily.levels.HYDAT[,c(2:5)]
    }
    
    daily.data1 <- as.data.frame(daily.data[0,])
    for (parameter in unique(daily.data$Parameter)) {
      daily.data.param <- daily.data %>% filter(Parameter==parameter)
      min.date <- as.Date((paste((as.numeric(format(min(daily.data.param$Date),'%Y'))),01,01,sep="-")),"%Y-%m-%d")
      max.date <- as.Date((paste((as.numeric(format(max(daily.data.param$Date),'%Y'))),12,31,sep="-")),"%Y-%m-%d")
      data.empty <- data.frame(Date=seq(min.date, max.date, by="days"))
      data.temp <- merge(data.empty,daily.data.param,by="Date",all = TRUE)
      data.temp$Parameter <- parameter
      daily.data1 <- as.data.frame(rbind(daily.data1,data.temp))
    }
    
    daily.data <- daily.data1
  })
  
  histDates <- reactive({
    daily.flow.dates <- dailyData() %>% 
      summarize(minDate=as.numeric(format(min(Date),'%Y')),
                maxDate=as.numeric(format(max(Date),'%Y')))
  })
  
  
  ### Historical Long-term Data
  ###################################################################################
  
  
  output$histYears <- renderUI({
    sliderInput("histYears",label="Filter data between the following years:",min=histDates()$minDate,max=histDates()$maxDate,value=c(histDates()$minDate,histDates()$maxDate),sep = "")
  })
  output$paramSymbol <- renderUI({
    selectizeInput("paramSymbol", label = "Add data symbols:",choices = as.list(unique(dailyData()$Parameter)),multiple =TRUE)
  })
  
  output$ltplot.title <- renderText({
    paste0("Daily Data - ",metaData()[2,2]," (",metaData()[1,2],")")
  })
  
  output$ltParam <- renderUI({
    selectizeInput("ltParam","Display parameters:",choices=as.list(unique(dailyData()$Parameter)),selected=as.list(unique(dailyData()$Parameter)), multiple =TRUE)
  })
  
  ltplot.y <- reactive({
    if (input$ltlog) {list(title= "Discharge (cms)",type= "log")}
    else {            list(title= "Discharge (cms)")}
  })
  
  output$ltplot <- renderPlotly({
    plot.data <- dailyData() %>% 
      mutate(Symbol=replace(Symbol, Symbol=="E", "Estimate"),
             Symbol=replace(Symbol, Symbol=="A", "Partial Day"),
             Symbol=replace(Symbol, Symbol=="B", "Ice conditions"),
             Symbol=replace(Symbol, Symbol=="D", "Dry"),
             Symbol=replace(Symbol, Symbol=="R", "Revised"),
             Year=as.numeric(format(Date,'%Y'))) %>% 
      filter(Year >= input$histYears[1] & Year <= input$histYears[2])
    
    
    if (length(input$ltParam)==2) {
      plot <-  plot_ly() %>%
        add_lines(data=plot.data %>% filter(Parameter=="FLOW"),x= ~Date,y= ~Value, name="Discharge") %>%
        add_lines(data=plot.data %>% filter(Parameter=="LEVEL"),x= ~Date,y= ~Value, name="Water Level", yaxis = "y2") %>%
        layout(xaxis=list(title="Date"),
               yaxis=ltplot.y(),
               yaxis2 = list(overlaying = "y",side = "right",title = "Water Level (m)"))
      
    } else if (length(input$ltParam)==1 & input$ltParam=="FLOW") {
      plot <- plot_ly() %>%
        add_lines(data=plot.data %>% filter(Parameter=="FLOW"),x= ~Date,y= ~Value, name="Discharge",text=~Symbol) %>%
        layout(xaxis=list(title="Date"),
               yaxis=ltplot.y(),
               showlegend = TRUE)
    } else if (length(input$ltParam)==1 & input$ltParam=="LEVEL") {
      plot <-plot_ly() %>%
        add_lines(data=plot.data %>% filter(Parameter=="LEVEL"),x= ~Date,y= ~Value, name="Water Level") %>%
        layout(xaxis=list(title="Date"),
               yaxis=list(title = "Water Level (m)"),
               showlegend = TRUE)
    }
    
    # Add data symbol to plot
    if (length(input$paramSymbol)>0) {plot <- plot %>% add_markers(data=plot.data %>% filter(Parameter %in% input$paramSymbol),x= ~Date,y= 0, color= ~Symbol)}
    
    plot
  })
  
  ltTableOutput <- reactive({
    data <- dailyData() %>% mutate(Value=round(Value,3),
                                   Year=as.integer(format(Date,'%Y')),
                                   Month=as.character(format(Date,'%B'))) %>%
      select(Date,Year,Month,Parameter,Value,Symbol)
    
    
    check <- STN_DATA_RANGE(HYDAT.path, STATION_NUMBER=input$station) %>% filter(DATA_TYPE=="Q"|DATA_TYPE=="H")
    if ("Q" %in% check$DATA_TYPE & "H" %in% check$DATA_TYPE) { # both Q and H
      data2 <- data %>% filter(Parameter=="FLOW") %>% mutate("Flow (cms)"=Value,"Flow Symbol"=Symbol)%>% select(-Parameter,-Value,-Symbol)
      data3 <- data %>% filter(Parameter=="LEVEL") %>% mutate("Water Level (m)"=Value,"Water Level Symbol"=Symbol)%>% select(-Parameter,-Value,-Symbol)
      data2 <- merge(data2,data3,by=c("Date","Year","Month"),all=TRUE) 
    } else if ("Q" %in% check$DATA_TYPE & !("H" %in% check$DATA_TYPE)) { # just Q
      data2 <- data %>% rename("Flow (cms)"=Value,"Flow Symbol"=Symbol) %>% select(-Parameter)
    } else if (!("Q" %in% check$DATA_TYPE) & "H" %in% check$DATA_TYPE) { # just H
      data2 <- data %>% rename("Water Level (m)"=Value,"Water Level Symbol"=Symbol) %>% select(-Parameter)
    }
    data2
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
  
  annualData <- reactive({
    
    annual <- ANNUAL_STATISTICS(hydat_path = HYDAT.path, STATION_NUMBER=input$station) %>% 
      filter(Parameter=="Flow" | Parameter == "Water Level")
    
    # Fill in missing years
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
      all.years$STATION_NUMBER <- input$station
      all.years$Symbol[is.na(all.years$Symbol)] <- ""
      annual.data <- rbind(annual.data,all.years)
    }
    annual.data <- annual.data %>% 
      filter(Year >= input$histYears[1] & Year <= input$histYears[2])
    annual.data 
  })
  
  annualInstantData <- reactive({
    annual.instant <- ANNUAL_INSTANT_PEAKS(hydat_path = HYDAT.path, STATION_NUMBER=input$station)
    
    annual.instant <- annual.instant %>% 
      mutate(Date=as.Date(paste(YEAR,MONTH,DAY,sep="-"),format="%Y-%m-%d"),
             Time=paste0(HOUR,":",ifelse(nchar(MINUTE)>1,paste(MINUTE),paste0(0,MINUTE))," ",TIME_ZONE),
             DateTime=paste0("On ",Date," at ",Time),
             Symbol=replace(Symbol, is.na(Symbol), "")) %>% 
      filter(YEAR >= input$histYears[1] & YEAR <= input$histYears[2])
    annual.instant
    
  })
  
  annualStatsList <- reactive({
    c("Mean"="MEAN","Maximum"="MAX","Minimum"="MIN")
  })
  output$annualStat <- renderUI({
    selectizeInput("annualStat", label = "Display annual statistic(s):",choices = annualStatsList(),selected = annualStatsList(),multiple =TRUE)
  })
  
  output$annualInstantStat <- renderUI({
    selectizeInput("annualInstantStat", label = "Display annual instantaneous extreme values:",choices = as.list(unique(annualInstantData()[annualInstantData()$Parameter == input$annualParam,]$PEAK_CODE)),multiple =TRUE)
  })
  
  output$annualParam <- renderUI({
    selectInput("annualParam", label = "Display parameters:",choices = as.list(unique(annualData()$Parameter)))
  })
  
  output$annualPlot.title <- renderText({
    paste0("Annual ",input$annualParam," Statistics - ",metaData()[2,2]," (",metaData()[1,2],")")
  })
  
  
  annualPlot.y <- reactive({
    if (input$annuallog) {list(title=ifelse(input$annualParam== "Flow","Discharge (cms)","Water Level (m)"),type= "log")}
    else {                list(title=ifelse(input$annualParam== "Flow","Discharge (cms)","Water Level (m)"))}
  })
  
  output$annualPlot <- renderPlotly({
    
    plot.data <- annualData() %>% filter(Parameter==input$annualParam)
      
    plot <- plot_ly() %>% 
      layout(xaxis=list(title="Year"),
             yaxis=annualPlot.y())
    
    # Add annual statistic
    if ("MAX" %in% input$annualStat){plot <- plot %>%  add_trace(data=plot.data %>% filter(Sum_stat=="MAX"),x= ~Year,y=~Value,name="Daily Maximum",color=I("red"),mode = 'lines+markers',text=~paste("On",Date," ",Symbol))}
    if ("MEAN" %in% input$annualStat){plot <- plot %>%  add_trace(data=plot.data %>% filter(Sum_stat=="MEAN"),x= ~Year,y=~Value,name="Daily Mean",color=I("blue"),mode = 'lines+markers')}
    if ("MIN" %in% input$annualStat){plot <- plot %>%  add_trace(data=plot.data %>% filter(Sum_stat=="MIN"),x= ~Year,y=~Value,name="Daily Minimum",color=I("green"),mode = 'lines+markers',text=~paste("On",Date," ",Symbol))}
    
    # Add instantaneous peaks
    if ("MAX" %in% input$annualInstantStat){plot <- plot %>%  add_markers(data=annualInstantData() %>% filter(Parameter==input$annualParam & PEAK_CODE == "MAX"),x= ~YEAR,y= ~Value, name="Instanteous Maximum",marker=list(symbol=2,size=8), text=~paste0(DateTime," ",Symbol))} 
    if ("MIN" %in% input$annualInstantStat){plot <- plot %>%  add_markers(data=annualInstantData() %>% filter(Parameter==input$annualParam & PEAK_CODE == "MIN"),x= ~YEAR,y= ~Value, name="Instanteous Minimum",marker=list(symbol=2,size=8), text=~paste0(DateTime," ",Symbol))}
    
    plot
  })
  
  #Download plot/data buttons
  output$download.annualData <- downloadHandler(
    filename = function() {paste0(metaData()[1,2]," - annual summary.csv")},
    content = function(file) {
      write.csv(annualData(),file, row.names = FALSE, na="")
    })  
  
  
  
  
  
  ### Historical Monthly Data
  ###################################################################################
  
  
  # Calculate annual data and render for printing
  monthData <- reactive({
    daily.data <- dailyData()
    
    daily.data$Year <- as.numeric(format(daily.data$Date,'%Y'))
    daily.data$Month <- as.integer(format(daily.data$Date,'%m'))
    
    # FIlter data for select years
    daily.data <- daily.data %>% 
      filter(Year >= input$histYears[1] & Year <= input$histYears[2])
    
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
  
  allmonthData <- reactive({
    check <- STN_DATA_RANGE(HYDAT.path, STATION_NUMBER=input$station) %>% filter(DATA_TYPE=="Q"|DATA_TYPE=="H")
    
    if ("Q" %in% check$DATA_TYPE & "H" %in% check$DATA_TYPE) { # both Q and H
      monthly.flows.hydat <- MONTHLY_FLOWS(hydat_path = HYDAT.path, STATION_NUMBER=input$station) %>% 
        mutate(Parameter="FLOW")
      monthly.levels.hydat <- MONTHLY_LEVELS(hydat_path = HYDAT.path, STATION_NUMBER=input$station) %>% 
        select(-PRECISION_CODE) %>% mutate(Parameter="LEVEL")
      monthly.data <- rbind(monthly.flows.hydat,monthly.levels.hydat) %>% 
        select(Parameter,YEAR,MONTH,Sum_stat,Value,Date_occurred)
    } else if ("Q" %in% check$DATA_TYPE & !("H" %in% check$DATA_TYPE)) { # just Q
      monthly.flows.hydat <- MONTHLY_FLOWS(hydat_path = HYDAT.path, STATION_NUMBER=input$station) %>% 
        mutate(Parameter="FLOW")
      monthly.data <- monthly.flows.hydat %>% 
        select(Parameter,YEAR,MONTH,Sum_stat,Value,Date_occurred)
    } else if (!("Q" %in% check$DATA_TYPE) & "H" %in% check$DATA_TYPE) { # just H
      monthly.levels.hydat <- MONTHLY_LEVELS(hydat_path = HYDAT.path, STATION_NUMBER=input$station) %>% 
        select(-PRECISION_CODE) %>% mutate(Parameter="LEVEL")
      monthly.data <- monthly.levels.hydat %>% 
        select(Parameter,YEAR,MONTH,Sum_stat,Value,Date_occurred)
    }
    
    monthly.data <- as.data.frame(monthly.data) %>% 
      mutate(YEAR = as.factor(YEAR))
    monthly.data
  })
  
  
  output$monthParam <- renderUI({
    selectInput("monthParam", label = "Display parameter:",choices = as.list(unique(monthData()$Parameter)))
  })
  
  output$monthYear <- renderUI({
    selectizeInput("monthYear", label = "Select year(s) to display:",choices = as.list(unique(allmonthData()[allmonthData()$Parameter == input$monthParam,]$YEAR)),multiple =TRUE)
  })
  
  output$monthYearStat <- renderUI({
    selectizeInput("monthYearStat", label = "Select monthly statistic(s):",choices = as.list(unique(allmonthData()[allmonthData()$Parameter == input$monthParam,]$Sum_stat)),multiple =TRUE)
  })
  
  monthlyStatsList <- reactive({
    c("Mean"="Mean","Maximum"="Maximum","Minimum"="Minimum","Median"="Median","75th Percentile"="Percentile75","25th Percentile"="Percentile25", "95th Percentile"="Percentile95", "5th Percentile"="Percentile5")
  })

  output$monthStat <- renderUI({
    selectizeInput("monthStat", label = "Select monthly statistic(s):",choices = monthlyStatsList(),multiple =TRUE,selected = c("Mean","Median"))
  })
  
  output$monthPlot.title <- renderText({
    paste0("Monthly ",ifelse(input$monthParam=="FLOW",paste("Flow"),paste("Water Level"))," Statistics - ",metaData()[2,2]," (",metaData()[1,2],")")
  })
  
  monthlyPlot.y <- reactive({
    if (input$monthlog) {list(title=ifelse(input$monthParam== "FLOW","Discharge (cms)","Water Level (m)"),type= "log")}
    else {               list(title=ifelse(input$monthParam== "FLOW","Discharge (cms)","Water Level (m)"))}
  })
  
  output$monthPlot <- renderPlotly({
    
    plot.data <- monthData() %>% spread(Stat,Value) %>% filter(Parameter==input$monthParam)
    
    plot <- plot_ly() %>% 
      layout(xaxis=list(title="Month",tickvals = seq(1:12),ticktext = c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Noc","Dec")),
             yaxis=monthlyPlot.y())
    
    # Add ribbons if checked
    if (input$monthMaxMin){plot <- plot %>%  add_ribbons(data=plot.data,x= ~Month,ymin= ~Minimum, ymax= ~Maximum,name="Max-Min Range",color=I("lightblue2"))}
    if (input$month90){plot <- plot %>%  add_ribbons(data=plot.data,x= ~Month,ymin= ~Percentile5, ymax= ~Percentile95,name="90% of Flows",color=I("lightblue4"))}
    if (input$month50){plot <- plot %>%  add_ribbons(data=plot.data,x= ~Month,ymin= ~Percentile25, ymax= ~Percentile75,name="50% of Flows",color=I("lightblue4"))}
    # Add lines if selected
    if ("Maximum" %in% input$monthStat){plot <- plot %>%  add_trace(data=plot.data,x= ~Month,y=~Maximum,name="Maximum",color=I("red"),mode = 'lines+markers')}
    if ("Percentile95" %in% input$monthStat){plot <- plot %>%  add_trace(data=plot.data,x= ~Month,y=~Percentile95,name="95th Percentile",color=I("yellow"),mode = 'lines+markers')}
    if ("Percentile75" %in% input$monthStat){plot <- plot %>%  add_trace(data=plot.data,x= ~Month,y=~Percentile75,name="75th Percentile",color=I("orange"),mode = 'lines+markers')}
    if ("Mean" %in% input$monthStat){plot <- plot %>%  add_trace(data=plot.data,x= ~Month,y=~Mean,name="Mean",color=I("green"),mode = 'lines+markers')}
    if ("Median" %in% input$monthStat){plot <- plot %>%  add_trace(data=plot.data,x= ~Month,y=~Median,name="Median",color=I("blue"),mode = 'lines+markers')}
    if ("Percentile25" %in% input$monthStat){plot <- plot %>%  add_trace(data=plot.data,x= ~Month,y=~Percentile25,name="25th Percentile",color=I("brown"),mode = 'lines+markers')}
    if ("Percentile5" %in% input$monthStat){plot <- plot %>%  add_trace(data=plot.data,x= ~Month,y=~Percentile5,name="5th Percentile",color=I("black"),mode = 'lines+markers')}
    if ("Minimum" %in% input$monthStat){plot <- plot %>%  add_trace(data=plot.data,x= ~Month,y=~Minimum,name="Minimum",color=I("purple"),mode = 'lines+markers')}

    
    # Add data from specific years and specific stats    
    plot <- plot %>% add_trace(data=allmonthData() %>% filter(Parameter==input$monthParam & Sum_stat %in% input$monthYearStat & YEAR %in% input$monthYear),x= ~MONTH,y= ~Value, color=~paste0(YEAR," ",Sum_stat),mode = 'lines+markers')#name= ~paste0(input$monthYearStat),
    
    plot
    


    
  })
  
  
  
  #Download plot/data buttons
  output$download.monthData <- downloadHandler(
    filename = function() {paste0(metaData()[1,2]," - monthly summary.csv")},
    content = function(file) {
      write.csv(monthData(),file, row.names = FALSE, na="")
    })  
  
  
  
  ### Historical Daily Data
  ###################################################################################
  
  # Calculate annual data and render for printing
  dailySummaryData <- reactive({
    daily.data <- dailyData()
    daily.data$Year <- as.numeric(format(daily.data$Date,'%Y'))
    daily.data$Day <- as.integer(format(daily.data$Date,'%j'))
    
    # FIlter data for select years
    daily.data <- daily.data %>% 
      filter(Year >= input$histYears[1] & Year <= input$histYears[2])
    
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
      mutate(Day=as.Date(Day,origin = "1899-12-31")) %>% 
      gather(Stat,Value,3:10)
    dailysummary
  })
  
  daily.yearslist <- reactive({
    data <- dailyData() %>% mutate(Year=as.factor(format(Date,'%Y'))) %>% 
      group_by(Parameter,Year) %>% summarise(n=sum(is.na(Value))) %>% 
      filter(n<365)
  }) 
  
  output$dailyYear <- renderUI({
    selectizeInput("dailyYear", label = "Select year(s) to display:",choices = as.list(unique(daily.yearslist()[daily.yearslist()$Parameter == input$dailyParam,]$Year)),multiple =TRUE)
  })
  
  dailyYears <- reactive({
    daily.data <- dailyData()
    daily.data$Year <- as.factor(format(daily.data$Date,'%Y'))
    daily.data$Day <- as.integer(format(daily.data$Date,'%j'))
    
    daily.Year <- daily.data %>% 
      filter(Year %in% input$dailyYear & Day<366) %>% 
      mutate(Day=as.Date(Day,origin = "1899-12-31"))
    daily.Year
  })
  
  
  output$dailyParam <- renderUI({
    selectInput("dailyParam", label = "Display parameter:",choices = as.list(unique(dailySummaryData()$Parameter)))
  })
  
  dailyStatsList <- reactive({
    c("Mean"="Mean","Maximum"="Maximum","Minimum"="Minimum","Median"="Median","75th Percentile"="Percentile75", "25th Percentile"="Percentile25", "95th Percentile"="Percentile95", "5th Percentile"="Percentile5")
  })
  
  output$dailyStat <- renderUI({
    selectizeInput("dailyStat", label = "Select daily statistic(s):",choices = dailyStatsList(),multiple =TRUE,
                   selected = c("Mean","Median"))
  })
  
  
  output$dailyPlot.title <- renderText({
    paste0("Daily ",ifelse(input$dailyParam=="FLOW",paste("Flow"),paste("Water Level"))," Statistics - ",metaData()[2,2]," (",metaData()[1,2],")")
  })
  
  dailyPlot.y <- reactive({
    if (input$dailylog) {list(title=ifelse(input$dailyParam== "FLOW","Discharge (cms)","Water Level (m)"),type= "log")} 
    else {               list(title=ifelse(input$dailyParam== "FLOW","Discharge (cms)","Water Level (m)"))}
  })
  
  output$dailyPlot <- renderPlotly({
    
    plot.data <- dailySummaryData() %>% spread(Stat,Value)%>% filter(Parameter==input$dailyParam)
    
    plot <- plot_ly() %>% 
      layout(xaxis=list(title="Day of Year",tickformat= "%b-%d"),
             yaxis=dailyPlot.y())
    
    
    # Add ribbons if checked
    if (input$dailyMaxMin){plot <- plot %>%  add_ribbons(data=plot.data,x= ~Day,ymin= ~Minimum, ymax= ~Maximum,name="Max-Min Range",color=I("lightblue2"))}
    if (input$daily90){plot <- plot %>%  add_ribbons(data=plot.data,x= ~Day,ymin= ~Percentile95, ymax= ~Percentile5,name="90% of Flows",color=I("lightblue4"))}
    if (input$daily50){plot <- plot %>%  add_ribbons(data=plot.data,x= ~Day,ymin= ~Percentile25, ymax= ~Percentile75,name="50% of Flows",color=I("lightblue4"))}
    # Add lines if selected
    if ("Maximum" %in% input$dailyStat){plot <- plot %>%  add_lines(data=plot.data,x= ~Day,y=~Maximum,name="Maximum",color=I("red"))}
    if ("Percentile95" %in% input$dailyStat){plot <- plot %>%  add_lines(data=plot.data,x= ~Day,y=~Percentile95,name="95th Percentile",color=I("yellow"))}
    if ("Percentile75" %in% input$dailyStat){plot <- plot %>%  add_lines(data=plot.data,x= ~Day,y=~Percentile75,name="75th Percentile",color=I("orange"))}
    if ("Mean" %in% input$dailyStat){plot <- plot %>%  add_lines(data=plot.data,x= ~Day,y=~Mean,name="Mean",color=I("green"))}
    if ("Median" %in% input$dailyStat){plot <- plot %>%  add_lines(data=plot.data,x= ~Day,y=~Median,name="Median",color=I("blue"))}
    if ("Percentile25" %in% input$dailyStat){plot <- plot %>%  add_lines(data=plot.data,x= ~Day,y=~Percentile25,name="25th Percentile",color=I("brown"))}
    if ("Percentile5" %in% input$dailyStat){plot <- plot %>%  add_lines(data=plot.data,x= ~Day,y=~Percentile5,name="5th Percentile",color=I("black"))}
    if ("Minimum" %in% input$dailyStat){plot <- plot %>%  add_lines(data=plot.data,x= ~Day,y=~Minimum,name="Minimum",color=I("purple"))}
    
    # Add data from specific years
    plot <- plot %>% add_lines(data=dailyYears() %>% filter(Parameter==input$dailyParam),x= ~Day,y= ~Value, color=~Year)
    
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
  
  output$rtExists <- reactive({
    if(is.null(realtime.HYDAT <- download_realtime_dd(STATION_NUMBER = input$station))){
      exist <- "No"
    } else {
      exist <- "Yes"
    }
    exist
  })
  
  # Place text in real-time sidepanel section if no real-time data
  output$noRT <- renderText({
    
    if(metaData()[9,2]=="No") {
      paste("*** No real-time data available for this station.")
    } else if(metaData()[9,2]=="Yes" & is.null(realtime.HYDAT <- download_realtime_dd(STATION_NUMBER = input$station))) {
      paste("*** No real-time data available at this time.")
    }
    
    
  })
  
  # Extract real-time data from webslink and clip to dates
  realtimeData <- reactive({ 
    realtime.HYDAT <- download_realtime_dd(STATION_NUMBER = input$station)#input$station
    
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
  
  output$rtParam <- renderUI({
    selectizeInput("rtParam","Parameters to plot:",choices=as.list(unique(realtimeData()$Parameter)),selected=as.list(unique(realtimeData()$Parameter)), multiple =TRUE)
  })
  
  
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
  
  
  output$rtplot.title <- renderText({
    paste0("Real-time Data - ",metaData()[2,2]," (",metaData()[1,2],")")
  })
  
  output$rtplot <- renderPlotly({
    
    if (length(input$rtParam)==2) {
      plot <- plot_ly() %>% 
        add_lines(data=realtimeData() %>% filter(Parameter=="FLOW"),x= ~Date,y= ~Value, name="Discharge") %>% 
        add_lines(data=realtimeData() %>% filter(Parameter=="LEVEL"),x= ~Date,y= ~Value, name="Water Level", yaxis = "y2") %>% 
        layout(xaxis=list(title="Date"),
               yaxis=rtplot.y(),
               yaxis2 = list(overlaying = "y",side = "right",title = "Water Level (m)"))
      
    } else if (length(input$rtParam)==1 & input$rtParam=="FLOW") {
      plot <- plot_ly() %>% 
        add_lines(data=realtimeData() %>% filter(Parameter=="FLOW"),x= ~Date,y= ~Value, name="Discharge") %>% 
        layout(xaxis=list(title="Date"),
               yaxis=rtplot.y(),
               showlegend = TRUE)
    } else if (length(input$rtParam)==1 & input$rtParam=="LEVEL") {
      plot <- plot_ly() %>% 
        add_lines(data=realtimeData() %>% filter(Parameter=="LEVEL"),x= ~Date,y= ~Value, name="Water Level") %>% 
        layout(xaxis=list(title="Date"),
               yaxis=list(title = "Water Level (m)"),
               showlegend = TRUE)
    } 
    
    
    
    
  })
  
  # Render ggplot for output (if not real time, dont plot)
  #output$rtplot <- renderPlot({
  #  if(metaData()[9,2]=="Yes") realtimePlot()
  #})
  
  
  
  
  ###############################################################################################
  ### Leaflet map ###
  ###############################################################################################
  
  output$map <- renderLeaflet({
    leaflet(stations) %>% addTiles() %>%
      #setView(lng = -125, lat = 54, zoom = 5) # set centre and extent of map
      addCircleMarkers(data= stations, lng = ~LONGITUDE, lat = ~LATITUDE, layerId = ~STATION_NUMBER, color = "blue", radius = 2,
                       label = ~paste0(STATION_NAME, " (",STATION_NUMBER,") - ",HYD_STATUS))
    
  })
  
  # Allows the selection of different stations without redrawing the map
  observe({
    leafletProxy("map") %>%
      removeMarker(layerId="selected") %>%
      addCircleMarkers(layerId="selected",data = filter(stations, STATION_NUMBER %in% input$station), ~LONGITUDE, ~LATITUDE, color = "green", radius = 6)
  })
  
  
  # Updates the selection station by clicking on the marker
  observeEvent(input$map_marker_click, { # update the location selectInput on map clicks
    updateSelectizeInput(session, "station", selected=input$map_marker_click$id)
  })
  
  
  observeEvent(input$stationsMapAdd, {
    leafletProxy("map") %>%
      clearMarkers() %>%
      clearControls() %>% 
      addCircleMarkers(data= downloadStationsList(), lng = ~LONGITUDE, lat = ~LATITUDE, layerId = ~STATION_NUMBER, color = "blue", radius = 2,
                       label = ~paste0(STATION_NAME, " (",STATION_NUMBER,") - ",HYD_STATUS))
  })
  
  ### Reactive Widgets ###
  ########################
  
  
  
  # Structure to download real-time CSV file, only works if opened in browser
  output$download.rtData <- downloadHandler(
    filename = function() {paste0(metaData()[1,2]," - real-time discharge.csv")},
    content = function(file) {
      write.csv(realtimeData(),file, row.names = FALSE, na="")
    })  
  
  
  
  
  allstationsTable <- reactive({
    
    stn.meta.HYDAT <- stations %>% filter(STATION_NUMBER==stations.list)#input$station)
    
    stn.info <-stn.meta.HYDAT[,c(1:4,7:13)] %>% 
      mutate(DRAINAGE_AREA_GROSS=round(DRAINAGE_AREA_GROSS,2)) %>% 
      rename("Station Number"=STATION_NUMBER,"Station Name" =STATION_NAME,"Prov/ Terr/ State"=PROV_TERR_STATE_LOC,"Station Status"=HYD_STATUS,
             "Drainage Area (sq km)"=DRAINAGE_AREA_GROSS,"Reference (RHBN)"=RHBN,"Real-Time"=REAL_TIME,"Contributor"=CONTRIBUTOR,"Operator"=OPERATOR,"Regional Office"=REGIONAL_OFFICE,
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
  
  proxy = DT::dataTableProxy('allstationsTable')
  observeEvent(input$station, {
    proxy %>% DT::selectRows(which(stations.list == input$station))
  })
  
  
  # Deal with later
  #observeEvent(input$downloadHYDAT, {
  #  download_hydat(dl_hydat_here = getwd())
  #})
  
  
  ########### download table of stations
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
  
  
}


shinyApp(ui = ui, server = server)