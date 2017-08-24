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




# This code utilizes the HYDAT R package (D. Hutchinson (ECCC), https://github.com/CentreForHydrology/HYDAT) to extract data from the HYDAT database

### Shiny App Script ###
########################

library(shiny) # 1.0.3 shiny
library(shinydashboard)
library(ggplot2)
library(dplyr) ## >0.7.0 dplyr
library(tidyr)
library(HYDAT)
library(RSQLite)
library(leaflet)
library(DBI)
library(tidyhydat)


#####  Set File Pathways #####

### Set path to HYDAT
HYDAT.path <- "Hydat.sqlite3" 

## Create a list of all stations in BC
stations <- STATIONS(HYDAT.path,
                       STATION_NUMBER = "ALL",
                       PROV_TERR_STATE_LOC = "BC") %>%
  left_join(AGENCY_LIST(HYDAT.path), by = c("CONTRIBUTOR_ID" = "AGENCY_ID")) %>% rename("CONTRIBUTOR"=AGENCY_EN) %>% 
  left_join(AGENCY_LIST(HYDAT.path), by = c("OPERATOR_ID" = "AGENCY_ID")) %>%  rename("OPERATOR"=AGENCY_EN) %>% 
  left_join(DATUM_LIST(HYDAT.path), by = c("DATUM_ID" = "DATUM_ID")) %>% rename("DATUM"=DATUM_EN) %>% 
  mutate(REGIONAL_OFFICE_ID = as.integer(REGIONAL_OFFICE_ID)) %>% 
  left_join(REGIONAL_OFFICE_LIST(HYDAT.path), by = c("REGIONAL_OFFICE_ID" = "REGIONAL_OFFICE_ID")) %>% rename("REGIONAL_OFFICE"=REGIONAL_OFFICE_NAME_EN) %>% 
  select(STATION_NUMBER,STATION_NAME,PROV_TERR_STATE_LOC,HYD_STATUS,LATITUDE,LONGITUDE,DRAINAGE_AREA_GROSS,RHBN,REAL_TIME,CONTRIBUTOR,OPERATOR,REGIONAL_OFFICE,DATUM)

stations.list <- as.list(stations$STATION_NUMBER)



# Set up the user-interface
ui <- dashboardPage(
  dashboardHeader(title="HYDAT Data Viewer"),
  dashboardSidebar(
    br(),
    uiOutput("stnSelect"),
    br(),
    hr(),
    h5("This app extracts hydrometric discharge data from the HYDAT database and displays station metadata, historical data, and real-time data, if available. A locally saved SQLite HYDAT database file is required."),
    br()
  ),
  dashboardBody(
    tabBox("TITLE",width = 12,
           tabPanel("Station Listings",
                    fluidRow(column(width = 8,
                    helpText("Search for a station by entering all or part of a station name, number, or other categories. To view station information and hydrometric data, click on the row and view the other tabs. To search by map, go to the 'Stations Map' tab and click on the marker of your desired station. Each map or table selection will replace the previous selection."))),
                    br(),
                    DT::dataTableOutput("allstationsTable")
                    
           ),
           tabPanel("Stations Map",
                    br(),
                    tags$style(type = "text/css", "#map {height: calc(100vh - 170px) !important;}"),
                    leafletOutput("map")
           ),
           tabPanel("Station Info",
                    br(),
                    h4("Station Information"),
                    fluidRow(column(width = 5,tableOutput("metaTable")),
                             column(width = 7,box(width=15,background="light-blue",
                                                  leafletOutput("stnmap")
                                                  )))),
           tabPanel("Historical Data",
                    selectInput("histView",label="Historical data type to view:",choices = list("Long-term","Annual","Monthly", "Daily","Instantaneous Extremes")),
                    # Historical Long-term
                    conditionalPanel(
                      condition = "input.histView == 'Long-term'",
                      br(),
                      column(6, uiOutput("dateRange")),
                      column(3, checkboxInput("ltlog", label = "Plot Discharge axis on log scale", value= FALSE)),
                      column(3, downloadButton('download.ltData', 'Download Data'),
                                downloadButton('download.ltPlot', 'Download Plot')),
                      column(11, br()),
                      plotOutput('ltplot')),
                    # Historical Annual
                    conditionalPanel(
                      condition = "input.histView == 'Annual'",
                      h3("under development"),
                      br(),
                      column(3, checkboxGroupInput("annualChecks", label = "Statistics", choices=list("Mean","Median","Maximum","Minimum"),selected = c("Mean","Median","Maximum","Minimum"))),
                      column(6, checkboxInput("annuallog", label = "Plot Discharge axis on log scale", value= FALSE)),
                      column(3, downloadButton('download.annualData', 'Download Data'),
                                downloadButton('download.annualPlot', 'Download Plot')),
                      plotOutput('annualPlot'),
                      h5("* Only years with complete data shown")#,
                     # tableOutput("annualTable")
                      ),
                    # Historical Monthly
                    conditionalPanel(
                      condition = "input.histView == 'Monthly'",
                      h3("under development"),
                      br(),
                      column(3, checkboxGroupInput("monthChecks", label = "Statistics", choices=list("Mean","Median","Maximum","Minimum","Upper Quartile","Lower Quartile"),selected = c("Mean","Median","Maximum","Minimum","Upper Quartile","Lower Quartile"))),
                      column(6, checkboxInput("monthlog", label = "Plot Discharge axis on log scale", value= FALSE)),
                      column(3, downloadButton('download.monthData', 'Download Data'),
                             downloadButton('download.monthPlot', 'Download Plot')),
                      plotOutput('monthPlot'),
                      h5("* Missing dates ignored")#,
                      #tableOutput("monthTable")
                    ),
                    conditionalPanel(
                      condition = "input.histView == 'Daily'",
                      br(),
                      h4("Daily data coming soon!"),
                      h5("Including daily means, medians, maxs and mins, and 25/27th percentiles")
                    ),
                    conditionalPanel(
                      condition = "input.histView == 'Instantaneous Extremes'",
                      br(),
                      h4("instantaneous extreme data coming soon!"),
                      h5("Including max and min")
                    )
                    ),
           tabPanel("Real-time Data",
                    br(),
                    column(6, textOutput("noRT"), uiOutput("dateRangeRT")),
                    column(3, checkboxInput("logRT", label = "Plot Discharge axis on log scale", value= FALSE)),
                    column(3, downloadButton('download.rtData', 'Download Data'),
                           downloadButton('download.rtPlot', 'Download Plot')),
                    column(11, br()),
                    plotOutput('rtplot'))
    )
  )
)


######################################################################################################
######################################################################################################

# Set up the server (where all the magic happens)
server <- function(input, output, session) {
  
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
    
    # will replace with tidyhydat
    db.con <- dbConnect(RSQLite::SQLite(), HYDAT.path)
    stn.reg.HYDAT <- StationRegulation(con = db.con, station_number=input$station) %>% rename("STATION_NUMBER"= station_number)#input$station)#input$station)#"08HB048"
    dbDisconnect(db.con)
    #
    
    stn.info <- merge(stn.meta.HYDAT, stn.reg.HYDAT[,c(1,4)], by= "STATION_NUMBER") %>% 
      mutate("Historical Data Link"=paste0("https://wateroffice.ec.gc.ca/report/historical_e.html?stn=",STATION_NUMBER),
             "Real-Time Data Link"=ifelse(REAL_TIME==TRUE, paste0("https://wateroffice.ec.gc.ca/report/real_time_e.html?stn=",STATION_NUMBER),"No real-time data available.")) %>% 
      rename("Station Number"=STATION_NUMBER,"Station Name" =STATION_NAME,"Prov/Terr/State"=PROV_TERR_STATE_LOC,"Station Status"=HYD_STATUS,
             "Latitude"=LATITUDE,"Longitude"=LONGITUDE,"Drainage Area (sq km)"=DRAINAGE_AREA_GROSS,"Reference (RHBN)"=RHBN,"Real-Time"=REAL_TIME,
             "Regional Office"=REGIONAL_OFFICE,"Contributor"=CONTRIBUTOR,"Operator"=OPERATOR,"Datum"=DATUM,"Regulated"=regulated) %>% 
      gather("header","content",1:16)
    stn.info[is.na(stn.info)] <- ""
    stn.info
  })
  
  # Rander table for output
  output$metaTable <- renderTable(metaData(),colnames = FALSE)
  
  
  output$stnmap <- renderLeaflet({
    leaflet(stations) %>% addTiles() %>%
      setView(lng = metaData()[6,2], lat = metaData()[5,2], zoom = 9) %>% # set centre and extent of map
      addCircleMarkers(data = filter(stations, STATION_NUMBER %in% input$station), ~LONGITUDE, ~LATITUDE, color = "red", radius = 6) %>%
      addCircles(lng = ~LONGITUDE, lat = ~LATITUDE, weight = 1,
                 radius = 1, label = ~STATION_NAME, 
                 popup = ~paste(STATION_NAME, "<br>",
                                STATION_NUMBER, "<br>",
                                "DRAINAGE AREA = ",DRAINAGE_AREA_GROSS, "SQ. KM","<br>")
      )
    
  })
  
  ### Historical Data ###
  ######################
  
  # Extract historical data from HYDAT and determine the start and end dates for clipping
  histDates <- reactive({

    daily.flow.HYDAT <- DLY_FLOWS(hydat_path = HYDAT.path,PROV_TERR_STATE_LOC =metaData()[3,2], STATION_NUMBER=input$station)

    daily.flow.dates <- daily.flow.HYDAT[,c(2,4)] %>% 
      summarize(minDate=min(Date),
                maxDate=max(Date))
  })
  
  
  ### Historical Long-term Data
  
  # Get daily data from HYDAT, fill in missing days, and clip to dates
  histData <- reactive({
    
    daily.flow.HYDAT <- DLY_FLOWS(hydat_path = HYDAT.path,PROV_TERR_STATE_LOC =metaData()[3,2], STATION_NUMBER=input$station)
    
    flow.data <- daily.flow.HYDAT[,c(2:5)]
    colnames(flow.data) <- c("Date", "Parameter","Discharge","Symbol")
    
    min.date <- as.Date((paste((as.numeric(format(min(flow.data$Date),'%Y'))),01,01,sep="-")),"%Y-%m-%d")
    max.date <- as.Date((paste((as.numeric(format(max(flow.data$Date),'%Y'))),12,31,sep="-")),"%Y-%m-%d")
    flow.data.empty <- data.frame(Date=seq(min.date, max.date, by="days"))
    flow.data <- merge(flow.data.empty,flow.data,by="Date",all = TRUE)

    flow.data= flow.data[flow.data$Date  >=input$date.range[1] & flow.data$Date <= input$date.range[2],]
  })
  
  # Plot reactive data and render for plotting
  longtermPlot <- function(){
    validate(
      need(histData(),"NOT AVAILABLE")
    )
    full.record.plot <- ggplot(data=histData(), aes_string(x=histData()$Date, y=histData()$Discharge))+
      ggtitle(paste0("DAILY DISCHARGE - ",metaData()[2,2]," (",metaData()[1,2],")"))+
      theme(plot.title = element_text(hjust = 0.5))+
      geom_line(colour="dodgerblue4")+
      {if(input$ltlog)scale_y_log10()}+
      ylab("Discharge (cms)")+
      xlab("Date")+
      theme(axis.title = element_text(size=15),
            plot.title = element_text(size=15,hjust = 0.5),
            axis.text = element_text(size=13))
    
    print(full.record.plot)
  }
  output$ltplot <- renderPlot({
    longtermPlot()
  })
  
  #Download plot/data buttons
  output$download.ltData <- downloadHandler(
    filename = function() {paste0(metaData()[1,2]," - daily discharge.csv")},
    content = function(file) {
      write.csv(histData(),file, row.names = FALSE, na="")
    })
  output$download.ltPlot <- downloadHandler(
    filename = function() {paste0(metaData()[1,2]," - daily discharge.png")},
    content = function(file) {
      png(file, width = 900, height=500)
      print(plotInput())
      dev.off()
    })    
  
  ### Historical Annual Data
  
  # Calculate annual data and render for printing
  annualData <- function(){
    daily.data <- histData()
    daily.data$Year <- as.numeric(format(daily.data$Date,'%Y'))
    
    annual.data <- daily.data %>% 
      group_by(Year) %>% 
      summarize("Missing Days"= sum(is.na(Discharge)),
                Mean=mean(Discharge),
                Maximum=max(Discharge),
                Minimum=min(Discharge),
                Median=median(Discharge))
    annual.data
  }
  # output$annualTable <- renderTable(annualData(),colnames = TRUE)
  
  # Plot reactive data and render for plotting
    annualPlot <- function(){
    validate(
      need(annualData(),"NOT AVAILABLE")
    )
    annual.plot <- ggplot(data=annualData(), aes_string(x=annualData()$Year))+
      ggtitle(paste0("ANNUAL FLOWS - ",metaData()[2,2]," (",metaData()[1,2],")"))+
      theme(plot.title = element_text(hjust = 0.5))+
      {if("Mean" %in% input$annualChecks) geom_point(aes_string(y=annualData()$Mean),colour="dodgerblue4", size=3)}+
      {if("Mean" %in% input$annualChecks) geom_line(aes_string(y=annualData()$Mean),colour="dodgerblue4")}+
      {if("Median" %in% input$annualChecks) geom_point(aes_string(y=annualData()$Median),colour="royalblue", size=3)}+
      {if("Median" %in% input$annualChecks) geom_line(aes_string(y=annualData()$Median),colour="royalblue")}+
      {if("Minimum" %in% input$annualChecks) geom_point(aes_string(y=annualData()$Minimum),colour="purple", size=3)}+
      {if("Minimum" %in% input$annualChecks) geom_line(aes_string(y=annualData()$Minimum),colour="purple")}+
      {if("Maximum" %in% input$annualChecks) geom_point(aes_string(y=annualData()$Maximum),colour="green", size=3)}+
      {if("Maximum" %in% input$annualChecks) geom_line(aes_string(y=annualData()$Maximum),colour="green")}+
      {if(input$annuallog)scale_y_log10()}+
      ylab("Discharge (cms)")+
      xlab("Year")+
      theme(axis.title = element_text(size=15),
            plot.title = element_text(size=15,hjust = 0.5),
            axis.text = element_text(size=13))
    print(annual.plot)
  }
  output$annualPlot <- renderPlot({
    annualPlot()
  })
  
  #Download plot/data buttons
  output$download.annualData <- downloadHandler(
    filename = function() {paste0(metaData()[1,2]," - annual summary.csv")},
    content = function(file) {
      write.csv(annualData(),file, row.names = FALSE, na="")
    })  
  output$download.annualPlot <- downloadHandler(
    filename = function() {paste0(metaData()[1,2]," - annual summary.png")},
    content = function(file) {
      png(file, width = 900, height=500)
      print(annualPlot())
      dev.off()
    })    
  
  
  ### Historical Monthly Data
  
  # Calculate annual data and render for printing
  monthData <- function(){
    daily.data <- histData()
    daily.data$Year <- as.numeric(format(daily.data$Date,'%Y'))
    daily.data$Month <- as.integer(format(daily.data$Date,'%m'))
    
    month.data <- daily.data %>% 
      group_by(Month) %>% 
      summarize(Mean=mean(Discharge, na.rm=TRUE),
                Maximum=max(Discharge, na.rm=TRUE),
                Minimum=min(Discharge, na.rm=TRUE),
                Median=median(Discharge, na.rm=TRUE),
                "Upper Quartile"=quantile(Discharge,.75, na.rm=TRUE),
                "Lower Quartile"=quantile(Discharge,.25,na.rm=TRUE))
    month.data
  }
  #output$monthTable <- renderTable(monthData(),colnames = TRUE)
  
  # Plot reactive data and render for plotting
  monthPlot <- function(){
    validate(
      need(monthData(),"NOT AVAILABLE")
    )
    month.plot <- ggplot(data=monthData(), aes_string(x=monthData()$Month))+
      ggtitle(paste0("MONTHLY FLOWS - ",metaData()[2,2]," (",metaData()[1,2],")"))+
      theme(plot.title = element_text(hjust = 0.5))+
      {if("Mean" %in% input$monthChecks) geom_point(aes_string(y=monthData()$Mean),colour="dodgerblue4", size=3)}+
      {if("Mean" %in% input$monthChecks) geom_line(aes_string(y=monthData()$Mean),colour="dodgerblue4")}+
      {if("Median" %in% input$monthChecks) geom_point(aes_string(y=monthData()$Median),colour="royalblue", size=3)}+
      {if("Median" %in% input$monthChecks) geom_line(aes_string(y=monthData()$Median),colour="royalblue")}+
      {if("Minimum" %in% input$monthChecks) geom_point(aes_string(y=monthData()$Minimum),colour="purple", size=3)}+
      {if("Minimum" %in% input$monthChecks) geom_line(aes_string(y=monthData()$Minimum),colour="purple")}+
      {if("Maximum" %in% input$monthChecks) geom_point(aes_string(y=monthData()$Maximum),colour="green", size=3)}+
      {if("Maximum" %in% input$monthChecks) geom_line(aes_string(y=monthData()$Maximum),colour="green")}+
      {if("Upper Quartile" %in% input$monthChecks) geom_point(aes_string(y=monthData()$"Upper Quartile"),colour="red", size=3)}+
      {if("Upper Quartile" %in% input$monthChecks) geom_line(aes_string(y=monthData()$"Upper Quartile"),colour="red")}+
      {if("Lower Quartile" %in% input$monthChecks) geom_point(aes_string(y=monthData()$"Lower Quartile"),colour="pink", size=3)}+
      {if("Lower Quartile" %in% input$monthChecks) geom_line(aes_string(y=monthData()$"Lower Quartile"),colour="pink")}+
      {if(input$monthlog)scale_y_log10()}+
      ylab("Discharge (cms)")+
      xlab("Month")+
      scale_x_continuous(breaks = c(1:12),labels = c("Jan","Feb","Mar","Apr","May","Jun","July","Aug","Sep","Oct","Nov","Dec"))+
      theme(axis.title = element_text(size=15),
            plot.title = element_text(size=15,hjust = 0.5),
            axis.text = element_text(size=13))
    print(month.plot)
  }
  output$monthPlot <- renderPlot({
    monthPlot()
  })
  
  #Download plot/data buttons
  output$download.monthData <- downloadHandler(
    filename = function() {paste0(metaData()[1,2]," - monthly summary.csv")},
    content = function(file) {
      write.csv(monthData(),file, row.names = FALSE, na="")
    })  
  output$download.monthPlot <- downloadHandler(
    filename = function() {paste0(metaData()[1,2]," - monthly summary.png")},
    content = function(file) {
      png(file, width = 900, height=500)
      print(monthPlot())
      dev.off()
    })    
  
  ### Real-Time Data ###
  ######################
  
  # Extract historical data from HYDAT and determine the start and end dates for clipping
  realtimeDates <- reactive({

    realtime.HYDAT <- download_realtime_dd(STATION_NUMBER = input$station,PROV_TERR_STATE_LOC = metaData()[3,2])
    
    real.timeDates <- as.data.frame(realtime.HYDAT[,2])
    colnames(real.timeDates) <- "DateTime"
    real.timeDates$Date <- as.Date(real.timeDates$DateTime,"%Y-%m-%d")
    
    real.timeDates <- real.timeDates %>% 
      summarize(minDate=min(Date),
                maxDate=max(Date))
  })
  
  # Extract real-time data from webslink and clip to dates
  realtimeData <- reactive({ 

    realtime.HYDAT <- download_realtime_dd(STATION_NUMBER = input$station,PROV_TERR_STATE_LOC = metaData()[3,2])

    real.time <- realtime.HYDAT[,c(2:4)] %>% spread(Parameter,Value)
    colnames(real.time) <- c("DateTime","Discharge","Water Level")
    real.time$Date <- as.Date(real.time$DateTime,"%Y-%m-%d")
    real.time <- as.data.frame(real.time[,c(4,1:3)])
    
    real.time= real.time[real.time$Date  >=input$date.rangeRT[1] & real.time$Date <= input$date.rangeRT[2],]
  })
  
  # Create ggplot function
  realtimePlot <- function(){
    real.time.plot <- ggplot(data=realtimeData(), aes_string(x="DateTime"))+
      #geom_line(aes(y=WL),colour="red")+
      geom_line(aes(y=Discharge),colour="dodgerblue4")+
      ggtitle(paste0("REAL-TIME DISCHARGE - ",metaData()[2,2]))+
      theme(plot.title = element_text(hjust = 0.5))+
      {if(input$logRT)scale_y_log10()}+
      ylab("Instantaneous Discharge (cms)")+
      xlab("Date")+
      theme(axis.title = element_text(size=15),
            plot.title = element_text(size=15,hjust = 0.5),
            axis.text = element_text(size=13))
    #scale_y_continuous(sec.axis = sec_axis(~.*5, name = "Relative humidity [%]"))
    
    real.time.plot
  }
  
  # Render ggplot for output (if not real time, dont plot)
  output$rtplot <- renderPlot({
    if(metaData()[9,2]=="Yes") realtimePlot()
  })
  
  ### Leaflet map ###
  ###################
  
  output$map <- renderLeaflet({
    leaflet(stations) %>% addTiles() %>%
      #setView(lng = -125, lat = 54, zoom = 5) # set centre and extent of map
      addCircleMarkers(data= filter(stations, HYD_STATUS=="ACTIVE"), lng = ~LONGITUDE, lat = ~LATITUDE, layerId = ~STATION_NUMBER, color = "blue", radius = 2,
                 group="Active",
                 label = ~paste0(STATION_NAME, " (",STATION_NUMBER,")")) %>% #,
                 #popup = ~paste(STATION_NAME, "<br>", STATION_NUMBER, "<br>","DRAINAGE AREA = ",DRAINAGE_AREA_GROSS, "SQ. KM", "<br>",ifelse(HYD_STATUS=="A","ACTIVE","DISCONTINUED"),"<br>")) %>%
      addCircleMarkers(data= filter(stations, HYD_STATUS=="DISCONTINUED"), lng = ~LONGITUDE, lat = ~LATITUDE, layerId = ~STATION_NUMBER, color = "red", radius = 2,
                 group="Discontinued",
                 label = ~paste0(STATION_NAME, " (",STATION_NUMBER,")")) %>% #,
                 #popup = ~paste(STATION_NAME, "<br>", STATION_NUMBER, "<br>","DRAINAGE AREA = ",DRAINAGE_AREA_GROSS, "SQ. KM", "<br>",ifelse(HYD_STATUS=="A","ACTIVE","DISCONTINUED"),"<br>")) %>%
      addCircleMarkers(data = filter(stations, STATION_NUMBER %in% input$station), ~LONGITUDE, ~LATITUDE, color = "green", radius = 6) %>% 
      addLayersControl(position="topright",
                       overlayGroups = c("Active","Discontinued"),
                       options = layersControlOptions(collapsed=FALSE)) %>% 
      hideGroup("Discontinued")
    
  })
  
  
  observeEvent(input$map_marker_click, { # update the location selectInput on map clicks
    updateSelectizeInput(session, "station", selected=input$map_marker_click$id)
  })
  
  ### Reactive Widgets ###
  ########################
  

  
  # Structure to download real-time CSV file, only works if opened in browser
  output$download.rtData <- downloadHandler(
    filename = function() {paste0(metaData()[1,2]," - real-time discharge.csv")},
    content = function(file) {
      write.csv(realtimeData(),file, row.names = FALSE, na="")
    })  
  
  # Structure to download real-time plot, only works if opened in browser
  output$download.rtPlot <- downloadHandler(
    filename = function() {paste0(metaData()[1,2]," - real-time discharge.png")},
    content = function(file) {
      png(file, width = 900, height=500)
      print(realtimePlot())
      dev.off()
    })    
  
  # Structure to set historical dates input to match start and ends of data
  output$dateRange <- renderUI({
    dateRangeInput("date.range","Select start and end dates to plot:",format = "yyyy-mm-dd",startview = "month",start = histDates()$minDate, end = histDates()$maxDate)
  })
  
  # Structure to set real-time dates input to match start and ends of data (remove if no real-time data)
  output$dateRangeRT <- renderUI({
    {if(metaData()[9,2]=="Yes") dateRangeInput("date.rangeRT","Select start and end dates to plot:",format = "yyyy-mm-dd",startview = "month",start = realtimeDates()$minDate, end = realtimeDates()$maxDate)}
  })
  
  # Place text in real-time sidepanel section if no real-time data
  output$noRT <- renderText({
    {if(metaData()[9,2]=="No") paste("*** No real-time data available for this station. No data or plot available to view or download. ***")}
  })
  
  allstationsTable <- reactive({
    
    stn.meta.HYDAT <- stations %>% filter(STATION_NUMBER==stations.list)#input$station)
    
    # will replace with tidyhydat
    db.con <- dbConnect(RSQLite::SQLite(), HYDAT.path)
    stn.reg.HYDAT <- StationRegulation(con = db.con, station_number=stations.list) %>% rename("STATION_NUMBER"= station_number)
    dbDisconnect(db.con)
    
    stn.info <- merge(stn.meta.HYDAT[,c(1:4,7:12)],stn.reg.HYDAT[,c(1,4)], by= "STATION_NUMBER") %>% 
      rename("Station Number"=STATION_NUMBER,"Station Name" =STATION_NAME,"Prov/ Terr/ State"=PROV_TERR_STATE_LOC,"Station Status"=HYD_STATUS,
             "Drainage Area (sq km)"=DRAINAGE_AREA_GROSS,"Reference (RHBN)"=RHBN,"Real-Time"=REAL_TIME,"Contributor"=CONTRIBUTOR,"Operator"=OPERATOR,"Regional Office"=REGIONAL_OFFICE,
             "Regulated"=regulated)
  }) 
  
  output$allstationsTable <- DT::renderDataTable(allstationsTable(), rownames=FALSE,selection=list(mode="single")) 
  
}


shinyApp(ui = ui, server = server)
