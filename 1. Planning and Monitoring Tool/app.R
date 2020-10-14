# Libraries  ---------------------------------
library(shiny)
library(shinydashboard)
library(dplyr)
library(plotly)
library(xlsx)
library(lubridate)
library(leaflet)
library(RColorBrewer)
library(rhandsontable)
library(rstudioapi)
library(shinyalert)
library(shinyjs)


  # Define dataframes to be populated locally ---------------------------------

numSim <- 1000 #Number of simulations to run
Samp <- 10

batchData <- NULL


bd <- data.frame(BaseT_Site = 1:5,
                        Location = rep("one", times = 5), 
                        Longitude =  rep(18.4241, times = 5),
                        Latitude = rep(-33.9249, times = 5),
                        RecruitGoal = rep(50, times = 5))

siteParam <- data.frame(BaseT_Site = 1:5, recruitRate = rep(NA, 5))         #!!!

xbar <- 1

  #Global Functions ---------------------------------

  #Simulation model
runSimulation <- function(param) {
  
  durVector <- rep(NA, times = numSim)
  
  for (i in 1:numSim) {
    
    sim <- rpois(10000, param$recruitRate)
    sim <- cumsum(sim)
    durVector[i] <- which.max(sim >= param$RecruitGoal)
    
  }
  
  durVector
  
}

  #Output data analysis 
calc_CI <- function(simVector, Prob) {    
  
  noSamp <- numSim/Samp
  
  xbar <<- rep(NA, times = numSim/noSamp)
  
  for (i in 1:noSamp) {
    
    lower <- (i-1) * Samp + 1
    upper <- i * Samp
    
    xbar[i] <<- mean(simVector[lower:upper])
    
  }
  
  stdDev <- sd(xbar)
  xdbar <- mean(xbar)
  
  lower <- xdbar - stdDev * qt((1 - Prob)/2, noSamp - 1, lower.tail = FALSE)/sqrt(noSamp)
  upper <- xdbar + stdDev * qt((1 - Prob)/2, noSamp - 1,  lower.tail = FALSE)/sqrt(noSamp)
  
  c(lower, upper)
  
}



  # Header ---------------------------------

header <- dashboardHeader(
              title = "Trial Recruitment Tool",
              titleWidth = 700
              
)

  # Sidebar ---------------------------------

sidebar <- dashboardSidebar(
  sidebarMenu( id = "tabs",
    menuItem("Planning", tabName = "plan", icon = icon("edit")),
    menuItem("Monitoring", tabName = "monitor", icon = icon("line-chart"),
      menuSubItem("1. Baseline Parameters", tabName = "baseline"), 
      menuSubItem("2. Trial Overview", tabName = "monTrial"),
      menuSubItem("3. Site Information", tabName = "monSite"),
      
      fileInput("dataInput", "Add the latest recruitment data:")
    )
  )
  
)



  # Body Content ---------------------------------
    # User interface for individual pages

      ## Planning Page =================================

  planPage <-  fluidPage(
                fluidRow(
                  box(
                    column(  
                      tabBox(
                      
                       tabPanel("Overview",
                        fluidRow(
            ### Input parameters #############################
                         box( title = "Specify trial paramenters", status = "primary",
                        
                          numericInput("planRecruitRate", "Specify the expected recruitment rate per week:", value = 1.12, min = 0, step = 0.01),   
                          numericInput("planSample", "Specify the recruitment goal for the trial:", 60, min = 0, max = 100000, step = 1),
                          numericInput("planProb", "Specify the level of confidence for the predicted output:", 95, min = 0, max = 100, step = .01),
                          
                          width = 2
                          ),
            
                       
            ### Output side #############################
                          box(title = "Output", status = "primary", 
                            
                            fluidRow(    
                            valueBoxOutput("recGoal", width = 4),
                            valueBoxOutput("expDate", width = 4),
                            valueBoxOutput("planRecRate", width = 4)
                            
                            ),
                            
                            fluidRow(
                             
                            ),
                          
                          
                        width = 10
                      )
                    )
                  ),
            ### Multiple Sites #############################
                  tabPanel( "Multiple Sites",
                           
                        fluidRow(
                          box(title = "Specify trial paramenters", status = "primary",
                            
                              numericInput("siteQty", label = "Specify the number of sites", value = 10, min = 2, step = 1),
                              
                              "Once the table has been populated click on the button below",
                              
                              rHandsontableOutput("sitePlanTable"),
                              numericInput("multiplanProb", "Specify the level of confidence for the predicted output:", 95, min = 0, max = 100, step = .01),
                              
                              actionButton("runSim", "Run Simulation"),
                              
                              width = 3
                            ),
                
                          box(title = "Output", status = "primary",
                              
                              fluidRow(
                              valueBoxOutput("multiRecGoal", width = 4),
                              valueBoxOutput("multiDurationInfo", width = 4),
                              valueBoxOutput("multiSiteNo", width = 4)),
                              
                              fluidRow(
                                plotlyOutput("planMultiPlot", height = "auto", width = "100%"))
                              ,

                              width = 9)
                        
                        )
                    ),
            
              width = 12),
            width = 12),
            width = 12
            )
          )
      )

      ## Monitor page =================================

  siteMonitor <- fluidPage(
       fluidRow(
      
      box(title = "Site Overview",
          column(width = 10,
            tabBox(
                
                tabPanel( "Map",
                  leafletOutput("siteMap", height = "500px")     #Possibly break the data set into 2  - those that are shanana and those that show issues
                                                #then with those that show issues the markers can be changed?  ----- Acceptable range below?
                ),
                
                tabPanel("Graph",
                  plotlyOutput("sitePlot", height = "800px")
                ),
                
                width = 12)
            ),
      
        column( width = 2,

                numericInput("warnNo", label = "Flag locations with total recruitments less than:", value = 0, step = 1)
                
        ),
      
        collapsible = TRUE, width = 12)
      
    ),
    fluidRow(
      box(title = "Site Specific",
          
          fluidRow(
            
            column(width = 4,     #Box showing the current recruitment rate/# of recruitments?    
                   
                   valueBoxOutput("vbSiteRec", width = 12)
                   
                   
            ),
            
            column(width = 4,     #Box showing the probability to finish at the given set date/expected date given baseline probability
                   
                   valueBoxOutput("vbSiteEnd", width = 12)
                   
            ),
            
            column(width = 4,     #Box Showing sites that are below the rate/# of recruitments?
                   
                   valueBoxOutput("vbSiteRate", width = 12)
                   
            )
            
            
          ),
          
          
          fluidRow(
          
            column(width = 9,
                   
                   plotlyOutput("siteGraph")
                   
                   ),
            
            column(width = 3, 
                   
                   selectInput("siteSelect", label = "Select a site from the dropdown or by clicking the desired site from the map:", choices = NULL),
                   numericInput("siteRate", label = "Site recruitment rate per weeks:", value = 0, step = 0.001, min = 0),
                   numericInput("sitePend", label = "Number of outstanding participants:", value = 0, step = 1, min = 0),
                   numericInput("siteplanProb", "Specify the level of confidence for the predicted output:", 95, min = 0, max = 100, step = .01)
                   
                   )
              
          ),    
              width = 12    
      )
    )
  )
  
  
  
    trialMonitor <- fluidPage(
      
            ### File to analyse
        
            ### Output Section #############################
        
      fluidRow(
        
        box(title = "Overview",
            
            fluidRow(
            
              column(width = 4,     #Box showing the current recruitment rate/# of recruitments?    
                     
                     valueBoxOutput("valueRec", width = 12)
                     
                     
                     ),
              
              column(width = 4,     #Box showing the probability to finish at the given set date/expected date given baseline probability
                     
                     valueBoxOutput("expDur", width = 12)
                     
                     
              ),
              
              column(width = 4,     #Box Showing sites that are below the rate/# of recruitments?
                     
                     valueBoxOutput("detSite", width = 12)
 
              )
            
            ),
           
             fluidRow(
              column(width = 12 ,
             plotlyOutput("recrGraph", width = "100%"),
             useShinyalert(),
             useShinyjs()
              )
             ), 
            
            width = 12
        )    
      )
      
    )

    ## Baseline Page ---------------------------------
  
  baselinePage <- fluidPage(
    
    fluidRow(
      column(width = 9,
        box(title = "Baseline Information",
            
        rHandsontableOutput("baseTable"),
        actionButton("updateTable", "Update baseline information"),
        
        
        width = NULL,
        solidHeader = TRUE,
        collapsible = TRUE
      
        )
      ),
      column(width = 3,
             box(title = "Import and Export Baseline Parameters",
             fileInput("trialBase", "Upload here..."),
             downloadButton("dlBaseline", "Export Baseline"),
             width = NULL)
             )
    )
  )

    ## Sitemap for body ---------------------------------
body <- dashboardBody(
  tabItems(
    tabItem(tabName = "plan",
            h1("Planning"),
           planPage
            
    ),
    tabItem(tabName = "monitor"
    ),
    
    tabItem(tabName = "monTrial",
            h1("Trial Overview"),
            trialMonitor            
            
    ),
    tabItem(tabName = "monSite",
            h1("Site Breakdown"),
            siteMonitor
    ),
    tabItem(tabName = "baseline",
            h1("Baseline Parameters"),
            baselinePage
    )
  )
)



# UI ---------------------------------

ui <- dashboardPage(
  
  header, sidebar, body
  
)



# Server ---------------------------------

server <- function(input, output, session) {
  
  vals <- reactiveValues(baseData = data.frame(
                         BaseT_Site = 1:5,
                         Location = LETTERS[1:5], 
                         Longitude =  rep(18.4241, times = 5),
                         Latitude = rep(-33.9249, times = 5),
                         RecruitGoal = rep(50, times = 5)),
                         upLogic = FALSE
                         )
  
  # Planning Page ---------------------------------
  
   
  
    ## Overview =================================
  normDuration <- reactive({  #Function that does all of the calculations for the trial length --- Data frame with periodic intervals?
    
    lambda <- input$planRecruitRate   # !!! Can possible make a seperate function that returns a dataframe summarising all of this info
    n <- input$planSample
    prob <- input$planProb/100
    
    zVal <- qnorm(prob)
    
    f <- function(t) (lambda*t - zVal * (lambda * t)^0.5 - (n- 0.5))
    
    uniroot(f, lower = 0, upper = 1000)$root
    
  })  
  
  simDuration <- reactive({
    lambda <- input$planRecruitRate   # !!! Can possible make a seperate function that returns a dataframe summarising all of this info
    n <- ceiling(input$planSample/(input$planRetention/100))
    
    a <- data.frame(recruitRate = lambda, RecruitGoal = n)
    
    runSimulation(a)
 
  })
  
  
  output$durationInfo <- renderValueBox({
    valueBox(c(round(normDuration(), digits = 0), " weeks") ,"Estimated recruitment duration", icon = icon("clock-o"), color = "teal")
    
  })
  
  output$expDate <- renderValueBox({
    
    lambda <- input$planRecruitRate   # !!! Can possible make a seperate function that returns a dataframe summarising all of this info
    n <- input$planSample
    
    a <- data.frame(recruitRate = lambda, RecruitGoal = n)
    
    myVec <- runSimulation(a)
    
    b <- calc_CI(myVec, input$planProb/100) 
    
    
    valueBox(paste(round(b[1],2), " - ", round(b[2], 2), " weeks") ,subtitle = "Estimated recruitment duration", icon = icon("clock-o"), color = "purple")
    
  })
  
  output$recGoal <- renderValueBox({
    
    valueBox(paste(input$planSample, " participants") , subtitle = "Total specified recruitment goal", icon = icon("users"), color = "teal")
    
  })
  
  output$planRecRate <- renderValueBox({
    
    valueBox(input$planRecruitRate, subtitle = "Expected number of weekly recruitments", icon = icon("line-chart"), color = "light-blue")
    
  })
  
  output$planPlot <- renderPlotly({
    
    simVector <- simDuration()
    
    simData <- data.frame(Durations = round((xbar-1.5)/1) * 1)
    
    simData <- simData %>% group_by(Durations) %>% summarise(Total = n()/length(xbar))
      
    plot_ly(simData, x = ~Durations, y = ~Total ,type = "scatter", fill = "tozeroy")
    
  })
  
    ## Multiple Sites =================================
  
  output$sitePlanTable <- renderRHandsontable({
  
    noSites <- input$siteQty
    
    dfSitePlan <- data.frame(SiteNo = 1:noSites,
                                recruitRate = rep(1, times = noSites), 
                                RecruitGoal =  rep(10, times = noSites),
                                StartDelay = rep(0, times = noSites)
                             ) 
    
    rhandsontable(dfSitePlan) 
  })


  multiSitePlan <- eventReactive(input$runSim, {
    
    noSites <- input$siteQty
    
    sitePlan <- input$sitePlanTable
    sitePlan <- hot_to_r(input$sitePlanTable)
    sitePlan$RecruitGoal <- sitePlan$RecruitGoal
    sitePlan <- select(sitePlan, SiteNo, recruitRate, RecruitGoal)
    
    
    siteCI <- data.frame(SiteNo = 1:noSites, lowerCI = rep(NA, noSites), upperCI = rep(NA, noSites))
    

    for (i in 1:noSites){
      
      a <- runSimulation(sitePlan[i,])
      
      vec_CI <-  calc_CI(a , input$multiplanProb/100)
      siteCI$lowerCI[i] <- vec_CI[1]
      siteCI$upperCI[i] <- vec_CI[2]

    }
    
    siteCI
    
  })
  
  output$multiRecGoal <- renderValueBox({
    
    sitePlan <- hot_to_r(input$sitePlanTable)
    
    valueBox(paste(sum(sitePlan$RecruitGoal), " participants") ,"Total specified recruitment goal", icon = icon("users"), color = "teal")
    
  })
  
  output$multiDurationInfo <- renderValueBox({
    
    sitePlan <- hot_to_r(input$sitePlanTable)
    sitePlan <- select(sitePlan, SiteNo, StartDelay)
    
    dfCI <- multiSitePlan()
    dfCI$upperCI <- dfCI$upperCI + sitePlan$StartDelay
    dfCI$lowerCI <- dfCI$lowerCI + sitePlan$StartDelay
    
    i <- which.max(dfCI$upperCI)
    
    valueBox(paste(round(dfCI$lowerCI[i],2), " - ", round(dfCI$upperCI[i], 2), " weeks") ,"Estimated recruitment duration", icon = icon("clock-o"), color = "purple")
    
  })
  
  output$multiSiteNo <- renderValueBox({
    
    sitePlan <- hot_to_r(input$sitePlanTable)
    sitePlan <- select(sitePlan, SiteNo, StartDelay)
    
    dfCI <- multiSitePlan()
    dfCI$upperCI <- dfCI$upperCI + sitePlan$StartDelay
    i <- which.max(dfCI$upperCI)
    
    valueBox(paste("Site number ", dfCI$SiteNo[i]) ,subtitle = "Determinative site", icon = icon("building"), color = "light-blue")
    
  })
  
  output$planMultiPlot <- renderPlotly({
    
    siteDur <- multiSitePlan()
    
    sitePlan <- input$sitePlanTable
    sitePlan <- hot_to_r(input$sitePlanTable)
    sitePlan <- select(sitePlan, SiteNo, StartDelay)
    
    siteDur <- left_join(sitePlan, siteDur, by = "SiteNo")
    
    cols <- brewer.pal(nrow(siteDur), name = "Set3")

    p <- plot_ly()
    
    for (i in 1:nrow(siteDur)) {
      p <- add_trace(p,
                     
                     x = c(siteDur$StartDelay[i], siteDur$StartDelay[i] + siteDur$upperCI[i]),  # x0, x1
                     y = c(i, i),  # y0, y1
                     mode = "lines",
                     line = list(color = cols[(i%%length(cols))+1], width = 20),
                     showlegend = F,
                     hoverinfo = "text",
                     
                     # Create custom hover text
                     
                     text = paste("Start: Week ", siteDur$StartDelay[i], "<br>",
                                  "End: Between week ", siteDur$StartDelay[i] +  round(siteDur$lowerCI[i],2), " and ", siteDur$StartDelay[i] +  round(siteDur$upperCI[i],2), "<br>",
                       "Duration: Between ", round(siteDur$lowerCI[i],2), " - ", round(siteDur$upperCI[i],2), "weeks")
            )
    }
    
    p %>% layout(xaxis = list(title = "Duration (Weeks)"),yaxis = list(autotick = FALSE, dtick = 1, title = "Site Number"), autosize = F, height = nrow(siteDur)*55, width = 1000)
    
  })
  
  
  
  
  # Monitor Page ---------------------------------
  
  storeData <- reactive({
    
    inFile <- input$dataInput
      if(is.null(inFile)) 
        return(NULL)
      
    myData <- read.xlsx(inFile$datapath, 1,  header=TRUE)
    myData
    

    
  }) #Function that returns the clean, updated data set
  
  allData <- reactive({
    
    if(!is.null(storeData()))
      batchData <- storeData()
    
    batchData <- select(batchData, BaseT_Site, BaseT_Date)
    batchData$BaseT_Date <-  as.Date(batchData$BaseT_Date, format = "%d-%m-%Y")
    
    
    siteDetails <- vals$baseData[,1:2]
    
    summaryData <- inner_join(batchData, siteDetails, by = "BaseT_Site")
    
    summaryData <- select(summaryData, BaseT_Site, Location, BaseT_Date)
    colnames(summaryData) <- c("BaseT_Site", "Location", "Date")
    
    
    summaryData
    
    
    
  }) #Temp function that fulfills same purpose as storeData but with real data
  
  sortSum <- reactive({
    myData <- allData() #Temporary function
    
    if (is.null(myData))
      return(NULL)
    
    
    minYr <- min(year(myData$Date))
    a <- week(myData$Date) + (year(myData$Date)-minYr)*52
    a <- data.frame(a - min(a) + 1)
    colnames(a) <- "WeekNo"
    myData <- bind_cols(myData, a)
    myData <- bind_rows(myData, c ())
    myData
    
  })
  
  siteSum <- reactive({  #Returns a data frame with cumulative week values
    
    myData <- sortSum()
    sumData <- myData %>%  group_by(BaseT_Site, Location, WeekNo) %>% summarise(Total = n())
    sumData <- sumData %>% group_by(BaseT_Site, Location) %>% mutate(cum = cumsum(Total))
    sumData
    
  })
  
  filterSum <- reactive({ 
    
    myData <- allData()
    
    dateRange <- input$monitorPeriod
    lower <- dateRange[1]
    upper <- dateRange[2]
    
    myData <- filter(myData, Date >= lower & Date <= upper)
    
    myData <-  myData %>% group_by(Location) %>% summarise(Total = n())
    myData
    
  })
  
  
  
  siteOverview <- reactive({
    
    
    if (is.null(input$dataInput))
      shinyalert(title = "Warning!", text = "Please ensure that the baseline parameters and recruitment data has been uploaded before viewing Monitoring information", type = "error")
 
    
    
    siteData <- siteSum()
    
      
    
    siteData <- siteData %>% group_by(BaseT_Site) %>% summarise(Total = max(cum))
    
    siteData <- left_join(siteData, vals$baseData, by = "BaseT_Site")
    siteData <- siteData[,c(1,3,4,5,2,6)]
    
    siteData
    
  })
  
  trialSum <- reactive({
    
    myData <- sortSum()
    sumData <- myData %>%  group_by(WeekNo) %>% summarise(Total = n())
    sumData <- sumData %>% mutate(cum = cumsum(Total))
    
  })
  
  listLocation <- reactive({ #Returns vector of locations
    
    levels(vals$baseData$Location)
    
  })
  
  observeEvent (input$projData, {   #Updates the filter options according to the file that is inserted (!!! Link when use upload)
    
   x <- listLocation()
    
  updateSelectInput(session, "locSelect", "Select which locations to view", choices = x, selected = x)

    
  })
  
  trialRecrRate <- reactive({
    
    myData <- sortSum()
    myData <- myData %>% group_by(WeekNo) %>% summarise(Total = n())
    
    siteObs <- max(myData$WeekNo)
    
    freqData <- myData %>% group_by(Total) %>% summarise(Values = n())
    
    lambda <- sum(freqData$Values * freqData$Total)/siteObs
    
  })
  
  siteRecrRate <- reactive({
    
    myData <- sortSum()
    myData <- myData %>%  group_by(BaseT_Site, Location, WeekNo) %>% summarise(Total = n())
    
    lambdaVec <- rep(NA, times = dim(vals$baseData)[1])
    
    siteParam <<- data.frame(data.frame(BaseT_Site = vals$baseData[1], recruitRate = rep(NA, nrow(vals$baseData))))
    
    for (i in 1:dim(vals$baseData)[1]) {
      workWith <- filter(myData, BaseT_Site == i)
      workWith$WeekNo <- workWith$WeekNo - min(workWith$WeekNo) + 1
      workWith <- workWith %>% group_by(WeekNo) %>% summarise(Values = sum(Total))
      
      siteObs <- max(workWith$WeekNo)
      
      freqData <- workWith %>% group_by(Values) %>% summarise(Total = n())
      
      
      siteParam[i,2] <<- sum(freqData$Values * freqData$Total)/siteObs
    }
    
  })
  
    ## Trial Page =================================
  
  siteSim <- reactive({
    
    siteInfo <- siteOverview()
    baseInfo <- data.frame(BaseT_Site = siteInfo$BaseT_Site, sitePend = siteInfo$RecruitGoal - siteInfo$Total)
    siteDur <- data.frame(BaseT_Site = siteInfo$BaseT_Site, lowerCI = rep(NA, times = nrow(baseInfo)), upperCI = rep(NA, times = nrow(baseInfo)))
    siteRecrRate()
    
    for (i in 1: nrow(baseInfo)){
      
      simParam <- data.frame(recruitRate = siteParam$recruitRate[i], RecruitGoal = baseInfo$sitePend[i])
      simVec <- runSimulation(simParam)
      vec_CI <- calc_CI(simVec, 0.95) 
      
      siteDur$lowerCI[i] <- vec_CI[1]
      siteDur$upperCI[i] <- vec_CI[2]
    }
    
    siteDur
    
    
  })
  
  output$valueRec <- renderValueBox({
    
    if (is.null(input$dataInput))
      return(valueBox(value = paste("0", " out of", "0"), subtitle = "Total number of participants recruited", color = "teal", width = 12, icon = icon("users")))
    
    totalRecr <- dim(allData())[1]
    baseRecr <- sum(vals$baseData$RecruitGoal)
    
    valueBox(value = paste(totalRecr, " out of", baseRecr), subtitle = "Total number of participants recruited", color = "teal", width = 12, icon = icon("users"))
  })
   
  output$expDur <- renderValueBox({
    
    if (is.null(input$dataInput))
      return(valueBox(value = paste("0 ", " weeks"), subtitle = "Expected remaining recruitment duration", color = "purple", width = 12, icon = icon("clock-o")))
     
    
    siteCI <- siteSim()
    
    i <- which.max(siteCI$upperCI)

    valueBox(value = paste(round(siteCI$lowerCI[i], 2), " - ", round(siteCI$upperCI[i], 2), " weeks"), subtitle = "Expected remaining recruitment duration", color = "purple", width = 12, icon = icon("clock-o"))
  })
  
  output$detSite <- renderValueBox({
    
    if (is.null(input$dataInput)){
      shinyalert(title = "Warning!", text = "Please ensure that the baseline parameters and recruitment data has been uploaded before viewing Monitoring information", type = "error")
      return(valueBox(value = paste("..."), subtitle = "Determinative site", color = "light-blue", width = 12, icon = icon("building")))
    }
    
    siteCI <- siteSim()
    i <- which.max(siteCI$upperCI)
    i <- which(vals$baseData$BaseT_Site == siteCI$BaseT_Site[i])
    
    valueBox(value = paste(vals$baseData$Location[i]), subtitle = "Determinative site", color = "light-blue", width = 12, icon = icon("building"))
  })
  
  output$recrGraph <- renderPlotly({ #Create the plot
    
    if (is.null(input$dataInput))
      return(plot_ly() %>% layout(title = "Recruitment Curve", xaxis = list(title = "Week Number"), yaxis = list(title = "Cumulative Recruitment per Week")))
      #Per Site !!!

      #Overall !!!
    dataSet <- trialSum()



    plot_ly(dataSet, x = ~WeekNo, y = ~cum ,  type = "scatter", mode = "lines", fill = "tozeroy", width = NULL ) %>%
      layout(title = "Recruitment Curve", xaxis = list(title = "Week Number"),
             yaxis = list(title = "Cumulative Recruitment per Week"))
    
    
  })
  

 
 output$lambda <- renderText({
   paste0(trialRecrRate())
 })
 
    ## Site Page ================================= 
 
  output$siteMap <- renderLeaflet({
    if (is.null(input$dataInput)) {
       shinyalert(title = "Warning!", text = "Please ensure that the baseline parameters and recruitment data has been uploaded before viewing Monitoring information", type = "error")
       return(leaflet() %>%
                addTiles() %>%
                setView(lng=19.2861, lat=-33.9859 , zoom=8.7))}
    
    
    #Explore circlemarkers and colorfactors for the map - https://rstudio.github.io/leaflet/markers.html
    siteInfo <- siteOverview()
    
    siteRecrRate()
    
    siteInfo <- left_join(siteInfo, siteParam, by = "BaseT_Site")
    
    siteInfo$hoverText <- mapply(
      function(site, tot, goal, rate) {
        htmltools::HTML(
          sprintf(
            "<div style='font-size:12px;width:220px;float:left'>
              <span style='font-size:18px;font-weight:bold'>%s</span>
              <br/><br/>
              <div style='width:70%%'>
                <span style='float:left'>Total Recruited:</span>
                <span style='float:right'>%s</span>
                <br/>

                <span style='float:left'>Weekly Recruit Rate:</span>
                <span style='float:right'>%s</span>
                <br/>
              </div>

              <div style='width:90%%'>
                <span style='color:#a9d18e;float:left'>%s%%</span>
                <span style='float:right'>%s</span>
                <br clear='all'/>
                <span style='background:#a9d18e;width:%s%%;float:left'>&nbsp;</span>
                <span style='background:#e2f0d9;width:%s%%;float:right'>&nbsp;</span>

              </div>
            </div>",
            site,tot, round(rate,4) , round(tot/goal*100,0), goal, tot/goal*100, (1 - tot/goal)*100
          )
        )
      },
      siteInfo$Location, siteInfo$Total, siteInfo$RecruitGoal, siteInfo$recruitRate, SIMPLIFY = FALSE)
   
    filterRef <- input$warnNo
    
    siteInfo$perc <- round(siteInfo$Total/siteInfo$RecruitGoal*100,0)
    
    #insert a if statement... If the dropdown is recruit goal then... else if ddl is recruit rate then...
    icon.Yea <- makeAwesomeIcon(icon = "record", markerColor = ifelse(siteInfo$perc >= filterRef ,"cadetblue", "red"))
    
     m <- leaflet(data = siteInfo) %>%
          addTiles() %>%
            setView(lng=19.301292, lat=-33.363399, zoom=7) %>% 
              #addPopups(lng = ~Long, lat = ~Lat, popup =  ~paste(Location, sep = "<br/>", as.character(Long))) #%>%
              addAwesomeMarkers(lng = ~Longitude, lat = ~Latitude, label =  ~hoverText, icon = icon.Yea , layerId = ~Location) 
      
    m
  })
  
 output$sitePlot <- renderPlotly({
   
   
   siteData <- siteOverview()

   plot_ly(siteData, x = ~RecruitGoal, y = ~Location, type = "bar", name = "Recruitment Goal", orientation = "h", color = I("#EADEDB"), hoverinfo = "none") %>% 
     add_trace(x = ~Total,  name = 'Current Recruitment', color = ~Location, hoverinfo = "none") %>%
     layout(xaxis = list(title = 'Count'), yaxis = list(title = "Site Name", type = "category"),
            barmode = "overlay", plot_bgcolor = "rgba(245, 246, 249, 1)", margin = list (l = 200, t = 50)) %>% 
     add_annotations(xref = "x", yref = "y",
                     x = ~RecruitGoal + 6, y = ~Location,
                     text = paste(siteData$Total, "out of", siteData$RecruitGoal, " (", round(100 * siteData$Total/siteData$RecruitGoal, 0), "% )"),
                     font = list(family = "Arial", size = 12),
                     showarrow = FALSE)
     
 })
 
 
 siteBase <- reactive({
   
   siteNo <- filter(vals$baseData, Location == input$siteSelect)
   siteNo <- siteNo$BaseT_Site[1]
   
   baseLine <- vals$baseData
   baseLine <- filter(baseLine, BaseT_Site == siteNo)
   baseLine$RecruitGoal[1]
   
 })
 
 siteRecr <- reactive({
   
   siteNo <- filter(vals$baseData, Location == input$siteSelect)
   siteNo <- siteNo$BaseT_Site[1]
   
   curRecr <- siteOverview()
   curRecr <- filter(curRecr, BaseT_Site == siteNo)
   curRecr$Total[1]
   
 })
 
 updateNum <- reactive({
   
   siteNo <- filter(vals$baseData, Location == input$siteSelect)
   siteNo <- siteNo$BaseT_Site[1]
   
   baseLine <- siteBase()
   curRecr <- siteRecr()

   
   curRate <- filter(siteParam, BaseT_Site == siteNo)
   
   updateNumericInput(session, "siteRate", value = round(curRate$recruitRate, 4))
   updateNumericInput(session, "sitePend", value = baseLine - curRecr)
 })
 
 observeEvent(input$siteMap_marker_click, {
   a <- input$siteMap_marker_click$id
   updateSelectInput(session, "siteSelect", selected = a)
   
 })
 
 observeEvent(event_data("plotly_click"), {
   a <- event_data("plotly_click")
   updateSelectInput(session, "siteSelect", selected = a$y)
   
   
 })
 
 output$vbSiteRec <- renderValueBox({
   
   if (is.null(input$dataInput))
     return(valueBox(value = paste("0", " out of ", "0", " participants"), subtitle = "Number of recruitments", icon = icon("users"), color = "teal" ))
   
   baseLine <- siteBase()
   curRecr <- siteRecr()
 
   
   valueBox(value = paste(curRecr, " out of ", baseLine, " participants"), subtitle = "Number of recruitments", icon = icon("users"), color = "teal" )
   
 })
 
 output$vbSiteRate <- renderValueBox({
   
   if (is.null(input$dataInput))
     return(valueBox(value = paste("0"), subtitle = "Number of recruitments per week", icon = icon("line-chart"), color = "light-blue"))
   
   
   siteNo <- filter(vals$baseData, Location == input$siteSelect)
   siteNo <- siteNo$BaseT_Site[1]
   
   curRate <- filter(siteParam, BaseT_Site == siteNo)
   
   valueBox(value = paste(round(curRate$recruitRate, 4)), subtitle = "Number of recruitments per week", icon = icon("line-chart"), color = "light-blue")
   
 })
 
 output$vbSiteEnd <- renderValueBox({
   
   if (is.null(input$dataInput))
     return(valueBox(value = paste("0", " weeks" ), subtitle = "Expected remaining recruitment duration", icon = icon("clock-o"), color = "purple"))
            
   
   simParam <- data.frame(recruitRate = input$siteRate, RecruitGoal = input$sitePend)
    
   simVec <- runSimulation(simParam)
   CI <- calc_CI(simVec, input$siteplanProb/100) 
   
   valueBox(value = paste(round(CI[1], 2), " - ", round(CI[2], 2), " weeks" ), subtitle = "Expected remaining recruitment duration", icon = icon("clock-o"), color = "purple")
   
 })
 
 
 output$siteGraph <- renderPlotly({
   
   if (is.null(input$dataInput))
     return(plot_ly() %>% layout(title = "Recruitment Status at Each Site", xaxis = list(title = "Week Number"), yaxis = list(title = "Cumulative Recruitment per Week")))
   
   siteName <- input$siteSelect
   
   dataSet <- siteSum()
   dataSet <- filter(dataSet, Location == siteName)

   updateNum()
   
   b <- plot_ly(dataSet, x = ~WeekNo, y = ~cum , color = ~Location, type = "scatter", mode = "lines", fill = 'tozeroy') %>%
     layout(title = "Recruitment Status at Each Site", xaxis = list(title = "Week Number"),
            yaxis = list(title = "Cumulative Recruitment per Week"))
   b
   
 })
  
  ## Baseline Page ---------------------------------
 

 observeEvent(input$trialBase,{
   
   vals$upLogic <- FALSE
 })
 
  readBase <- reactive({
      
      baseInFile <- input$trialBase
      if(is.null(baseInFile)) 
        return(NULL)
      
      tempBase <- read.xlsx(baseInFile$datapath, 1,  header=TRUE)
      
      tempBase
    
  })
  
  output$baseTable <- renderRHandsontable({

    if (vals$upLogic)
      tempData <- NULL
      
    else (tempData <- readBase())
    
    
    if (!(is.null(tempData)))
        vals$baseData <<- tempData
    
    rhandsontable(vals$baseData) %>% hot_col(1, format = "0") %>% hot_col(c(3, 4), format = "0.000000") %>% hot_col(2, readOnly = FALSE)
    
  })
  
  observeEvent(input$updateTable,{
    
    
    udTable <- hot_to_r(input$baseTable)

    vals$baseData <<- udTable
    vals$upLogic <- TRUE
    updateSelectInput( session, "siteSelect", label = "Select a site from the dropdown or by clicking the desired site from the map:", choices = levels(vals$baseData$Location))
    
  })
  
 output$dlBaseline <- downloadHandler(
   filename = paste("Trial Baseline Data ", Sys.Date(), ".xlsx", sep="")
   ,
   content = function(file){
     
     write.xlsx(vals$baseData, file, row.names = FALSE)
     
   }
   
 )
  
 session$onSessionEnded(stopApp)
 
}

# Run the application ###########################################
shinyApp(ui = ui, server = server)

