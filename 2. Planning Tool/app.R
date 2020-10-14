# Libraries  ---------------------------------
library(shiny)
library(shinydashboard)
library(dplyr)
library(plotly)
#library(xlsx)
library(lubridate)
library(leaflet)
library(RColorBrewer)
library(rhandsontable)
#library(rstudioapi)

# Global Variables ---------------------------------
numSim <- 1000 #Number of simulations to run
Samp <- 10
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
  title = "Trial Recruitment Planning Tool",
  titleWidth = 700
  
)

# Sidebar ---------------------------------

sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Planning", tabName = "plan", icon = icon("edit")))
    # ),
    # menuItem("Information", tabName = "info", icon = icon("info-circle"))
)

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

body <- dashboardBody(
  tabItems(
    tabItem(tabName = "plan",
            h1("Planning"),
            planPage)
            
    # ),
    # tabItem(tabName = "info",
    #         h1("Tutorials"),
    #         HTML('<iframe width="560" height="315" src="https://www.youtube.com/embed/videoseries?list=PLEESpC-GOSh9_r61fo2wbkXvULTIETWRk" frameborder="0" allow="accelerometer; autoplay; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>')
    #         
    # )
  )
)


ui <- dashboardPage(
   
  header, sidebar, body 
  
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {

  # Planning Page ---------------------------------
  
  
  
  ## Overview =================================
  normDuration <- reactive({  #Function that does all of the calculations for the trial length --- Data frame with periodic intervals?
    
    lambda <- input$planRecruitRate   
    n <- input$planSample
    prob <- input$planProb/100
    
    zVal <- qnorm(prob)
    
    f <- function(t) (lambda*t - zVal * (lambda * t)^0.5 - (n- 0.5))
    
    uniroot(f, lower = 0, upper = 1000)$root
    
  })  
  
  simDuration <- reactive({
    #numSim <- input$simRun
    lambda <- input$planRecruitRate   
    n <- ceiling(input$planSample/(input$planRetention/100))
    
    a <- data.frame(recruitRate = lambda, RecruitGoal = n)
    
    runSimulation(a)
    
    
  })
  
  
  output$durationInfo <- renderValueBox({
    valueBox(c(round(normDuration(), digits = 0), " weeks") ,"Estimated recruitment duration", icon = icon("clock-o"), color = "teal")
    
  })
  
  output$expDate <- renderValueBox({
    
    
    lambda <- input$planRecruitRate 
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
  
}

# Run the application 
shinyApp(ui = ui, server = server)
