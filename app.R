library(shiny)
library(tidyverse)

# import data ####
setwd("~/NYDSA/Projects/ShinyApp")
flights <- read.csv("US.Airport_flights.csv", stringsAsFactors = TRUE)
flights <- flights %>% select(-X) %>% mutate(date=as.Date(date)) %>%
    mutate(month=as.numeric(format(date,"%m")))

cases <- read.csv("cleaned_cases.csv", stringsAsFactors = TRUE)
cases <- cases %>% mutate(date=as.Date(date), cases=as.numeric(cases)) %>%
    filter(date<"2020-11-1" & region != "Noncontiguous") %>% select(-X)

# UI ####
ui <- fluidPage(
    theme = "bootstrap.min.css",
    titlePanel(h1("US domestic flights 2020 vs COVID cases")), br(),

        sidebarLayout(
        
        sidebarPanel(
            h3("Overview"),
            "The purpose of this app is to investigate the effect of COVID on US air traffic in 2020.",
            br(),br(),
            h3("Further Work"),
            "Flights dataset contains all aviation traffic around the globe; cases and flights can be 
            updated until end of 2020 as they become available. An expansion to the current study is to 
            explore the international trends, also including how travel restrictions affects 
            international air traffic.",
            br(),br(),
            h3("Data sources"),
            tags$a(href="https://zenodo.org/record/4266938#.X66h8C-cZQI", "Flights dataset"),
            " is crowdsourced air traffic data from The OpenSky Network 2020.\n",
            br(),
            tags$a(href="https://github.com/CSSEGISandData/COVID-19", 
                   "COVID cases"), 
            " data is obtained from John Hopkins Center for Systems Science and Enginnering Github page."
            
            ),

        mainPanel(
            tabsetPanel(

                tabPanel("Overall", 
                         
                         fluidRow(br(),                           
                             column(6,(
                                 dateRangeInput("dates", label = h4("Select Date"),
                                        min = "2020-01-01", max = "2020-10-31",
                                        start = "2020-02-01", end = "2020-08-31"))),
                             column(6, br(),br(),
                                    "data available from 2020/1/1 until 2020/10/31")
                             
                             ),
                         
                         plotOutput("allFlights"),
                         plotOutput("allCases")
                         
                         ), 
                
                tabPanel("By Region", 
                         
                         # fluidRow(radioButtons("origDest", 
                         #              label = h4("Flight Region by:"),
                         #              choices = list("Origin" = "origin.region", 
                         #                             "Destination" = "dest.region"), 
                         #              selected = "origin.region")),
                         #img(src="US_Regions.gif",width="100%"),
                         plotOutput("flightsByRegion"),
                         plotOutput("casesByRegion")
                         
                         ),
                
                tabPanel("By State", 
                         
                         selectizeInput(inputId = "state",
                                        label = h4("Select State"),
                                        choices = sort(unique(cases$state))),
                         
                         plotOutput("flightsByState"),
                         plotOutput("casesByState")
                        ),
                
                tabPanel("By Airport", 
                         
                         
                         fluidRow(radioButtons("Origin.airportSize",
                                               label = "Origin Airport Size:",
                                               choices = list("Large" = "large_airport",
                                                              "Medium" = "medium_airport",
                                                              "Small" = "small_airport"),
                                               selected = "large_airport"),
                                  selectizeInput(inputId = "origin",
                                                 label = h4("Select Origin"),
                                                 choices = unique(flights$origin))
                                  ),
                         
                         
                         fluidRow(radioButtons("Dest.airportSize",
                                               label = "Destination Airport Size:",
                                               choices = list("Large" = "large_airport",
                                                              "Medium" = "medium_airport",
                                                              "Small" = "small_airport"),
                                               selected = "large_airport"),
                                  selectizeInput(inputId = "dest",
                                                 label = h4("Select Destination"),
                                                 choices = unique(flights$dest))
                                ),
                         # selectInput("state", "Choose a state:",
                         #             list(`East Coast` = list("NY", "NJ", "CT"),
                         #                  `West Coast` = list("WA", "OR", "CA"),
                         #                  `Midwest` = list("MN", "WI", "IA"))),

                         plotOutput("flightsByAirport"),
                         #plotOutput("casesByState")
                )
            )
        )
    )
)

# SERVER ####
server <- function(input, output, session) {

    output$allFlights <- renderPlot({
        flights %>% filter(date >= input$dates[1] & date <= input$dates[2]) %>% 
            group_by(date) %>% count() %>%
            ggplot(., aes(x=date, y=n))+geom_line()+
            labs(title="flight trends",y="daily flights")
    })
    
    output$allCases <- renderPlot({
        cases %>% filter(date >= input$dates[1] & date <= input$dates[2]) %>%
            group_by(date) %>% 
            summarise(total=sum(cases)) %>% 
            ggplot(., aes(x=date, y=total))+geom_line()+
            labs(title="COVID cases", y="cumulative cases")
    })
    
    output$flightsByRegion <- renderPlot({

        flights %>% group_by(month, origin.region) %>% 
            summarise(n=n()) %>% 
            ggplot(.,aes(x=month, y=n)) + 
            geom_bar(stat="identity",aes(fill=origin.region))+
            coord_cartesian(xlim = c(1,10)) +
            scale_x_continuous(breaks = 1:10,
                               labels=c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct"))
    })
    
    output$casesByRegion <- renderPlot({
        cases %>% group_by(date,region) %>% 
            summarise(total=sum(cases)) %>%
            ggplot(., aes(x=date, y=total))+geom_line(aes(color=region))
    })
    
    output$flightsByState <- renderPlot({
        flights %>% mutate(month = as.factor(month))
        flights %>% filter(origin.state == input$state) %>% 
            group_by(date,month) %>% summarise(n=n()) %>% 
            ggplot(.,aes(x = month, y = n, group = month))+geom_boxplot()+
            coord_cartesian(xlim = c(1,10)) +
            scale_x_continuous(breaks = 1:10,
                               labels=c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct")) +
            labs(y = "flights per day")
    })
    
    output$casesByState <- renderPlot({
        cases %>% group_by(date) %>% 
            summarise(total=sum(cases)) %>%
            ggplot(., aes(date, total))+geom_area(fill="grey")
        cases %>% filter(state==input$state) %>% 
            ggplot(.,aes(x=date,y=cases)) + geom_line(stat="identity")
    })
    
    output$flightsByAirport <- renderPlot({
        flights %>% filter(origin == input$origin & dest == input$dest) %>% 
            group_by(date) %>% count() %>% 
            ggplot(.,aes(x = date, y = n))+geom_bar(stat="identity")

    })
    
    # observe({
    #     dest <- unique(flights[origin == input$origin, dest])
    #     updateSelectizeInput(
    #         session, "dest",
    #         choices = dest,
    #         selected = dest[1])
    # })
    
    # observe({
    #     origin <- unique(flights[origin.type == input$Origin.airportSize, origin])
    #     updateSelectizeInput(
    #         session, "Origin.airportSize",
    #         choices = origin,
    #         selected = origin[1])
    # })
}

shinyApp(ui = ui, server = server)
