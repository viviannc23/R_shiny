library(shiny)
library(tidyverse)

#import data ####
# setwd("~/NYDSA/Projects/ShinyApp")
# flights <- read.csv("US.Airport_flights.csv", stringsAsFactors = TRUE)
# flights <- flights %>% select(-X) %>% mutate(date=as.Date(date)) %>%
#     mutate(month=as.numeric(format(date,"%m")))
# 
# cases <- read.csv("cleaned_cases.csv", stringsAsFactors = TRUE)
# cases <- cases %>% mutate(date=as.Date(date), cases=as.numeric(cases)) %>%
#     filter(date<"2020-11-1" & region != "Noncontiguous") %>% select(-X)

# UI ####
ui <- fluidPage(
    theme = "bootstrap.min.css",
    
    titlePanel(h1("US domestic flights 2020 vs COVID cases")), br(),

        sidebarLayout(
        
        sidebarPanel(
            #h3("Overview"),
            h4(tags$b("The purpose of this app is to investigate the effect of COVID on US air traffic in 2020,
            or vice versa.")),
            br(),
            h3("Data sources & Limitations"),
            tags$a(href="https://zenodo.org/record/4266938#.X66h8C-cZQI", "Flights dataset"),
            " is crowdsourced air traffic data from The OpenSky Network 2020. Since this dataset is crowdsourced,
            the exhausativeness of the list may be incomplete - hence there appears to be very few air traffic for
            certain states. Flights list is not limited to commerical traffic, as it may include servillance
            helicopters and private planes. Current dataset has a filter for flights departing and arriving within
            the US, and does not include flights that takes off and lands at the same airport on the same day. ",
            br(),br(),
            tags$a(href="https://github.com/CSSEGISandData/COVID-19", 
                   "COVID cases"), 
            " data is obtained from John Hopkins Center for Systems Science and Enginnering Github page. ",
            br(),br(),  
            
            h3("Further Work"),
            tags$p("Flights dataset contains all aviation traffic around the globe; cases and flights can be 
            updated until end of 2020 as they become available. An expansion to the current study is to 
            explore the international trends, also including how travel restrictions affects 
            international air traffic."),
            
            br(),br(),
            img(src="US_Regions.gif",width="100%"),
            ),

        mainPanel(
            tabsetPanel(

                tabPanel(h3("Overall"), 
                         br(),
                         fluidRow(br(),                           
                             column(4,
                                 dateRangeInput("dates", label = h4("Select Date"),
                                        min = "2020-01-01", max = "2020-10-31",
                                        start = "2020-02-01", end = "2020-08-31")),
                             column(8, br(),br(),
                                    "data available from 2020/1/1 until 2020/10/31")
                             
                             ),
                         
                         plotOutput("allFlights"),
                         plotOutput("allCases")
                         
                         ), 
                
                tabPanel(h3("By Region"), 
                         br(),
                         
                         plotOutput("flightsByRegion"),
                         plotOutput("casesByRegion")
                         
                         ),
                
                tabPanel(h3("By State"), 
                         br(),
                         fluidRow(
                             radioButtons(
                                 "direction",
                                 label = h4("Flight direction"),
                                 choices = list("Outbound/ originating from" = "origin.state",
                                                "Inbound/ arriving into" = "dest.state"),
                                 selected = "origin.state"
                             ),
                             
                             selectizeInput(
                                 inputId = "state",
                                 label = h4("Select State"),
                                 choices = sort(unique(cases$state)),
                                 selected = "New York"
                             ),

                         ), 
                         
                         plotOutput("flightsByState"),
                         plotOutput("casesByState")
                        ),
                
                tabPanel(h3("By Airport"), 
                         br(),
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
server <- function(input, output) {

    output$allFlights <- renderPlot({
        flights %>% filter(date >= input$dates[1] & date <= input$dates[2]) %>% 
            group_by(date) %>% count() %>%
            ggplot(., aes(x=date, y=n))+geom_line()+
            labs(title="Flight trends",y="daily flights")+
            theme(plot.title = element_text(size = rel(2)))
    })
    
    output$allCases <- renderPlot({
        # cases %>% filter(date >= input$dates[1] & date <= input$dates[2]) %>%
        #     group_by(date) %>% 
        #     summarise(total=sum(cases)) %>% 
        #     ggplot(., aes(x=date, y=total))+geom_area(fill="grey")+
        #     labs(title="COVID cases", y="cumulative cases")+
        #     theme(plot.title = element_text(size = rel(2)))
        
        
        cases %>% filter(date >= input$dates[1] & date <= input$dates[2]) %>% 
            group_by(date) %>% summarise(n=sum(new_cases),
                                               total=sum(cases)) %>% 
            ggplot(.,aes(x=date))+
            geom_bar(aes(y=n*50,fill=n),stat="identity") +
            geom_line(aes(y=total), size=1)
    })
    
    output$flightsByRegion <- renderPlot({

        flights %>% group_by(month, origin.region) %>% 
            summarise(n=n()) %>% 
            ggplot(.,aes(x=month, y=n)) + 
            geom_bar(stat="identity",aes(fill=origin.region))+
            coord_cartesian(xlim = c(1,10)) +
            scale_x_continuous(breaks = 1:10,
                               labels=c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct")) +
            labs(y="daily flights")
    })
    
    output$casesByRegion <- renderPlot({
        cases %>% group_by(date,region) %>% 
            summarise(total=sum(cases)) %>%
            ggplot(., aes(x=date, y=total))+geom_line(aes(color=region))+
            labs(y="cumulative cases")
    })
    
    output$flightsByState <- renderPlot({
        flights %>% mutate(month = as.factor(month))
        flights %>% rename(.,city=input$direction)%>%
            filter(city == input$state) %>% 
            group_by(date,month) %>% summarise(n=n()) %>% 
            ggplot(.,aes(x = month, y = n, group = month))+geom_boxplot()+
            coord_cartesian(xlim = c(1,10)) +
            scale_x_continuous(breaks = 1:10,
                               labels=c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct")) +
            labs(y = "flights per day")
    })
    
    output$casesByState <- renderPlot({
        cases %>% filter(state==input$state) %>% arrange(date) %>%
            ggplot(.,aes(x=date)) +
            geom_bar(stat="identity",aes(y=new_cases,fill=new_cases))+
            labs(y="new cases")
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

