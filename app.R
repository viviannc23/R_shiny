library(shiny)
library(tidyverse)

#import data ####
# setwd("~/NYDSA/Projects/ShinyApp")
# flights <- read.csv("US.Airport_flights.csv", stringsAsFactors = TRUE)
# flights <- flights %>% select(-X) %>% mutate(date=as.Date(date)) %>%
#     mutate(month=as.numeric(format(date,"%m")))
# flights_large <- flights %>% filter(origin.type == "large_airport" & dest.type == "large_airport") %>%
#    select(-origin.type,-dest.type)
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
            certain states. Flights list is not limited to commerical traffic, as it may include private jets,
            cargos, even servalliance jets. Current dataset has a filter for flights departing and arriving within
            the US, and excluding heliports and flights that takes off and lands at the same airport on the same day.",
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
                
                # Overall tab ####
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
                         
                         plotOutput("allFlights", width = "94%"),
                         plotOutput("allCases")
                         
                         ), 
                # Regions tab ####
                tabPanel(h3("By Region"), 
                         br(),
                         
                         plotOutput("flightsByRegion"),
                         plotOutput("casesByRegion")
                         
                         ),
                
                # State tab ####
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
                
                # Airport tab ####
                tabPanel(h3("By Airport"), 
                         br(),
                         fluidRow(
                             column(4,selectizeInput(inputId = "origin",
                                                 label = h4("Select Origin"),
                                                 choices = sort(unique(flights_large$origin)),
                                                 selected = "JFK")),
                             column(4,selectizeInput(inputId = "dest",
                                                 label = h4("Select Destination"),
                                                 choices = unique(flights_large$dest))),
                             column(4,br(),br(),br(),"large, major airports only")
                             ),
                         
                         # 
                         # selectInput("stateList", "Choose an airport:",
                         #             list(`East Coast` = list("NY", "NJ", "CT"),
                         #                  `West Coast` = list("WA", "OR", "CA"),
                         #                  `Midwest` = list("MN", "WI", "IA"))),

                         plotOutput("flightsByAirport")
                         )
                )
            )
        )
    )


# SERVER ####
server <- function(input, output, session) {

    observe({
        Dest <- flights_large %>% filter(origin==input$origin) %>% 
            select(dest) %>% unique() 
        updateSelectizeInput(
            session, "dest",
            choices = Dest[,1])
    })
    
    output$allFlights <- renderPlot({
        flights %>% filter(date >= input$dates[1] & date <= input$dates[2]) %>% 
            group_by(date) %>% count() %>%
            ggplot(., aes(x=date, y=n))+geom_line(size=1)+
            labs(title="Flight trends",y="daily flights")+
            theme(plot.title = element_text(size = rel(2)))
    })
    
    output$allCases <- renderPlot({
        scaleFactor = 50
        cases %>% filter(date >= input$dates[1] & date <= input$dates[2]) %>% 
            group_by(date) %>% summarise(n=sum(new_cases),
                                               total=sum(cases)) %>% 
            ggplot(.,aes(x=date))+
            geom_bar(aes(y=n*scaleFactor,fill=n),stat="identity") +
            geom_line(aes(y=total), size=1, color="red") +
            ggtitle("COVID cases") +
            scale_y_continuous(
                name = "cumulative cases",
                sec.axis = sec_axis(trans=~./scaleFactor, name ="new cases")) +
            scale_fill_continuous(name = NULL) +
            theme_bw()+
            theme(plot.title = element_text(size = rel(2)),
                  axis.title.y = element_text(color = "red", size=13),
                  axis.title.y.right = element_text(color="black", size=13),
                  legend.position = c(.05, .95),
                  legend.justification = c("left", "top"),
                  )
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
            ggplot(., aes(x=date, y=total))+geom_line(aes(color=region),size=0.8)+
            labs(y="cumulative cases") + theme_bw()
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
            labs(y=NULL)+
            scale_fill_continuous(name = "new cases") +
            theme_bw() + 
            theme(legend.position = "left")
            
    })
    
    output$flightsByAirport <- renderPlot({
        flights_large %>% filter(origin == input$origin & dest == input$dest) %>% 
            group_by(date) %>% count() %>% 
            ggplot(.,aes(x = date, y = n))+geom_bar(stat="identity") +
            labs(title = "Daily flights for selected route", y="number of flights") +
            theme(plot.title = element_text(size = rel(2)))

    })

}

shinyApp(ui = ui, server = server)

