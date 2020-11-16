library(shiny)

# UI ####
ui <- fluidPage(
    #theme = "bootstrap.css",
    titlePanel("US domestic flights 2020 vs COVID cases"),

    sidebarLayout(
        sidebarPanel(
            h3("Overview"),
            "The purpose of this app is to investigate the effect of COVID on US air traffic in 2020.",
            br(),
            h3("Data sources"),
            "Flight data is crowdsourced air traffic data from The OpenSky Network 2020:"
            #html("https://zenodo.org/record/4266938#.X66h8C-cZQI")
            
            ),

        mainPanel(
            tabsetPanel(
                tabPanel("Overall", 
                         
                         dateRangeInput("dates", label = h4("Select Date"),
                                        min = "2020-01-01", max = "2020-10-31",
                                        start = "2020-01-01", end = "2020-10-31"),
                         
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
                                        label = "Select State",
                                        choices = unique(cases$state)),
                         
                         plotOutput("flightsByState"),
                         plotOutput("casesByState")
                        ),
                
                tabPanel("By Airport", 
                         
                         selectizeInput(inputId = "origin",
                                        label = "Select Origin",
                                        choices = unique(flights$origin)),
                         selectizeInput(inputId = "destination",
                                        label = "Select Destination",
                                        choices = unique(flights$dest)),
                         selectInput("state", "Choose a state:",
                                     list(`East Coast` = list("NY", "NJ", "CT"),
                                          `West Coast` = list("WA", "OR", "CA"),
                                          `Midwest` = list("MN", "WI", "IA"))
                         )
                         #plotOutput("flightsByState"),
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
            ggplot(., aes(x=date, y=n))+geom_line()+labs(y="daily flights")
    })
    
    output$allCases <- renderPlot({
        cases %>% filter(date >= input$dates[1] & date <= input$dates[2]) %>%
            group_by(date) %>% 
            summarise(total=sum(cases)) %>% 
            ggplot(., aes(x=date, y=total))+geom_line()
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
        flights %>% filter(origin.state==input$state)
            group_by(month) %>% 
            summarise(n=n()) %>% 
            ggplot(.,aes(x=month, y=n)) + 
            geom_boxplot()
    })
    
    output$casesByState <- renderPlot({
        cases %>% group_by(date) %>% 
            summarise(total=sum(cases)) %>%
            ggplot(., aes(date, total))+geom_area(fill="grey")
    })
}

shinyApp(ui = ui, server = server)
