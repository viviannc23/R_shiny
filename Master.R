rm(list=ls())
library(dplyr)
library(ggplot2)

setwd("~/NYDSA/Projects/Shiny/")

CovidAirTraffic <- read.csv('covid_impact_on_airport_traffic.csv',
                            stringsAsFactors = TRUE)

AirTraffic <- CovidAirTraffic %>% rename(.,ISO = ISO_3166_2)
AirTraffic$Date <- as.Date(CovidAirTraffic$Date)
AirTraffic$Country <- as.factor(CovidAirTraffic$Country)
AirTraffic$City <- as.factor(CovidAirTraffic$City)
AirTraffic$State <- as.factor(CovidAirTraffic$State)
AirTraffic$AirportName <- as.factor(CovidAirTraffic$AirportName)
AirTraffic$ISO <- as.factor(AirTraffic$ISO)

AirAU <- AirTraffic %>% filter(., Country == "Australia")
  
gAU <- ggplot(data = AirAU, aes(Date, PercentOfBaseline))+geom_line()
gAU #Australia air traffic chart
