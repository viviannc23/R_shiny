setwd("~/NYDSA/Projects/Shiny")

setwd('./datasets')

library(tidyverse)

files = list.files(pattern="*.csv")

handle_each <- function(x){
  temp <- read.csv(x)

  temp <- temp %>% select(.,origin, destination, day) %>%
    filter(origin != "", destination != "", origin != destination)
  
  temp$initialOrigin <- substring(temp$origin, first=1, last=1) 
  temp$initialDest <- substring(temp$destination, first=1, last=1)
  temp$date <- as.Date(temp$day)
  
  #filter for only US flights
  temp <- temp %>% filter(initialOrigin == "K" & initialDest == "K") %>%
    select(origin, destination, date)

  return(temp)
}

master_flights = do.call(rbind, lapply(files, handle_each))

write.csv(master_flights,"US_master_flights.csv")
