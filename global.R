library(shiny)
library(tidyverse)

flights <- read.csv("US.Airport_flights.csv", stringsAsFactors = TRUE)
flights <- flights %>% select(-X) %>% mutate(month=as.numeric(format(date,"%m")),
                                             date=as.Date(date))

cases <- read.csv("cleaned_cases.csv", stringsAsFactors = TRUE)
cases <- cases %>% filter(date<"2020-11-1" & region != "Noncontiguous") %>%
  mutate(date=as.Date(date), cases=as.numeric(cases))

