setwd("~/NYDSA/Projects/Shiny")
library(tidyverse)

# cleaning monthly data ####
df <- read.csv("flights_Jan20.csv")

df1 <- df %>% select(.,origin, destination, day) %>%
  filter(origin != "", destination != "", origin != destination)
df1$day <- as.Date(df1$day)

df1$initial <- substring(df1$origin, first = 1, last = 1) 
df1$initialDestination <- substring(df1$destination, first=1, last=1)
df2 <- df1 %>% filter(initial == "K" & initialDestination == "K") %>%
  select(origin, destination, day) %>% rename(date=day)
df2$date <- as.Date(df2$date)

write.csv(df2,"cleaned_Jan20.csv")
rm(df, df1, df2)

# import airport & cases ####
airport <- read.csv("airport_codes.csv")
airport$initial <- substring(airport$ident, first = 1, last = 1)
airport1 <- airport %>% filter(initial == "K" & 
                                type != "closed" & 
                                iso_country == "US") %>%
  rename(airport_code = ident) %>%
  select(airport_code, type, name, iso_region, municipality)

UStates <- read.csv("US_states.csv")
UStates <- UStates %>% rename(iso_region = state)

airport2 <- airport1 %>% left_join(., UStates, by="iso_region") %>%
  unique()

write.csv(airport2,"cleaned_airport.csv")

UStates <- UStates %>% rename(state=state.name)

cases <- read.csv("cases_by_state.csv")
cases <- cases %>% select(-fips) %>% right_join(.,UStates,by="state") %>%
  select(-iso_region) %>% rename(region=state.region)
cases$date <- as.Date(cases$date)
cases$state <- as.factor(cases$state)
cases$region <- as.factor(cases$region)


# compile master flights ####
Mar20 <- read.csv("cleaned_Mar20.csv")
Jul20 <- read.csv("cleaned_Jul20.csv")
Apr20 <- read.csv("cleaned_Apr20.csv")
May20 <- read.csv("cleaned_May20.csv")
Jun20 <- read.csv("cleaned_Jun20.csv")
Feb20 <- read.csv("cleaned_Feb20.csv")
Jan20 <- read.csv("cleaned_Jan20.csv")

master_flights = rbind(Mar20, Jul20, Apr20, May20, Jun20, Feb20, Jan20)
write.csv(master_flights,"master_flights.csv")
#rm(Mar20, Jul20, Apr20, May20, Jun20, Feb20, Jan20)

# clean master ####
temp <- master_flights %>% select(-X,date,origin,destination) %>%
  rename(airport_code = origin) %>% 
  inner_join(.,airport2,by="airport_code") %>%
  rename(origin = airport_code, 
         origin.type = type, 
         origin.airport_name=name,
         origin.state=iso_region, 
         origin.municipality=municipality,
         origin.region = state.region) %>% select(-state.name)
temp$origin <- as.factor(substring(temp$origin,first=2,last=4))
temp <- temp %>% rename(airport_code = destination) %>%
  inner_join(.,airport2,by="airport_code") %>%
  rename(dest = airport_code, 
         dest.type = type, 
         dest.airport_name=name,
         dest.state=iso_region, 
         dest.municipality=municipality,
         dest.region = state.region) %>% select(-state.name)
temp$dest <- as.factor(substring(temp$dest,first=2,last=4))
write.csv(temp,"master_flights.csv")

# test plots ####

timeSeries <- master_flights %>% arrange(date) %>% group_by(date) %>% count() 
timeSeries$date <- as.Date(timeSeries$date)
ggplot(data=timeSeries, aes(x=date, y=n))+geom_line()

caseTrend <- cases %>% group_by(date,region) %>% summarise(total=sum(cases))
ggplot(data=caseTrend, aes(x=date, y=total))+geom_line(aes(color=region))

