setwd("~/NYDSA/Projects/Shiny")
library(tidyverse)

# import & clean airports ####
airport <- read.csv("airport_codes.csv",stringsAsFactors = TRUE)
airport$initial <- substring(airport$ident, first = 1, last = 1)
airport <- airport %>% filter(initial == "K" & 
                                type != "closed" & 
                                iso_country == "US") %>%
  rename(airport_code = ident) %>%
  select(airport_code, type, name, iso_region, municipality)

UStates <- read.csv("US_states.csv",stringsAsFactors = TRUE)
UStates <- UStates %>% rename(iso_region = state)

airport <- airport %>% left_join(., UStates, by="iso_region") 
  #%>% unique()

#write.csv(airport,"cleaned_airport.csv")

# import & clean cases ####

UStates <- UStates %>% rename(state=state.name)

cases <- read.csv("cases_by_state.csv",stringsAsFactors = TRUE)
cases <- cases %>% select(-fips) %>% right_join(.,UStates,by="state") %>%
  select(-iso_region) %>% rename(region=state.region)
cases$date <- as.Date(cases$date)

stateList = unique(cases$state)
cases$new_cases = "hello"
final <- cases[1,]

for (item in stateList){
  temp <- cases %>% filter(state==item)
  temp$new_cases[1] = temp$cases[1]
  
    for (i in 2:length(temp$date)){
      
      if ((temp$cases[i]-temp$cases[i-1])<0){
        temp$new_cases[i]=0
      }else {temp$new_cases[i] = temp$cases[i]-temp$cases[i-1]}}
      
  final <- rbind(temp,final)
}
final <- final %>% filter(new_cases != "hello") %>% unique()
write.csv(final,"cleaned_cases.csv")
rm(temp)

# combine master flights & airport ####
setwd('./datasets')
master_flights=read.csv("US_master_flights.csv",stringsAsFactors = TRUE)

US_flights <- master_flights %>% select(-X,date,origin,destination) %>%
  rename(airport_code = origin) %>% 
  inner_join(.,airport,by="airport_code") %>%
  rename(origin = airport_code, 
         origin.type = type, 
         origin.airport_name=name,
         origin.state=state.name, 
         origin.municipality=municipality,
         origin.region = state.region) %>% select(-iso_region)
US_flights$origin <- substring(US_flights$origin,first=2,last=4)

US_flights <- US_flights %>% rename(airport_code = destination) %>%
  inner_join(.,airport,by="airport_code") %>%
  rename(dest = airport_code, 
         dest.type = type, 
         dest.airport_name=name,
         dest.state=state.name, 
         dest.municipality=municipality,
         dest.region = state.region) %>% select(-iso_region)
US_flights$dest <- substring(US_flights$dest,first=2,last=4)

setwd('..')
write.csv(US_flights,"US.Airport_flights.csv")

# test plots ####

timeSeries <- flights %>% arrange(date) %>% group_by(date) %>% count() 
timeSeries$date <- as.Date(timeSeries$date)
ggplot(data=timeSeries, aes(x=date, y=n))+geom_line()+
  labs(title="daily flights", x="date", y="# of flight")

caseTrend <- cases %>% group_by(date,region) %>% summarise(total=sum(cases))
ggplot(data=caseTrend, aes(x=date, y=total))+geom_line(aes(color=region))

flights %>% group_by(month, origin.region) %>% 
  summarise(n=n()) %>% 
  ggplot(.,aes(x=month, y=n)) + geom_bar(stat="identity",aes(fill=origin.region))

cases %>% filter(state=="Texas") %>% arrange(date) %>%
  ggplot(.,aes(x=date))+
  geom_area(aes(y=cases),fill="grey")+
  geom_bar(stat="identity",aes(y=new_cases,fill=new_cases))
  scale_y_continuous(name="cumulative cases",
                     sec.axis=sec_axis(~./10000, name="new cases"))
