library(RCurl)
library(tidyverse)
###" Downloading data from github
confirmed_sheet <- RCurl::getURL("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv")
deaths_sheet <- RCurl::getURL("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv")
recovered_sheet <-  RCurl::getURL("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_recovered_global.csv")
#### Raw content to dataframe using readr
COV19_confirmed<-read_delim(confirmed_sheet, delim=',')
COV19_deaths<-read_delim(deaths_sheet, delim=',')
COV19_recovered<-read_delim(recovered_sheet, delim=',')
#######
if (ncol(COV19_confirmed)>4 & nrow(COV19_confirmed)!=0){
  COV19_confirmed<-COV19_confirmed %>% select(-`Province/State`)
  COV19_deaths<-COV19_deaths %>% select(-`Province/State`)
  COV19_recovered<-COV19_recovered %>% select(-`Province/State`)
  #### Wider ton longer form
  COV19_confirmed<- COV19_confirmed%>% pivot_longer(cols = -c(`Country/Region`,Lat,Long),names_to = "date",values_to = "num_cases")
  COV19_deaths<- COV19_deaths%>% pivot_longer(cols = -c(`Country/Region`,Lat,Long),names_to = "date",values_to = "num_cases")
  COV19_recovered<- COV19_recovered%>% pivot_longer(cols = -c(`Country/Region`,Lat,Long),names_to = "date",values_to = "num_cases")
  #### Unique data for all countries using group by
  COV19_confirmed<- COV19_confirmed%>%select(-c(Long,Lat)) %>% group_by(`Country/Region`,date)%>% summarise(num_cases=sum(num_cases)) %>% rename(country=`Country/Region`)
  COV19_deaths<- COV19_deaths%>% select(-c(Long,Lat)) %>% group_by(`Country/Region`,date)%>% summarise(num_cases=sum(num_cases)) %>% rename(country=`Country/Region`)
  COV19_recovered<- COV19_recovered%>%select(-c(Long,Lat)) %>% group_by(`Country/Region`,date)%>% summarise(num_cases=sum(num_cases)) %>% rename(country=`Country/Region`)
  ######
  COV19_confirmed$date<-lubridate::mdy(COV19_confirmed$date)
  COV19_deaths$date<-lubridate::mdy(COV19_deaths$date)
  COV19_recovered$date<-lubridate::mdy(COV19_recovered$date)
  
  #### Calculate the number of new cases by day
  COV19_confirmed<- COV19_confirmed %>% group_by(country,date)%>% mutate(nb_day_cases=0) 
  COV19_deaths<- COV19_deaths %>% group_by(country,date)%>% mutate(nb_day_cases=0) 
  COV19_recovered<- COV19_recovered %>% group_by(country,date)%>% mutate(nb_day_cases=0) 
  #### CONFIRMED
  temp_database <- COV19_confirmed
  temp_database<- temp_database[0,]
  countries<-unique(COV19_confirmed$country)
  pb <- progress::progress_bar$new(format = "(:spin) [:bar] :percent",
                                   clear = FALSE, width = 60,total = length(countries))
  cat("Calulating the number of confirmed cases per day \n")
  for (i in 1:length(countries)) {
    pb$tick()
    temp<- COV19_confirmed %>% dplyr::filter(country==countries[i]) %>% arrange(date)
    for (j in 2:nrow(temp)){
      temp$nb_day_cases[j]<-temp$num_cases[j]-temp$num_cases[j-1]
    }
    temp_database <-temp_database%>% bind_rows(temp)
  }
  COV19_confirmed<-temp_database
  #####
  #### DEATHS
  temp_database <- COV19_deaths
  temp_database<- temp_database[0,]
  countries<-unique(COV19_deaths$country)
  pb <- progress::progress_bar$new(format = "(:spin) [:bar] :percent",
                                   clear = FALSE, width = 60,total = length(countries))
  cat("Calulating the number of death cases per day \n")
  for (i in 1:length(countries)) {
    pb$tick()
    temp<- COV19_deaths %>% dplyr::filter(country==countries[i]) %>% arrange(date)
    for (j in 2:nrow(temp)){
      temp$nb_day_cases[j]<-temp$num_cases[j]-temp$num_cases[j-1]
    }
    temp_database <-temp_database%>% bind_rows(temp)
  }
  COV19_deaths<-temp_database
  #### RECOVERED
  temp_database <- COV19_recovered
  temp_database<- temp_database[0,]
  countries<-unique(COV19_recovered$country)
  pb <- progress::progress_bar$new(format = "(:spin) [:bar] :percent",
                                   clear = FALSE, width = 60,total = length(countries))
  cat("Calulating the number of recovered cases per day \n")
  for (i in 1:length(countries)) {
    pb$tick()
    temp<- COV19_recovered %>% dplyr::filter(country==countries[i]) %>% arrange(date)
    for (j in 2:nrow(temp)){
      temp$nb_day_cases[j]<-temp$num_cases[j]-temp$num_cases[j-1]
    }
    temp_database <-temp_database%>% bind_rows(temp)
  }
  COV19_recovered<-temp_database
  #### Cleaning the environment
  rm(temp,temp_database,confirmed_sheet,deaths_sheet,recovered_sheet,pb,i,j,countries)
  
  COV19_confirmed<- COV19_confirmed %>% rename(nb_conf_cases=num_cases,nb_conf_day_cases=nb_day_cases)
  COV19_deaths<- COV19_deaths %>% rename(nb_death_cases=num_cases,nb_death_day_cases=nb_day_cases)
  COV19_recovered<- COV19_recovered %>% rename(nb_recov_cases=num_cases,nb_recov_day_cases=nb_day_cases)
  
  COV19_data<- inner_join(COV19_confirmed,COV19_deaths,by=c('country','date'))
  COV19_data<- inner_join(COV19_data,COV19_recovered,by=c('country','date'))
  
  write_excel_csv2(COV19_data,"Data/COV19_data.csv")
  rm(COV19_confirmed,COV19_deaths,COV19_recovered,COV19_data)
  } else cat("Problem with github links")


