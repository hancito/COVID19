library(readr)
library(dplyr)
library(ggplot2)
library(plotly)
library(ggdark)
library(shiny)
library(flexdashboard)
library(DT)
#####
COV19_data <- read_delim("Data/COV19_data.csv",";", escape_double = FALSE, trim_ws = TRUE)

##### formatting Date
COV19_data$date<-as.Date.character(COV19_data$date)
countries<-unique(COV19_data$country)
latest_day<-max(COV19_data$date)


### World stats
world_data<-COV19_data %>% group_by(date) %>% summarise(nb_conf_cases=sum(nb_conf_cases),nb_conf_day_cases=sum(nb_conf_day_cases),
                                                        nb_death_cases=sum(nb_death_cases),nb_death_day_cases=sum(nb_death_day_cases),
                                                        nb_recov_cases=sum(nb_recov_cases),nb_recov_day_cases=sum(nb_recov_day_cases))











