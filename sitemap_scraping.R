library(readr)
library(tidyverse)
library(ggiraph)
library(rnaturalearth)
library(rnaturalearthdata)
library(sf)
require(data.table)

oid_data <- fread("https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/owid-covid-data.csv")
speed_data <- oid_data %>% select(date, iso_code, location, new_cases)
speed_data$new_cases[is.na(speed_data$new_cases)] <- 0
speed_data = split(speed_data, f = speed_data$location)
speed_data = lapply(speed_data, function(x){
  date = as.data.frame(x$date)
  date = date[-1,]
  diff = diff(x$new_cases)
  location = unique(x$location)
  iso_code = unique(x$iso_code)
  a = data.frame(
    iso_code = iso_code,
    location = location,
    date = date,
    difference = diff
  )
  a = a %>%
    mutate(`20-days average` = zoo::rollmean(difference, k = 20, fill = NA),
           `7-days average` = zoo::rollmean(difference, k = 20, fill = NA))
  return(a)
})
speed_data = do.call(rbind, speed_data)
speed_data = reshape2::melt(speed_data, id.var = c('date', 'location', 'iso_code'))
speed_data = subset(speed_data, variable != 'difference')

write_csv(speed_data, 'speed_data.csv')
