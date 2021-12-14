library(readr)
library(tidyverse)
library(ggiraph)
library(rnaturalearth)
library(rnaturalearthdata)
library(sf)
require(data.table)

oid_data <- fread("https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/owid-covid-data.csv")
world <- ne_countries(scale = "medium", returnclass = "sf") 
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


speed_data_latest = na.omit(speed_data)
speed_data_latest = split(speed_data_latest, f = speed_data_latest$location)
speed_data_latest = lapply(speed_data_latest, function(x){
  subset(x, date == max(x$date))
})
speed_data_latest = do.call(rbind, speed_data_latest)


speed_data_map <- rename(speed_data_latest, iso_a3 = iso_code)
speed_data_map$tooltip = paste(speed_data_latest$location, 
                               formatC(speed_data_map$value, big.mark = ','))
world = st_sf(world)
world = world %>% left_join(speed_data_map, by = 'iso_a3')


policy_stringency = oid_data %>% select(location, date,
                                        stringency_index, 
                                        new_deaths_per_million) %>%
  fill(stringency_index) %>% 
  mutate(stringency_lag = lag(stringency_index, 15)) %>%
  filter(date == max(date)) 

policy_stringency$new_deaths_per_million[is.na(policy_stringency$new_deaths_per_million)
] <- 0
policy_stringency$new_deaths_per_million[policy_stringency$new_deaths_per_million < 0
] <- 0

policy_vaxx = oid_data %>% select(location, date,
                                  people_vaccinated_per_hundred, 
                                  new_deaths_per_million) %>%
  fill(people_vaccinated_per_hundred) %>% 
  mutate(vaxx_lag = lag(people_vaccinated_per_hundred, 15)) %>%
  filter(date == max(date)) 
policy_vaxx$new_deaths_per_million[is.na(policy_vaxx$new_deaths_per_million)
] <- 0
policy_stringency$new_deaths_per_million[policy_stringency$new_deaths_per_million < 0
] <- 0

third_doses = oid_data %>% select(location, iso_code, date, total_boosters_per_hundred)
third_doses = split(third_doses, f = third_doses$location)
third_doses = lapply(third_doses, function(x){
  x %>% fill(total_boosters_per_hundred)
})
third_doses = do.call(rbind, third_doses)
third_doses = subset(third_doses, date > Sys.Date() - 40)
third_doses = subset(third_doses, iso_code %in% world$iso_a3)
third_doses$total_boosters_per_hundred[is.na(third_doses$total_boosters_per_hundred)] <-0
third_doses = subset(third_doses, date == max(date))


picci = theme_minimal()  + theme(legend.position = 'bottom',
                                 plot.title = element_text(family = 'Garamond',
                                                           face = 'bold',
                                                           size = 18
                                 ),
                                 plot.subtitle = element_text(family = 'Garamond',
                                                              face = 'plain',
                                                              colour = 'grey27',
                                                              size = 16
                                 ),
                                 text = element_text(family = 'Garamond'),
                                 axis.text = element_text(face = 'bold'),
                                 panel.grid.major.y = (element_line(color = 'grey27')),
                                 panel.grid.minor.y = (element_blank()),
                                 panel.grid.major.x = (element_blank()),
                                 panel.grid.minor.x = element_blank(),
                                 axis.ticks.x = (element_line()),
                                 plot.background = element_rect(fill =
                                                                  'white',
                                                                color = 'white'),
                                 plot.title.position = 'plot') 

save.image(file = 'tracker_data.RData')
