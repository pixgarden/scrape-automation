library(readr)
library(tidyverse)
library(ggiraph)
require(data.table)


#Download data and exclude suff
oid_data <- fread("https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/owid-covid-data.csv")
iso_3 <- fread("https://raw.githubusercontent.com/fpmassam/scrape-automation/main/iso_3.csv")
iso_3 = iso_3 %>% select(ISO3)
iso_3 = iso_3$ISO3
oid_data = subset(oid_data, iso_code %in% iso_3)
oid_rest = subset(oid_data, !(iso_code %in% iso_3))
cases_latest = oid_data %>% select(date, location, iso_code, date, new_cases) %>% 
  filter(date == max(date))
cases_latest = rename(cases_latest, iso_a3 = iso_code)
write_csv(cases_latest, 'new_cases_latest.csv')


#Prepare data for visualization and write the *.csv files
##Smoothed approximation of the second derivative of COVID cases
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
    mutate(`20-days average` = zoo::rollmean(difference, k = 20, fill = NA, align = 'right'))
  return(a)
})
speed_data = do.call(rbind, speed_data)
speed_data = reshape2::melt(speed_data, id.var = c('date', 'location', 'iso_code'))
speed_data = subset(speed_data, variable != 'difference')
write_csv(speed_data, 'speed_data.csv')


##Last available data and perparation for the maps
speed_data_latest = na.omit(speed_data)
speed_data_latest = split(speed_data_latest, f = speed_data_latest$location)
speed_data_latest = lapply(speed_data_latest, function(x){
  subset(x, date == max(x$date))
})
speed_data_latest = do.call(rbind, speed_data_latest)
write_csv(speed_data_latest, 'speed_data_latest.csv')
speed_data_map <- rename(speed_data_latest, iso_a3 = iso_code)
speed_data_map$tooltip = paste(speed_data_latest$location, 
                               formatC(speed_data_map$value, big.mark = ','))
write_csv(speed_data_map, 'speed_data_map.csv')

##Share of vaxxed people (no booster) and effect on new_deaths_per_million
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
write_csv(policy_stringency, 'policy_stringency.csv')

##Share of vaxxed people (no booster) and effect on new_deaths_per_million
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
write_csv(policy_vaxx, 'policy_vaxx.csv')

#Total case by regime type 
Vdem <- read_csv("https://raw.githubusercontent.com/fpmassam/scrape-automation/main/Vdem.csv")
cases_total_per_capita = oid_data %>% select(date, location, iso_code, total_cases_per_million) %>%
  filter(date == max(date))

impact_regime = merge(Vdem, cases_total_per_capita, by.y = 'iso_code',
                      by.x = 'country_text_id')

impact_regime$label = paste(impact_regime$location, format(round(impact_regime$total_cases_per_million,
                                                                 1), big.mark = ','),
                            sep = ':')

write_csv(impact_regime, 'impact_regime.csv')


#Share boosters percent
third_doses = oid_data %>% select(location, iso_code, date, total_boosters_per_hundred)
third_doses = split(third_doses, f = third_doses$location)
third_doses = lapply(third_doses, function(x){
  x %>% fill(total_boosters_per_hundred)
})
third_doses = do.call(rbind, third_doses)
third_doses = subset(third_doses, date > Sys.Date() - 40)
third_doses$total_boosters_per_hundred[is.na(third_doses$total_boosters_per_hundred)] <-0
third_doses = subset(third_doses, date == max(date))

write_csv(third_doses, 'third_doses.csv')

## Top-ten countries with the most cases
top_10_cases = oid_data %>% select(date, iso_code, location, total_cases) %>%
  filter(date == max(date)) %>% slice_max(order_by = total_cases, n = 10)
top_10_cases$total_cases = format(top_10_cases$total_cases, big.mark = ',')
write_csv(top_10_cases, 'top_10_cases.csv')
top_10_deaths = oid_data %>% select(date, iso_code, location, total_deaths) %>%
  filter(date == max(date)) %>% slice_max(order_by = total_deaths, n = 10)
top_10_deaths$total_deaths = format(top_10_deaths$total_deaths, big.mark = ',')
write_csv(top_10_deaths, 'top_10_deaths.csv')

## Top-ten trends
trends_top_10_c = oid_data %>% select(location, 
                                      date, 
                                      total_cases) %>% 
  filter(location %in% top_10_cases$location) %>%
mutate(variable = 'New cases') %>% rename(value = total_cases)

trends_top_10_d = oid_data %>% select(location, 
                                      date, 
                                      total_deaths) %>% 
  filter(location %in% top_10_deaths$location) %>%
  mutate(variable = 'Deaths') %>% rename(value = total_deaths)
  

trends = rbind(trends_top_10_c, trends_top_10_d)
write_csv(trends, 'trends.csv')


##Share of cases and deaths per country
share_cases = oid_data %>% select(date, location, total_cases) %>%
  filter(date == max(date)) %>% mutate(share_cases = round(total_cases/sum(total_cases,
                                                                           na.rm = TRUE), 2)
  )
share_deaths = oid_data %>% select(date, location, total_deaths) %>%
  filter(date == max(date))%>% mutate(share_deaths = round(total_deaths/sum(total_deaths, 
                                                                            na.rm = TRUE), 2))
shares =  merge(share_cases, share_deaths, by = 'location')
shares = shares %>% select(location, share_cases, share_deaths)
shares = reshape2::melt(shares, id.var = 'location')
write_csv(shares, 'shares.csv')

#Difference-in-difference dataviz 
did_dataset = oid_data %>% select(date, iso_code, location, new_deaths_per_million, 
                                  total_boosters_per_hundred) %>%
  fill(total_boosters_per_hundred)

did_dataset = subset(did_dataset, date > '2021-11-01')

did_dataset$time = ifelse(did_dataset$date > '2021-11-26', 1, 0)
did_dataset$treated = ifelse(did_dataset$total_boosters_per_hundred > 10, 1, 0)
did_dataset$treatment = ifelse(did_dataset$treated == 1, 'Higher vaccinations', 
                               'Lower vaccinations')
did_dataset$omicron = ifelse(did_dataset$time == 1, 'After omicron', 
                             'Before omicron')

did_dataset = aggregate(new_deaths_per_million ~ treatment + omicron + date, data = did_dataset, 
                        FUN = mean)
did_plot = subset(did_dataset, date == '2021-11-26')
did_plot_1 = subset(did_dataset, date == max(did_dataset$date))
did_plot = rbind(did_plot, did_plot_1)
did_plot$To = ifelse(did_plot$date == '2021-11-26', 'Discovery of Omicron',
                     'Latest available data')

did_plot_2 = subset(did_dataset, date == '2021-11-11')
did_plot_2$To = 'Before Omicron'
did_plot = rbind(did_plot, did_plot_2)
write_csv(did_plot, 'did_plot.csv')

#Prevalence of the several variants in Europe according to teh ECDC 
variants = read_csv('https://opendata.ecdc.europa.eu/covid19/virusvariant/csv/data.csv')
variants = subset(variants, source == 'TESSy')
variants = split(
  variants, f = variants$country
)

variants = do.call(rbind, lapply(variants, function(x){
    subset(x, year_week == max(x$year_week))
  }))

unique(variants$variant)
variants$Variant[variants$variant == 'B.1.351'] <- 'Beta'
variants$Variant[variants$variant == 'P.1'] <- 'Gamma'
variants$Variant[variants$variant == 'B.1.617.2'] <- 'Delta'
variants$Variant[variants$variant == 'B.1.1.529'] <- 'Omicron'
variants$Variant[is.na(variants$Variant)] <- 'Other'
variants$percent_variant[is.na(variants$percent_variant)] <- 0
write_csv(variants, 'variants.csv')

#Backup data in an R tracker 
save.image(file = 'tracker_data.RData')
