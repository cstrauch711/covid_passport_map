#' Passport Maps during Covid-19 Pandemic
#' Data Source: 
#' Created by Clayton Strauch on 23 JAN 2021
#' Last Updated: 23 JAN 2021

library(tidyverse)
library(maps)
library(countrycode)
setwd("C://Users/Clayton/Desktop/Projects/passport")



# IMPORT DATA
# data_2019_12 <- read.csv("data/passport-index-matrix-iso3_2019-12-17.csv", header=T, na.strings = "-1")
# n <- names(data_2019_12)[-c(1)]
# trans <- as.data.frame( t(data_2019_12) ) 
# trans <- trans[-c(1),]
# colnames(trans) <- n
# 
# df2 <- mutate_all(trans, function(x) as.numeric(as.character(x)))
# 
# data_2019_12_sums <- df2 %>%
#   summarize_all(sum, na.rm=TRUE)


# NOVEMBER 2020 DATA
data_2020_11 <- read.csv("data/passport-index-matrix-iso3_2020-11.csv", header=T, na.strings="-1")

d2 <- data_2020_11 %>%
  mutate(status_nov_2020 = ifelse(Requirement %in% c("visa required", "covid ban", "no admission"), 0, 
                         ifelse(is.na(Requirement), NA, 1)))

totals <- d2 %>%
  group_by(Passport) %>%
  summarize(nov_2020 = sum(status, na.rm=TRUE)) 

#MAP OF NOV 2020
world <- map_data("world") %>%
  mutate(iso = countrycode(region, origin='country.name', destination = 'iso3c'))


new <- inner_join(world, totals, by= c("iso"="Passport"))

ggplot() + geom_polygon(data = new, aes(x=long, y = lat, group = group, fill=nov_2020), color="white" ) + 
  coord_fixed(1.3)  

# CHANGE FROM NOV 2019 to NOV 2020
data_2019_11 <- read.csv("data/passport-index-tidy-iso3_2019-11.csv", header=T, na.strings="-1") %>%
  mutate(status_nov_2019 = ifelse(Code == 0, 0, 1)) %>%
  group_by(Passport) %>%
  summarize(nov_2019 = sum(status_nov_2019, na.rm=TRUE)) 

  
change_data <- inner_join(data_2019_11, totals)

change_data <- change_data %>%
  mutate(difference = nov_2020 - nov_2019)

world_19to20 <- map_data("world") %>%
  mutate(iso = countrycode(region, origin='country.name', destination = 'iso3c')) %>%
  inner_join(change_data, by= c("iso"="Passport"))

ggplot() + geom_polygon(data = world_19to20, aes(x=long, y = lat, group = group, fill=difference), color="white" ) + 
  coord_fixed(1.3) 
