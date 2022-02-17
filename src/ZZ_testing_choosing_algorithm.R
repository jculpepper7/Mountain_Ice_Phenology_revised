#Choosing test case

#fake dataset for choosing with known "ice on" and "ice off" dates (just two years)
#One year will have an ice on and one year will not have an ice on at all to test
#if the algorithm picks up on when there is no ice on

library(tidyverse)
library(lubridate)

data1 <- tibble(
  name = c('lake_name'),
  date = c(
    seq(from = ymd('2020-01-01'), to = ymd('2020-03-31'), by = 'days'),
    seq(from = ymd('2021-01-01'), to = ymd('2021-03-31'), by = 'days')
           ),
  fraction = c(
    seq(from = 0, to = 0, length.out = 31), 
    #seq(from = 1, to = 1, length.out = 29),
    seq(from = 1, to = 1, length.out = 1),
    c('NA','NA','NA','NA','NA'),
    seq(from = 1, to = 1, length.out = 3),
    seq(from = 0, to = 0, length.out = 20),
    seq(from = 0, to = 0, length.out = 31),
    seq(from = 0, to = 0, length.out = 31), 
    seq(from = 0.8, to = 1, length.out = 8),
    seq(from = 0.5, to = 0.5, length.out = 10),
    seq(from = 0.8, to = 1, length.out = 2),
    seq(from = 1, to = 1, length.out = 8),
    seq(from = 0, to = 0, length.out = 31)
    )
)

data1 <- data1 %>%
  mutate(
    year = year(date)
  )

ice_on_test <- data1 %>%
  group_by(year) %>%
  mutate(median_iceFrac = rollapply(fraction, width = 6, min, align = "left", fill = NA, na.rm = TRUE)) %>%
  filter(median_iceFrac >= 0.8) %>%
  filter(row_number() == 1) %>%
  rename(date_ice_on = date) %>% #remove when not going through all columns
  ungroup()

ice_on_join <- ice_on_test %>%
  select(name, date_ice_on, year)

ice_off_test <- data1 %>%
  group_by(year) %>%
  left_join(., y= ice_on_join) %>%
  mutate(median_iceFrac = rollapply(fraction, width = 3, max, align = "left", fill = NA, na.rm = TRUE)) %>%
  filter(median_iceFrac <= 0.2 & date > date_ice_on) %>%
  filter(row_number() == 1) %>%
  rename(date_ice_off = date) %>% #remove when not going through all columns
  ungroup()

######
#What do I need to do now that I know the algorithm chooses ice on with NA values counted as >=0.8???
#The ice off algorithm will choose a value after the NAs have stopped. (See ice_off_test results)
#I need to look at the individual datasets for NA values
#Get the percentage of cloud cover, first as a whole, second as a percent of seasons
#See when the ice on algorithm is choosing an ice on date that is followed by NA values
#It may be that we can only get at ice off...which would not be ideal.
#Also look to see if you can add an NA for a year where an ice on is not chosen.

