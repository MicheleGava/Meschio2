library(readr)
library(dplyr)
library(ggplot2)
library(zoo)
data <- read_csv('data/long_df.csv' )


data %>% 
  filter(lev_type == 'MAX') %>% 
  arrange(station, date) %>% 
  mutate(ma=rollapply(level, 5, mean, align = 'right', fill=NA)) %>% 
  ggplot() + 
   geom_line(mapping=aes(date, level, color = station), alpha = 1/3) + 
   geom_line(mapping=aes(date, ma, color = station), linetype=2) +
   scale_x_date(date_breaks = "1 years", date_labels = '%Y-%m') +
   theme(axis.text.x = element_text(angle=-45)) +
   labs(title = 'Maximum Hydrometric level of River Meschio', subtitle = '2010 - 2020', y = 'Hydr. level')  

