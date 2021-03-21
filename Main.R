# required libraries
library(readr)
library(dplyr)
library(ggplot2)
library(zoo)
# import dataset
data <- read_csv('data/long_df.csv' )

# first plot: max level by station with 5 days moving average
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
ggsave('plots/max_level.png')

# aggregation by month
# add some feature: month, year
data$month <- format(data$date, "%B")
data$year <- format(data$date, "%Y")
# aggregate
by_month = group_by(filter(data,lev_type=='MAX'), month, station)
by_month_summary <- summarise(by_month, level = mean(level, na.rm = TRUE))
by_month_summary <- mutate(by_month_summary, month=factor(month, levels=c("gennaio","febbraio","marzo","aprile","maggio","giugno","luglio","agosto","settembre","ottobre","novembre","dicembre")))
ggplot(by_month_summary) +
 geom_bar(aes(month, level, fill = station), position = 'dodge', stat = "summary")
ggsave('plots/by_month.png')

