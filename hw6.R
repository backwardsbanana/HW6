library(dplyr)
library(tidyr)
library(ggplot2)
library(lubridate)

setwd("")
load("house_prices.rda")

houses <- house_prices %>% filter(year(date) >= 1980)

max(house_prices$date) #only goes to 2017

ggplot(data=houses, aes(date, house_price_index)) +
  geom_line(color = "blue",size = .6) + facet_wrap(~state, labeller = label_wrap_gen(width = 12)) +
  labs(title = "Price Index Over Years",
      y = "House Price Index", x = "Years") + 
  scale_x_date(date_breaks="10 years",
      date_labels = "%y", 
      limits=c(as.Date("1980-01-01", "%Y-%m-%d"), as.Date("2020-12-01", "%Y-%m-%d")),
      expand = expand_scale(mult = c(0.00, 0))) +
  theme(panel.spacing.x = grid::unit(20, "pt"), strip.text = element_text(size = 8))

###############################

house_reshaped <- houses %>% select(date, state, house_price_index, unemploy_perc) %>% 
  gather(key="measure", value="value", house_price_index:unemploy_perc)

###############################

ggplot(data=house_reshaped, mapping=aes(x=date, y=value, group=measure, color=measure)) +
  geom_line()+
  facet_wrap(~state, labeller = label_wrap_gen(width = 12))+
  scale_x_date(date_breaks="10 years",
               date_labels = "%y", 
               limits=c(as.Date("1980-01-01", "%Y-%m-%d"), as.Date("2020-12-01", "%Y-%m-%d")),
               expand = expand_scale(mult = c(0.00, 0)))+
  labs(title = "Price Index and Unemployment Over Years", x = "Years") +
  theme(panel.spacing.x = grid::unit(20, "pt"), strip.text = element_text(size = 8))

# This plot doesn't seem to yield too much information about the relationship between unemploy_perc
# and house_price_index maybe because it reaches values of 300+ while unemploy_perc only reaches 15+

##############################

ggplot(data=house_reshaped, mapping=aes(x=date, y=value, group=0, color=measure)) +
  geom_line()+
  facet_wrap(~state, labeller = label_wrap_gen(width = 12))+
  scale_x_date(date_breaks="10 years",
               date_labels = "%y", 
               limits=c(as.Date("1980-01-01", "%Y-%m-%d"), as.Date("2020-12-01", "%Y-%m-%d")),
               expand = expand_scale(mult = c(0.00, 0)))+
  labs(title = "Price Index and Unemployment Over Years", x = "Years") +
  theme(panel.spacing.x = grid::unit(20, "pt"), strip.text = element_text(size = 8))

# This plot shows an easier to view relationship between unemploy_perc and house_price_index
# But you must override the default grouping and I'm unsure if this makes it misleading.
# Now it seems unemploy_perc correlates with house_price_index strongly











