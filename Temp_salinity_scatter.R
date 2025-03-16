#Scatter plot time
#I want to plot temp vs salinity for all stations grouped by region to see if this defines the regions
library(tidyverse)
library(ggplot2)

#surface TEMP/Salinity by region w/ Chla
ENV_data %>% 
  mutate(Region = as.factor(Region)) %>% ## Change Month to a Categorical variable 
  ggplot( ., aes(x = Sal, y = Temp, size = Chla.WSW, color = Region)) +
  geom_point(alpha = .4) +
  ggtitle(label = "Temperature & Salinity", subtitle = "Colors indicate Region"
  ) +
  xlab("Salinity (PSU)") +
  ylab("Temperature(C)") +
  theme_classic() 

#surface Temp/sal by shore position  w/ Chla
ENV_data %>% 
  mutate(Position = as.factor(Position)) %>% ## Change Month to a Categorical variable 
  ggplot( ., aes(x = Sal, y = Temp, size = Chla.WSW, color = Position)) +
  geom_point(alpha = .4) +
  ggtitle(label = "Temperature & Salinity", subtitle = "Colors Indicate Position from Shore"
  ) +
  xlab("Salinity (PSU)") +
  ylab("Temperature(C)") +
  theme_classic() 

#surface Temp/sal by transect w/ Chla
ENV_data %>% 
  mutate(Transect = as.factor(Transect)) %>% ## Change Month to a Categorical variable 
  ggplot( ., aes(x = Sal, y = Temp, size = Chla.WSW, color = Transect)) +
  geom_point(alpha = .4) +
  ggtitle(label = "Temperature & Salinity", subtitle = "Colors indicate Transect"
  ) +
  xlab("Salinity (PSU)") +
  ylab("Temperature(F)") +
  theme_classic() 

