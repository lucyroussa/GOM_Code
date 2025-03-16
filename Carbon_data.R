C_data <- read.csv ("Carbon_data.csv") #read CSV into R

#make a box plot of average C standing stocks by depth class
C_data %>%
  mutate(Depth_class = as.factor(Depth_class)) %>%
  ggplot(., aes(x=factor(Depth_class, level=c('surface', 'subsurface', 'chl max', 'deep', 'very deep')), y = Total_plus_1, group = Depth_class, color = Depth_class,)) +
  ggtitle(label = "Relationship Between Depth and Carbon Standing Stocks") +
  xlab("Depth Class") +
  ylab("Log (x+1) Transformed Carbon Standing Stocks (µg C L-1)") +
  scale_y_continuous(trans = 'log10') +
  geom_boxplot() +
  theme_minimal()

#make a box plot of average C standing stocks by distance to shore
C_data %>%
  mutate(Position = as.factor(Position)) %>%
  ggplot(., aes(x=factor(Position, level=c('nearshore', 'middle', 'offshore')), y = Total_plus_1, group = Position, color = Position,)) +
  ggtitle(label = "Relationship Between Distance to shore and Carbon Standing Stocks") +
  xlab("Position to shore") +
  ylab("Log (x+1) Transformed Carbon Standing Stocks (µg C L-1)") +
  scale_y_continuous(trans = 'log10') +
  geom_boxplot() +
  theme_minimal()

#Box plot of average C standing stocks by distance to shore, but excluding the 0s
# not super useful given that 0's are part of the data that I would like to include
C_data %>%
  mutate(Position = as.factor(Position)) %>%
  ggplot(., aes(x=factor(Position, level=c('nearshore', 'middle', 'offshore')), y = Total_C_standing_stocks, group = Position, color = Position,)) +
  ggtitle(label = "Relationship Between Distance to shore and Carbon Standing Stocks", subtitle = "All Depths, excluding 0 counts") +
  xlab("Position to shore") +
  ylab("Log Transformed Carbon Standing Stocks (µg C L-1)") +
  scale_y_continuous(trans = 'log10') +
  geom_boxplot() +
  theme_minimal()

#scatter plot of NO3 and NO2 vs C for all stations, grouped by region
C_data %>% 
  mutate(Region = as.factor(Region)) %>% 
  ggplot( ., aes(x = Total_C_standing_stocks, y = NITRATE_NITRITE_PPM, color = Region)) + ## addin color aesthetic 
  geom_point(size = 4, alpha = .6) +  ## these are actually additional aesthetics added to the geom_point() layer
  ggtitle(label = "Relationship between Nitrogen and Carbon Standing Stocks", subtitle = "Colored by Region") +
  xlab("Log Transformed (log10) Carbon Standing Stocks (µg C L-1)") +
  ylab("Log Transformed (log10) Nitrate+Nitrite (PPM)") +
  scale_y_continuous(trans = 'log10') +
  scale_x_continuous (trans = 'log10') +
  theme_classic()

#scatter plot of NO3 and NO2 vs C for all stations, grouped by depth class
C_data %>% 
  mutate(Region = as.factor(Depth_class)) %>% 
  ggplot( ., aes(x = Total_C_standing_stocks, y = NITRATE_NITRITE_PPM, color = Depth_class)) + ## addin color aesthetic 
  geom_point(size = 4, alpha = .6) +  ## these are actually additional aesthetics added to the geom_point() layer
  ggtitle(label = "Relationship between Nitrogen and Carbon Standing Stocks", subtitle = "Colored by Depth Class") +
  xlab("Log Transformed (log10) Carbon Standing Stocks (µg C L-1)") +
  ylab("Log Transformed (log10) Nitrate+Nitrite (PPM)") +
  scale_y_continuous(trans = 'log10') +
  scale_x_continuous (trans = 'log10') +
  theme_classic()

#scatter plot of NO3 and NO2 vs C for all stations, grouped by depth
C_data %>% 
  ggplot( ., aes(x = NITRATE_NITRITE_PPM, y = Depth_m, color = Depth_class)) + ## addin color aesthetic 
  geom_point(size = 4, alpha = .6) +  ## these are actually additional aesthetics added to the geom_point() layer
  ggtitle(label = "Relationship between Nitrogen and Depth", subtitle = "Colored by Depth Class") +
  xlab("Nitrate+Nitrite (PPM)") +
  ylab("Depth (m)") +
  scale_y_reverse() +
 #scale_y_continuous(trans = 'log10') +
 #scale_x_continuous (trans = 'log10') +
  theme_classic()

#scatter plot of NO3 and NO2 vs C for all stations, grouped by position to shore
C_data %>% 
  ggplot( ., aes(x = NITRATE_NITRITE_PPM, y = Depth_m, color = Position)) + ## addin color aesthetic 
  geom_point(size = 4, alpha = .6) +  ## these are actually additional aesthetics added to the geom_point() layer
  ggtitle(label = "Relationship between Nitrogen and Depth", subtitle = "Colored by Position to shore") +
  xlab("Nitrate+Nitrite (PPM)") +
  ylab("Depth (m)") +
  scale_y_reverse() +
  #scale_y_continuous(trans = 'log10') +
  #scale_x_continuous (trans = 'log10') +
  theme_classic()
