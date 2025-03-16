#the following code is for temperature salinity plots

#load packages
#install.packages("TSstudio")
library("TSstudio")
library(ggplot2)
library(readr)
library(dplyr)
install.packages("ggalt")
library(ggalt)

#import spreadsheets
GOM4_all_stations <-  read.csv("GOM4_all_stations.csv", header = TRUE)
GOM4_select_stations <-  read.csv("GOM4_select_stations.csv", header = TRUE)

# A basic formula to calculate seawater density (sigma)
GOM4_select_density <- GOM4_select_stations %>%
  mutate(sigma = 1000 + (0.8 * temp) + (0.4 * salinity))

# Basic scatter plot of temperature vs salinity with color by depth_class
ggplot(GOM4_select_density, aes(x = salinity, y = temp, color = factor(depth_class))) +
  geom_point() + # Plot the points
  labs(x = "Salinity", y = "Temperature (°C)", color = "Depth Class") +
  theme_minimal() + # Clean theme
  scale_color_viridis_d() # A nice color scale


# Basic scatter plot of temperature vs salinity with circles around each depth_class group
ggplot(GOM4_select_density, aes(x = salinity, y = temp, color = factor(depth_class))) +
  geom_point() + # Plot the points
  geom_encircle(aes(group = factor(depth_class), fill = factor(depth_class)), 
                color = "black", # Circle color
                alpha = 0.1, size = 1) + # Circle around each group
  labs(x = "Salinity", y = "Temperature (°C)", color = "Depth Class") +
  theme_minimal() + # Clean theme
  scale_color_viridis_d() # A nice color scale
###############################################################################
# Basic scatter plot of temperature vs salinity with color by depth_class
ggplot(GOM4_all_stations, aes(x = salinity, y = temp, color = factor(depth_class))) +
  geom_point() + # Plot the points
  labs(x = "Salinity", y = "Temperature (°C)", color = "Depth Class") +
  theme_minimal() + # Clean theme
  scale_color_viridis_d() # A nice color scale

# Basic scatter plot of temperature vs salinity with circles around each depth_class group
ggplot(GOM4_all_stations, aes(x = salinity, y = temp, color = factor(depth_class))) +
  geom_point() + # Plot the points
  geom_encircle(aes(group = factor(depth_class)), 
                color = "black", # Circle border color
                fill = NA,       # Make the fill transparent
                alpha = 0.2,     # Adjust transparency of the circles (0 is fully transparent, 1 is opaque)
                size = 3) + # Circle around each group
  labs(x = "Salinity", y = "Temperature (°C)", color = "Depth Class") +
  theme_minimal() + # Clean theme
  scale_color_viridis_d() # A nice color scale
###############################################################################
# Load the oce package
install.packages("oce")
library(oce)



# Assuming your data is in a data frame called "data" with columns "temperature" and "salinity"

plotTS(GOM4_all_stations,  
       
       inSitu = TRUE,  # Set to TRUE if using in-situ temperature data
       
       referencePressure = 0, # Set reference pressure for density calculations [1, 3, 4]
       
       nlevels = 6,  # Number of isopycnal lines to plot [1, 4, 10]
       
       grid = TRUE)  # Add a grid to the plot [1, 4, 10]

