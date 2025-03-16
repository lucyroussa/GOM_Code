## Mapping environmental variables

# Load necessary libraries
library(marmap)
library(ggplot2)
library(mapdata)
library(mapproj)
library(cmocean)
library(readxl)
library(dplyr)

# Load in data from excel sheets
Planktoncounts <- read_excel("Plankton_counts.xlsx")
Environmentaldata <- read_excel("ENV_DATA_COUNTED_STATIONS.xlsx")

# Add environmental info to counts dataframe by station and depth
GOM4_DATA_FULL <- full_join(Planktoncounts, Environmentaldata, by = c("STATION", "DEPTH_METER"))

GOM4_DATA_COUNTED_STATIONS <- inner_join(Planktoncounts, Environmentaldata, by = c("STATION", "DEPTH_METER"))

# Set map limits (can be adjusted)
lons = c(-98, -79)
lats = c(32, 17)

# Configure bathymetry
bathymetry_df <- getNOAA.bathy(lon1 = -97, lon2 = -80.3,
                               lat1 = 30.8, lat2 = 16, resolution = 10)

# Get regional polygons
# To show list of all possible regions, run line below:
#unique(map_data('world2Hires')$region)
reg = map_data("world2Hires")
reg = subset(reg, region %in% c('Mexico', 'USA'))


## Add color scales
# Note: you can use any color scales; cmocean is my preferred library
# Color scaling may need to be adjusted to account for extremes in the data

# Sets color and limits for depth to min and max
cols_depth <- cmocean('deep')(100)
range_cols_depth <- c(min(GOM4_DATA_COUNTED_STATIONS$DEPTH_METER, na.rm = TRUE),max(GOM4_DATA_COUNTED_STATIONS$DEPTH_METER, na.rm = TRUE))

# Sets color and limits for surface salinity to min and max
cols_sfc_salinity <- cmocean('dense')(100)
range_cols_salinity <- c(min(GOM4_DATA_COUNTED_STATIONS$SALINITY, na.rm = TRUE),max(GOM4_DATA_COUNTED_STATIONS$SALINITY, na.rm = TRUE))

# Sets color and limits for surface temperature to min and max
cols_sfc_temp <- cmocean('thermal')(100)
range_cols_sfc_temp <- c(min(GOM4_DATA_COUNTED_STATIONS$TEMPERATURE, na.rm = TRUE),max(GOM4_DATA_COUNTED_STATIONS$TEMPERATURE, na.rm = TRUE))

# Sets color and limits for pH to min and max
cols_pH <- cmocean('speed')(100)
range_cols_pH <- c(min(GOM4_DATA_COUNTED_STATIONS$pH, na.rm = TRUE),max(GOM4_DATA_COUNTED_STATIONS$pH, na.rm = TRUE))



## Plotting
# Note: the first plot is annotated line by line for ease of replication
# Bathymetry is included in the first plot but not in following plots
# Formatting of axis titles is included in first plot but not following plots

# Plot all stations with samples
all_stations_plt <- ggplot()+
  
  # add 100m contour
  geom_contour(data = bathymetry_df, aes(x=x, y=y, z=z), 
               breaks=c(-100), size=c(0.3), color="lightgrey") +
  
  # add 200m contour
  geom_contour(data = bathymetry_df, aes(x=x, y=y, z=z), 
               breaks=c(-200), size=c(0.6), color="darkgrey") +
  
  # # add 250m contour
  geom_contour(data = bathymetry_df, aes(x=x, y=y, z=z), 
               breaks=c(-250), size=c(0.6), color="black") +
  
  # add coastline
  geom_polygon(aes((data = reg, aes(x = LONGITUDE, y = LATITUDE, group = group),
               fill = "darkgrey"", color = NA)) +
  
  # plot all sample points
  geom_point(data = GOM4_DATA_COUNTED_STATIONS, aes(x = LONGITUDE, y = LATITUDE, size = 2)) +
  
  # configure projection and plot domain
  coord_map(xlim = lons, ylim = lats) +
  
  # title formatting
  ylab("Latitude") + xlab("Longitude") + ggtitle ("GOMECC4 Sampling Sites") +
  
  # theme--switch to theme_classic for no gridlines
  theme_bw() +
  
  # format axis labels and titles
  theme(axis.text.x = element_text(face="plain", color="#000000", 
                                   size=14, angle=0),
        axis.text.y = element_text(face="plain", color="#000000", 
                                   size=14, angle=0),
        axis.title.x = element_text(face="plain", color="#000000", 
                                    size=18, angle=0),
        axis.title.y = element_text(face="plain", color="#000000", 
                                    size=18, angle=90),
        title = element_text(face="bold", color="#000000", 
                             size=14, angle=0))

# Plot stations colored by depth
depth_plt <- ggplot()+
  geom_polygon(data = reg, aes(x = long, y = lat, group = group), 
               fill = "darkgrey", color = NA) + 
  geom_point(data = GOM4_DATA_COUNTED_STATIONS, aes(x = lon, y = lat, color = depth), size = 2) +
  scale_color_gradientn(limits = range_cols_depth, colours=(cols_depth)) +
  coord_map(xlim = lons, ylim = lats) +
  ylab("Latitude") + xlab("Longitude") + 
  ggtitle ("ECOA Sampling Sites by Depth") + labs(color="Depth (m)") +
  theme_bw()

# Plot stations colored by sea surface temperature
sfc_temp_plt <- ggplot()+
  geom_polygon(data = reg, aes(x = long, y = lat, group = group), 
               fill = "darkgrey", color = NA) + 
  geom_point(data = GOM4_DATA_COUNTED_STATIONS, aes(x = lon, y = lat, color = sfc_temp), size = 2) +
  scale_color_gradientn(limits = range_cols_sfc_temp, colours=(cols_sfc_temp)) +
  coord_map(xlim = lons, ylim = lats) +
  ylab("Latitude") + xlab("Longitude") + 
  ggtitle ("GOMECC4 Sampling Sites by Temperature") + labs(color="Temp (C)") +
  theme_bw()

# Plot stations colored by sea surface salinity
sfc_salt_plt <- ggplot()+
  geom_polygon(data = reg, aes(x = long, y = lat, group = group), 
               fill = "darkgrey", color = NA) + 
  geom_point(data = GOM4_DATA_COUNTED_STATIONS, aes(x = lon, y = lat, color = sfc_salt), size = 2) +
  scale_color_gradientn(limits = range_cols_sfc_salt, colours=(cols_sfc_salt)) +
  coord_map(xlim = lons, ylim = lats) +
  ylab("Latitude") + xlab("Longitude") + 
  ggtitle ("GOMECC4 Sampling Sites by Salinity") + labs(color="Salinity") +
  theme_bw()

# Plot stations colored by sea surface pH
pH_plt <- ggplot()+
  geom_polygon(data = reg, aes(x = long, y = lat, group = group), 
               fill = "darkgrey", color = NA) + 
  geom_point(data = GOM4_DATA_COUNTED_STATIONS, aes(x = lon, y = lat, color = pH), size = 2) +
  scale_color_gradientn(limits = range_cols_pH, colours=(cols_pH)) +
  coord_map(xlim = lons, ylim = lats) +
  ylab("Latitude") + xlab("Longitude") + 
  ggtitle ("GOMECC4 Sampling Sites by pH") + labs(color="pH") +
  theme_bw()
