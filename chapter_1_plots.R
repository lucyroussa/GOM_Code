library(dplyr)
library(ggplot2)
library(tidyr)
#install.packages("ggpattern")
library(ggpattern)
#install.packages("devtools")
#devtools::install_github("coolbutuseless/ggpattern")
#install.packages("tidyverse")
library(tidyverse)

#make a custom color pallete for depth classes (always use these colors)
depth_colors <- c("surface" = "#ce4993", 
                  "subsurface" = "#e8702a", 
                  "chl max" = "#36802d", 
                  "deep" = "#0c457d", 
                  "very deep" = "#6a0d83")

distance_colors <- c("nearshore" = "#f8b196", 
                  "middle" = "#c06c84", 
                  "offshore" = "#335c7d")

functional_colors <- c("Phototroph" = "#7fc48f", 
                  "CM" = "#008c9f", 
                  "NCM" = "#d0bd73", 
                  "Heterotroph" = "#8d8bbf", 
                  "NA" = "#000000")

region_colors <- c("WFS" = "#274a27", 
                  "TLS" = "#07cccc", 
                  "EMS" = "#ee4000", 
                  "CB" = "#cecd01", 
                  "FS/YC" = "#efaeef")
# Make a box plot of average C standing stocks by depth class
Carbon_data <- read.csv ("Carbon_data.csv")

write.csv(Carbon_data,file=path, row.names = FALSE)
Carbon_data %>%
  mutate(Depth_class = factor(Depth_class, levels = c('surface', 'subsurface', 'chl max', 'deep', 'very deep'))) %>%
  ggplot(aes(
    x = Depth_class, 
    y = Total_plus_1, 
    group = Depth_class, 
    fill = Depth_class  # Use 'fill' for boxplot colors
  )) +
  ggtitle(label = "Relationship Between Depth Range and Carbon Standing Stocks") +
  xlab("Depth Range") +
  ylab("Carbon Standing Stocks (µg C L-1)") +
  scale_y_continuous(
    trans = 'log10',  # Apply log10 transformation to y-axis
    breaks = c(0,5,10,50,100,500),  # Set custom y-axis breaks
    limits = c(min(Carbon_data$Total_plus_1), 500),  # Extend y-axis to 500
    expand = c(0, 0)
    ) +
  geom_boxplot() +
  scale_fill_manual(values = depth_colors) +  # Map the custom colors to the Depth_class levels
  theme_classic() +
theme(
  axis.title.x = element_text(size = 14),  # Increase font size of x axis title
  axis.title.y = element_text(size = 14),  # Increase font size of y axis title
  axis.text.x = element_text(size = 12),   # Increase font size of x axis tick labels
  axis.text.y = element_text(size = 12)) 

#make a box plot of chlorophyll by depth class
C_data %>%
  mutate(Depth_class = factor(Depth_class, levels = c('surface', 'subsurface', 'chl max', 'deep', 'very deep'))) %>%
  ggplot(aes(
    x = Depth_class, 
    y = chla_plus1, 
    group = Depth_class, 
    fill = Depth_class  # Use 'fill' for boxplot colors
  )) +
  ggtitle(label = "Relationship Between Depth Class and Chlorophyll-a") +
  xlab("Depth Class") +
  ylab("Log (x+1) Transformed Chla WSW (µg L-1)") +
  scale_y_continuous(trans = 'log10') +
  geom_boxplot() +
  scale_fill_manual(values = depth_colors) +  # Map the custom colors to the Depth_class levels
  theme_classic()
################################################################################
#make a scatter plot of carbon vs N/P ratio
Carbon_data %>%
  mutate(Depth_class = factor(Depth_class, levels = c('surface', 'subsurface', 'chl max', 'deep', 'very deep'))) %>%
  ggplot(aes(x = N_P_ratio, y = Total_plus_1, color = Depth_class)) +  # Plot using the mutated data
  geom_point(size = 2.2) +  # Adds the scatter plot points with increased size +  # Adds the scatter plot points
  labs(x = "N/P Ratio ppm (log transformed)", 
       y = "Total C Standing Stocks (µg C L-1) (log transformed)", 
       color = "Depth Class") +  # Axis labels
  scale_y_continuous(trans = 'log10') +  # Log transformation for y axis
  scale_x_continuous(trans = 'log10') +  # Log transformation for x axis
  scale_color_manual(values = depth_colors) +  # Custom colors for Depth Class
  theme_classic() +  # Use a minimal theme for the plot
  theme(
  axis.title.x = element_text(size = 14),  # Increase font size of x axis title
  axis.title.y = element_text(size = 14),  # Increase font size of y axis title
  axis.text.x = element_text(size = 12),   # Increase font size of x axis tick labels
  axis.text.y = element_text(size = 12)) 

#make a scatter plot of carbon vs N 
Carbon_data %>%
  mutate(Depth_class = factor(Depth_class, levels = c('surface', 'subsurface', 'chl max', 'deep', 'very deep'))) %>%
  filter(NITRATE_NITRITE_PPM > 0, !is.na(NITRATE_NITRITE_PPM), !is.infinite(NITRATE_NITRITE_PPM), !is.na(Total_plus_1), !is.infinite(Total_plus_1)) %>%
  ggplot(aes(x = NITRATE_NITRITE_PPM, y = Total_plus_1, color = Depth_class)) +  # Plot using the mutated data
  geom_point(size = 3) +  # Adds the scatter plot points with increased size
  labs(x = "Nitrogen (ppm)", 
       y = "Total C Standing Stocks (µg C L-1)", 
       color = "Depth Class") +  # Axis labels
  scale_y_continuous(
    trans = 'log10',  # Apply log10 transformation to y-axis
    breaks = seq(min(Carbon_data$Total_plus_1), 500, by = 79),  # Set custom y-axis breaks
    limits = c(min(Carbon_data$Total_plus_1), 500),  # Extend y-axis to 500
    expand = c(0, 0)
  ) +  # Log transformation for y-axis
  scale_x_continuous(
    trans = 'log10' # Apply log10 transformation to x-axis
  ) + 
  scale_color_manual(values = depth_colors) +  # Custom colors for Depth Class
  theme_classic() +  # Use a minimal theme for the plot
  theme(
    axis.title.x = element_text(size = 14),  # Increase font size of x-axis title
    axis.title.y = element_text(size = 14),  # Increase font size of y-axis title
    axis.text.x = element_text(size = 12),   # Increase font size of x-axis tick labels
    axis.text.y = element_text(size = 12)    # Increase font size of y-axis tick labels
  )
#make a scatter plot of carbon vs P
Carbon_data %>%
  mutate(Depth_class = factor(Depth_class, levels = c('surface', 'subsurface', 'chl max', 'deep', 'very deep'))) %>%
  ggplot(aes(x = PHOSPHATE_PPM, y = Total_plus_1, color = Depth_class)) +  # Plot using the mutated data
  geom_point(size = 3) +  # Adds the scatter plot points with increased size +  # Adds the scatter plot points
  labs(x = "Phosphate (ppm)", 
       y = "Total C Standing Stocks (µg C L-1)", 
       color = "Depth Class") +  # Axis labels
  scale_y_continuous(
    trans = 'log10',  # Apply log10 transformation to y-axis
    breaks = seq(min(Carbon_data$Total_plus_1), 500, by = 79),  # Set custom y-axis breaks
    limits = c(min(Carbon_data$Total_plus_1), 500),  # Extend y-axis to 500
    expand = c(0, 0)
  ) +   # Log transformation for y axis
  scale_x_continuous(trans = 'log10') +  # Log transformation for x axis
  scale_color_manual(values = depth_colors) +  # Custom colors for Depth Class
  theme_classic() +  # Use a minimal theme for the plot
  theme(
    axis.title.x = element_text(size = 14),  # Increase font size of x axis title
    axis.title.y = element_text(size = 14),  # Increase font size of y axis title
    axis.text.x = element_text(size = 12),   # Increase font size of x axis tick labels
    axis.text.y = element_text(size = 12)) 
###############################################################################
#same set of 3 graphs as above, but for chla instead of carbon
#make a scatter plot of carbon vs N/P ratio
Carbon_data %>%
  mutate(Depth_class = factor(Depth_class, levels = c('surface', 'subsurface', 'chl max', 'deep', 'very deep'))) %>%
  ggplot(aes(x = N_P_ratio, y = chla_plus1, color = Depth_class)) +  # Plot using the mutated data
  geom_point(size = 2.2) +  # Adds the scatter plot points with increased size +  # Adds the scatter plot points
  labs(x = "N/P Ratio ppm (log transformed)", 
       y = "Chl-a WSW (µg L-1) (log transformed)", 
       color = "Depth Class") +  # Axis labels
  scale_y_continuous(trans = 'log10') +  # Log transformation for y axis
  scale_x_continuous(trans = 'log10') +  # Log transformation for x axis
  scale_color_manual(values = depth_colors) +  # Custom colors for Depth Class
  theme_classic() +  # Use a minimal theme for the plot
  theme(
    axis.title.x = element_text(size = 14),  # Increase font size of x axis title
    axis.title.y = element_text(size = 14),  # Increase font size of y axis title
    axis.text.x = element_text(size = 12),   # Increase font size of x axis tick labels
    axis.text.y = element_text(size = 12)) 

#make a scatter plot of carbon vs N 
Carbon_data %>%
  mutate(Depth_class = factor(Depth_class, levels = c('surface', 'subsurface', 'chl max', 'deep', 'very deep'))) %>%
  ggplot(aes(x = NITRATE_NITRITE_PPM, y = chla_plus1, color = Depth_class)) +  # Plot using the mutated data
  geom_point(size = 2.2) +  # Adds the scatter plot points with increased size +  # Adds the scatter plot points
  labs(x = "Nitrogen ppm (log transformed)", 
       y = "Chl-a WSW (µg L-1) (log transformed)", 
       color = "Depth Class") +  # Axis labels
  scale_y_continuous(trans = 'log10') +  # Log transformation for y axis
  scale_x_continuous(trans = 'log10') +  # Log transformation for x axis
  scale_color_manual(values = depth_colors) +  # Custom colors for Depth Class
  theme_classic() +  # Use a minimal theme for the plot
  theme(
    axis.title.x = element_text(size = 14),  # Increase font size of x axis title
    axis.title.y = element_text(size = 14),  # Increase font size of y axis title
    axis.text.x = element_text(size = 12),   # Increase font size of x axis tick labels
    axis.text.y = element_text(size = 12)) 

#make a scatter plot of carbon vs P
Carbon_data %>%
  mutate(Depth_class = factor(Depth_class, levels = c('surface', 'subsurface', 'chl max', 'deep', 'very deep'))) %>%
  ggplot(aes(x = PHOSPHATE_PPM, y = chla_plus1, color = Depth_class)) +  # Plot using the mutated data
  geom_point(size = 2.2) +  # Adds the scatter plot points with increased size +  # Adds the scatter plot points
  labs(x = "Phosphate ppm (log transformed)", 
       y = "Chl-a WSW (µg L-1) (log transformed)", 
       color = "Depth Class") +  # Axis labels
  scale_y_continuous(trans = 'log10') +  # Log transformation for y axis
  scale_x_continuous(trans = 'log10') +  # Log transformation for x axis
  scale_color_manual(values = depth_colors) +  # Custom colors for Depth Class
  theme_classic() +  # Use a minimal theme for the plot
  theme(
    axis.title.x = element_text(size = 14),  # Increase font size of x axis title
    axis.title.y = element_text(size = 14),  # Increase font size of y axis title
    axis.text.x = element_text(size = 12),   # Increase font size of x axis tick labels
    axis.text.y = element_text(size = 12)) 
################################################################################
#this code is for a coupled box for carbon grouped by depth class, seperated by distance from shore
Carbon_data %>%
  mutate(Position = factor(Position, levels = c('nearshore', 'middle', 'offshore')),  # Ordering Position
         Depth_class = factor(Depth_class, levels = c('surface', 'subsurface', 'chl max', 'deep', 'very deep'))) %>%  # Ordering Depth_class
  ggplot(aes(x = Depth_class, y = Total_plus_1, fill = Position)) +  # y = Total_plus_1, fill by Position
  geom_boxplot(position = "dodge") +  # Create the box plot
  ggtitle("Distribution of Carbon Standing Stocks by Depth Class and Position") + 
  xlab("Depth Class") + 
  ylab("Total C Standing Stocks (µg C L-1)") +
  scale_fill_manual(values = distance_colors) +  # Use scale_fill_manual instead of scale_color_manual for fill
  scale_y_continuous(
    trans = 'log10',  # Apply log10 transformation to y-axis
    breaks = seq(min(Carbon_data$Total_plus_1), 500, by = 79),  # Set custom y-axis breaks
    limits = c(min(Carbon_data$Total_plus_1), 500),  # Extend y-axis to 500
    expand = c(0, 0)
  ) +  # Log10 transformation for y-axis
  theme_classic() +
theme(
  axis.title.x = element_text(size = 14),  # Increase font size of x axis title
  axis.title.y = element_text(size = 14),  # Increase font size of y axis title
  axis.text.x = element_text(size = 12),   # Increase font size of x axis tick labels
  axis.text.y = element_text(size = 12))

#now a more sinple version of the same plot that does not seperate by depth class
Carbon_data %>%
  mutate(Position = factor(Position, levels = c('nearshore', 'middle', 'offshore'))) %>%  # Ordering Position
  ggplot(aes(x = Position, y = Total_plus_1, fill = Position)) +  # Map Position to x-axis and Total_plus_1 to y-axis
  geom_boxplot() +  # Create the box plot
  ggtitle("Distribution of Carbon Standing Stocks by Shore Position") + 
  xlab("Shore Position") + 
  ylab("Total C Standing Stocks (µg C L-1) (log transformed)") +
  scale_fill_manual(values = distance_colors) +  # Custom colors for Position
  scale_y_continuous(trans = 'log10', expand = c(0, 0)) +  # Log10 transformation for y-axis
  theme_classic() +
  theme(
  axis.title.x = element_text(size = 14),  # Increase font size of x axis title
  axis.title.y = element_text(size = 14),  # Increase font size of y axis title
  axis.text.x = element_text(size = 12),   # Increase font size of x axis tick labels
  axis.text.y = element_text(size = 12))
################################################################################
#facet wrapped bar plots for carbon standing stocks and each station. Each depth has a different plot
# Assuming your data is in C_data

C_data <- Carbon_data %>%
  mutate(Depth_class = factor(Depth_class, levels = c('surface', 'subsurface', 'chl max', 'deep', 'very deep')),
         Station = as.factor(Station))  # Ensure station is a factor

# Create the bar plot with facet wrapping by Depth_class, arranged in a 2x3 grid
ggplot(C_data, aes(x = Station, y = Total_plus_1, fill = Depth_class)) +
  geom_bar(stat = "identity", position = "dodge") +  # Set bars with dodge position
  facet_wrap(~ Depth_class, scales = "free", ncol = 1) +  # Facet by Depth_class with 3 columns
  ggtitle("Carbon Standing Stocks by Station and Depth Class") + 
  xlab("Station") +  # Only show Station on the x-axis
  ylab("Carbon Standing Stocks (µg C L-1)") +
  scale_fill_manual(values = depth_colors) +  # Apply the custom color palette for Depth_class
  theme_minimal() +  # Minimal theme
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 10),  # Rotate and adjust x-axis labels
        axis.text = element_text(size = 10),  # Make x and y axis labels readable
        legend.position = "none",   # Remove the legend
        strip.text.x = element_text(size = 12),  # Increase the size of facet labels for readability
        panel.spacing = unit(1, "lines")) +  # Increase space between panels to fit labels
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 9))  # Adjust label size for better visibility
################################################################################
#Carbon by functional grouping bar plot - summed bars

#read CSV
carbon_taxa <- read.csv ("Carbon-taxa.csv")

# First, let's check the column types to identify which ones are numeric
str(carbon_taxa)

# Reshape the data, excluding 'SAMPLE_ID' and other non-numeric columns
carbon_long <- carbon_taxa %>%
  select(-SAMPLE_ID, -taxa) %>%  # Exclude non-numeric columns (e.g., SAMPLE_ID and taxa)
  pivot_longer(cols = -Functional_type,  # Exclude 'Functional_type' to keep it intact
               names_to = "Station", 
               values_to = "Carbon")

# Ensure 'Functional_type' is a factor and ordered correctly
carbon_long$Functional_type <- factor(carbon_long$Functional_type, 
                                      levels = c("Phototroph", "CM", "NCM", "Heterotroph"))

# Summarize the data by summing the 'Carbon' values for each 'Functional_type'
carbon_summarized <- carbon_long %>%
  # Ensure 'Carbon' is numeric before summarizing
  mutate(Carbon = as.numeric(Carbon)) %>% 
  group_by(Functional_type) %>%
  summarise(Sum_Carbon = sum(Carbon, na.rm = TRUE), .groups = "drop")

# Create the box plot of summed carbon values by functional type
ggplot(carbon_summarized, aes(x = Functional_type, y = Sum_Carbon, fill = Functional_type)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = functional_colors) +  # Apply custom colors
  labs(title = "Summed Carbon Values by Functional Type",
       x = "Functional Type",
       y = "Summed Carbon (µL C L-1)") +
  theme_classic() +
  theme(legend.position = "right") + # Optionally hide the legend if you don't need it
  theme(
    axis.title.x = element_text(size = 14),  # Increase font size of x axis title
    axis.title.y = element_text(size = 14),  # Increase font size of y axis title
    axis.text.x = element_text(size = 12),   # Increase font size of x axis tick labels
    axis.text.y = element_text(size = 12))
################################################################################
#this code is for a coupled box for carbon grouped by depth class, colored by functional grouping
#load dada
Carbon_data <- read.csv ("Carbon_data.csv")
#make it long
Carbon_data_long <- Carbon_data %>%
  pivot_longer(cols = c("Phototroph", "Heterotroph", "CM", "NCM"),  # Specify the columns to pivot
               names_to = "Functional_Type",  # The new column for the names
               values_to = "carbon")  

# Remove rows with NA values in the Carbon column
Carbon_data_long <- Carbon_data_long %>%
  filter(!is.na(carbon))

# Check if there are any zero or negative values in the 'Carbon' column
# and adjust if necessary (if you want to keep a log scale, you cannot have 0 or negative values).
Carbon_data_long <- Carbon_data_long %>%
  mutate(carbon = ifelse(carbon <= 0, NA, carbon))  # Replace non-positive values with NA


# Reorder Depth_class in Carbon_data_long
Carbon_data_long <- Carbon_data_long %>%
  mutate(Depth_class = factor(Depth_class, 
                              levels = c('surface', 'subsurface', 'chl max', 'deep', 'very deep')))

# Reorder Functional_Type in Carbon_data_long for the legend
Carbon_data_long <- Carbon_data_long %>%
  mutate(Functional_Type = factor(Functional_Type, 
                                  levels = c("Phototroph", "CM", "NCM", "Heterotroph")))

# Define functional type colors
functional_colors <- c("Phototroph" = "#7fc48f", 
                       "CM" = "#008c9f", 
                       "NCM" = "#d0bd73", 
                       "Heterotroph" = "#8d8bbf", 
                       "NA" = "#000000")

# Define y_min value
y_min <- 0.01  # Adjust as necessary

# Create the plot
ggplot(Carbon_data_long, aes(x = Depth_class, y = carbon, fill = Functional_Type)) +
  geom_boxplot(position = "dodge") +  # Boxplots for each Depth_class and Functional_type
  ggtitle("Distribution of Carbon Standing Stocks by Depth Class and Functional Type") +
  xlab("Depth Range") +
  ylab("Microplankton BM (µg C L-1)") +
  scale_fill_manual(values = functional_colors) +  # Color by functional type
  scale_y_continuous(
    trans = 'log10',  # Apply log10 transformation to y-axis
    breaks = c(0.01, 0.1, 1, 10, 50, 100, 150, 275, 400),  # Define breaks starting from 0.01
    labels = scales::comma_format(),  # Format labels for easy reading (e.g., 0.01, 1, 10, etc.)
    limits = c(y_min, 400),  # Log scale, set limits for better visual clarity
    expand = c(0, 0)
  ) + 
  geom_vline(xintercept = seq(0.5, length(unique(Carbon_data_long$Depth_class)) - 0.5, by = 1), 
             color = "grey", linetype = "solid") +  # Vertical dashed lines
  theme_classic() +
  theme(
    axis.title.x = element_text(size = 12),  # Increase font size of x-axis title
    axis.title.y = element_text(size = 12),  # Increase font size of y-axis title
    axis.text.x = element_text(size = 12),   # Increase font size of x-axis tick labels
    axis.text.y = element_text(size = 12)    # Increase font size of y-axis tick labels
  )

################################################################################
#this code is for a coupled box for carbon grouped by shore position, colored by functional grouping
#load dada
Carbon_data <- read.csv ("Carbon_data.csv")
#make it long
Carbon_data_long <- Carbon_data %>%
  pivot_longer(cols = c("Phototroph", "Heterotroph", "CM", "NCM"),  # Specify the columns to pivot
               names_to = "Functional_Type",  # The new column for the names
               values_to = "carbon")  

# Remove rows with NA values in the Carbon column
Carbon_data_long <- Carbon_data_long %>%
  filter(!is.na(carbon))

# Check if there are any zero or negative values in the 'Carbon' column
# and adjust if necessary (if you want to keep a log scale, you cannot have 0 or negative values).
Carbon_data_long <- Carbon_data_long %>%
  mutate(carbon = ifelse(carbon <= 0, NA, carbon))  # Replace non-positive values with NA


# Reorder Shore position in Carbon_data_long
Carbon_data_long <- Carbon_data_long %>%
  mutate(Position = factor(Position, 
                              levels = c('nearshore', 'middle', 'offshore')))

# Reorder Functional_Type in Carbon_data_long for the legend
Carbon_data_long <- Carbon_data_long %>%
  mutate(Functional_Type = factor(Functional_Type, 
                                  levels = c("Phototroph", "CM", "NCM", "Heterotroph")))

# Define functional type colors
functional_colors <- c("Phototroph" = "#7fc48f", 
                       "CM" = "#008c9f", 
                       "NCM" = "#d0bd73", 
                       "Heterotroph" = "#8d8bbf", 
                       "NA" = "#000000")

# Define y_min value
y_min <- 0.01  # Adjust as necessary

# Create the plot
ggplot(Carbon_data_long, aes(x = Position, y = carbon, fill = Functional_Type)) +
  geom_boxplot(position = "dodge") +  # Boxplots for each Depth_class and Functional_type
  ggtitle("Distribution of Carbon Standing Stocks by Shore Position and Functional Type") +
  xlab("Position to Shore") +
  ylab("Microplankton BM (µg C L-1)") +
  scale_fill_manual(values = functional_colors) +  # Color by functional type
  scale_y_continuous(
    trans = 'log10',  # Apply log10 transformation to y-axis
    breaks = c(0.01, 0.1, 1, 10, 50, 100, 150, 275, 400),  # Define breaks starting from 0.01
    labels = scales::comma_format(),  # Format labels for easy reading (e.g., 0.01, 1, 10, etc.)
    limits = c(y_min, 400),  # Log scale, set limits for better visual clarity
    expand = c(0, 0)
  ) + 
  geom_vline(xintercept = seq(0.5, length(unique(Carbon_data_long$Depth_class)) - 0.5, by = 1), 
             color = "grey", linetype = "solid") +  # Vertical dashed lines
  theme_classic() +
  theme(
    axis.title.x = element_text(size = 12),  # Increase font size of x-axis title
    axis.title.y = element_text(size = 12),  # Increase font size of y-axis title
    axis.text.x = element_text(size = 12),   # Increase font size of x-axis tick labels
    axis.text.y = element_text(size = 12)    # Increase font size of y-axis tick labels
  )

################################################################################
#Box plot, no outliers removed

#load new CSV
functional_summed <- read.csv ("Functional_groups_carbon.csv")

# make that shit long as hell
functional_long <- functional_summed %>%
  pivot_longer(cols = -Functional_type,  # Exclude 'Functional_type' to keep it intact
               names_to = "Station", 
               values_to = "Carbon")

# Ensure 'Functional_type' is a factor and ordered correctly
functional_long$Functional_type <- factor(functional_long$Functional_type, 
                                      levels = c("Phototroph", "CM", "NCM", "Heterotroph"))

# Filter out rows where Carbon is 0
functional_long_no_0 <- functional_long %>%
  filter(Carbon != 0)

# Create the box plot using ggplot2
ggplot(functional_long_no_0, aes(x = Functional_type, y = Carbon, fill = Functional_type)) +
  geom_boxplot() +
  labs(title = "Carbon Values by Functional Type",
       x = "Functional Type",
       y = "Carbon (µL C L-1)") +
  scale_y_continuous(trans = 'log10', expand = c(0, 0)) + 
  scale_fill_manual(values = functional_colors) +  # Custom colors for fill (not color)
  theme_classic() +
  theme(
    axis.title.x = element_text(size = 14),  # Increase font size of x axis title
    axis.title.y = element_text(size = 14),  # Increase font size of y axis title
    axis.text.x = element_text(size = 12),   # Increase font size of x axis tick labels
    axis.text.y = element_text(size = 12))
################################################################################
#stacked bar plot of carbon vs depth class, w/ functional contributions

#seperate station number and depth into two seperate columns
functional_depth_station_seperate <- functional_long_no_0 %>%
  separate(Station, into = c("Station", "Depth"), sep = "_")

#replace names of depth classes
functional_depth_station_seperate$Depth <- replace(functional_depth_station_seperate$Depth, functional_depth_station_seperate$Depth=='CLM', 'chl max')
functional_depth_station_seperate$Depth <- replace(functional_depth_station_seperate$Depth, functional_depth_station_seperate$Depth=='D', 'deep')
functional_depth_station_seperate$Depth <- replace(functional_depth_station_seperate$Depth, functional_depth_station_seperate$Depth=='verydeep', 'very deep')
functional_depth_station_seperate$Depth <- replace(functional_depth_station_seperate$Depth, functional_depth_station_seperate$Depth=='surf', 'surface')


#Set the desired order for the Depth column
functional_depth_station_seperate$Depth <- factor(functional_depth_station_seperate$Depth,
                                                  levels = c('surface', 'subsurface', 'chl max', 'deep', 'very deep'))

# 1. Group by Depth and Functional_type and calculate the average Carbon value
carbon_avg <- functional_depth_station_seperate %>%
  group_by(Depth, Functional_type) %>%
  summarise(Average_Carbon = mean(Carbon, na.rm = TRUE)) %>%
  ungroup()


# 2. Create the stacked bar plot
ggplot(carbon_avg, aes(x = Depth, y = Average_Carbon, fill = Functional_type)) +
  geom_bar(stat = "identity") +   # Use stat = "identity" to directly use the average values
  labs(title = "Average Carbon by Depth and Functional Type",
       x = "Depth",
       y = "Average Carbon (µL C L-1)") +
  scale_fill_manual(values = functional_colors) +  # Apply custom colors (optional)
  theme_minimal()

ggplot(carbon_avg, aes(x = Depth, y = Average_Carbon, fill = Functional_type)) +
  geom_bar(stat = "identity") +   # Use stat = "identity" to directly use the average values
  geom_text(aes(label = ifelse(Average_Carbon >= 2, round(Average_Carbon, 2), "")),  # Only show labels if value >= 2
            position = position_stack(vjust = 0.5),     # Place text at the center of the bars
            color = "white", size = 4) +                # Text color and size
  labs(title = "Average Carbon by Depth and Functional Type",
       x = "Depth",
       y = "Average Carbon (µL C L-1)") +
  scale_fill_manual(values = functional_colors) +  # Apply custom colors (optional)
  theme_minimal()
###############################################################################
#Dot plot all Carbon BM at all stations
Carbon_data <- Carbon_data %>%
  mutate(Depth_class = factor(Depth_class, levels = c('surface', 'subsurface', 'chl max', 'deep', 'very deep')),
         Station = as.factor(Station))  # Ensure station is a factor

ggplot(Carbon_data, aes(x = factor(Station), y = Total_plus_1 , color = Depth_class)) +
  geom_jitter(width = 0.1, height = 0, size = 3, alpha = 1) +
  labs(title = "Carbon biomass by Station and Depth", x = "Station", y = "Carbon BM (log x+1)") +
  scale_color_manual(values = depth_colors) +  # Apply custom color palette
  scale_y_continuous(breaks = seq(min(Carbon_data$Total_plus_1), max(Carbon_data$Total_plus_1), by = 0.5).) +  
  scale_y_continuous(trans = 'log10', expand = c(0, 0)) +# Adjust y-axis tick marks
  theme_classic() +
  theme(
  axis.text.x = element_text(angle = 45, hjust = 1),  # Rotate x-axis labels
  panel.grid.major.y = element_blank(),               # Remove major horizontal grid lines
  panel.grid.minor.y = element_blank(),               # Remove minor horizontal grid lines
  panel.grid.major.x = element_line(color = "gray", size = 0.2),  # Keep vertical grid lines (x-axis)
  panel.grid.minor.x = element_line(color = "gray", size = 0.25)  # Keep minor vertical grid lines (x-axis)
)

ggplot(Carbon_data, aes(x = factor(Station), y = Total_plus_1, color = Depth_class)) +
  geom_jitter(width = 0.1, height = 0, size = 3, alpha = 1) +
  labs(title = "Carbon Biomass by Station and Depth", x = "Station", y = "Carbon BM (log x+1)") +
  scale_color_manual(values = depth_colors) +  # Apply custom color palette
  scale_y_continuous(
    trans = 'log10',  # Apply log10 transformation to y-axis
    breaks = seq(min(Carbon_data$Total_plus_1), max(Carbon_data$Total_plus_1), by = 49),  # Set custom y-axis breaks
    expand = c(0, 0)  # Remove extra space at the bottom of the plot
  ) +
  theme_classic() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),  # Rotate x-axis labels
    panel.grid.major.y = element_blank(),               # Remove major horizontal grid lines
    panel.grid.minor.y = element_blank(),               # Remove minor horizontal grid lines
    panel.grid.major.x = element_line(color = "gray", size = 0.2),  # Keep vertical grid lines (x-axis)
    panel.grid.minor.x = element_line(color = "gray", size = 0.25))
###############################################################################
#box plots for all env variables (y-axis) vs depth rages (x-axis) - couldnt get facet to work, looked ugly
#read in sheet 
# Load necessary libraries
library(ggplot2)
library(tidyr)

# Read your data (ensure the file path is correct)
Carbon_data <- read.csv("Carbon_data.csv")

# Check for missing values in Depth_class (if necessary)
sum(is.na(Carbon_data$Depth_class))

# Clean and ensure Depth_class is a factor with the correct levels
Carbon_data <- Carbon_data %>%
  mutate(Depth_class = str_trim(Depth_class)) %>%  # Trim spaces
  mutate(Depth_class = factor(Depth_class, levels = c('surface', 'subsurface', 'chl max', 'deep', 'very deep')))

# List of environmental variables you want to plot
env_vars <- c("TEMPERATURE", "SALINITY", "OXYGEN_PPM", "NITRATE_NITRITE_PPM", "PHOSPHATE_PPM", "N_P_ratio", 
              "Chla_WSW", "Chla_less20", "Chla_greater20", "fCO2_MEA_UATM","pH","DIC_UMOL_KG", "TA_UMOL_KG", "CARBONATE_PPM","SILICATE_PPM")

# Custom color palette for Depth_class
depth_colors <- c("surface" = "#ce4993", 
                  "subsurface" = "#e8702a", 
                  "chl max" = "#36802d", 
                  "deep" = "#0c457d", 
                  "very deep" = "#6a0d83")

Carbon_data <- Carbon_data %>%
  filter(!is.na(Depth_class))  # Remove rows with NA in Depth_class

# Loop through each variable and create a plot
for (env_var in env_vars) {
  
  # Create the boxplot for the current variable
  p <- ggplot(Carbon_data, aes(x = Depth_class, y = .data[[env_var]], fill = Depth_class)) +
    geom_boxplot() +
    scale_fill_manual(values = depth_colors) +  # Apply custom colors for Depth_class
    theme_classic() +
    theme(
      legend.position = "none",
      panel.border = element_rect(color = "black", fill = NA, size = 1),
      axis.title.x = element_text(size = 12),  # Increase font size of x axis title
      axis.title.y = element_text(size = 12),  # Increase font size of y axis title
      axis.text.x = element_text(size = 12),   # Increase font size of x axis tick labels
      axis.text.y = element_text(size = 12),
      ) +  # Rotate x-axis labels for readability
    labs(
      y = env_var,  # Dynamic y-axis label based on the current environmental variable
      x = "Depth Range",
      title = paste("Boxplot for", env_var)  # Title based on the current environmental variable
    )
  
  # Print the plot for the current variable
  print(p)
}
###############################################################################
#Pearson correlation coefficient heatmap using pheatmap
# Install and load pheatmap if necessary
#install.packages("pheatmap")
# Load necessary libraries
library(dplyr)
library(tidyr)
library(pheatmap)

# Load your dataset (assuming the updated data is in the "Carbon_data.csv" file)
Carbon_data <- read.csv("Carbon_data.csv")

# Select relevant columns for environmental variables and functional types
env_vars <- c("TEMPERATURE", "SALINITY", "OXYGEN_PPM", "NITRATE_NITRITE_PPM", 
              "PHOSPHATE_PPM", "N_P_ratio", "CARBONATE_PPM", "SILICATE_PPM", 
              "fCO2_MEA_UATM", "pH", "DIC_UMOL_KG", "TA_UMOL_KG", 
              "Chla_greater20", "Chla_less20")

functional_types <- c("Total_C_standing_stocks","CM", "NCM", "Heterotroph", "Phototroph")

# Prepare the data: Select the columns for environmental variables and functional groups
data_for_correlation <- Carbon_data %>%
  select(all_of(env_vars), all_of(functional_types))

# Compute the correlation matrix (Pearson correlation) between env_vars and functional_types
# Using cor.test to also compute the p-values for each pair

cor_matrix <- matrix(ncol = length(functional_types), nrow = length(env_vars))

# Loop through the environmental variables and functional types to calculate correlations
for (i in seq_along(env_vars)) {
  for (j in seq_along(functional_types)) {
    cor_matrix[i, j] <- cor(data_for_correlation[[env_vars[i]]], 
                            data_for_correlation[[functional_types[j]]], 
                            use = "complete.obs", method = "pearson")
  }
}

# Convert the matrix to a data frame and assign row and column names
rownames(cor_matrix) <- env_vars
colnames(cor_matrix) <- functional_types

# Now compute p-values for masking (non-significant correlations will be set to NA)
p_values <- matrix(ncol = length(functional_types), nrow = length(env_vars))

for (i in seq_along(env_vars)) {
  for (j in seq_along(functional_types)) {
    p_values[i, j] <- cor.test(data_for_correlation[[env_vars[i]]], 
                               data_for_correlation[[functional_types[j]]], 
                               use = "complete.obs", method = "pearson")$p.value
  }
}

# Mask the correlations where the p-value is >= 0.05 (set them to NA)
cor_matrix[p_values >= 0.05] <- NA  # Set non-significant correlations to NA

# Reorder the correlation matrix by the specified row_order
row_order <- c("TEMPERATURE", "SALINITY", "OXYGEN_PPM", "NITRATE_NITRITE_PPM", 
               "PHOSPHATE_PPM", "N_P_ratio", "CARBONATE_PPM", "SILICATE_PPM", 
               "fCO2_MEA_UATM", "pH", "DIC_UMOL_KG", "TA_UMOL_KG", 
               "Chla_greater20", "Chla_less20")

# Ensure the row names of cor_matrix match the order of environmental variables
cor_matrix <- cor_matrix[row_order, , drop = FALSE]  # Reorder rows based on the row_order vector

# Create the heatmap using pheatmap (no need to transpose since we want env_vars on the left and functional_types on top)
pheatmap(cor_matrix, 
         cluster_rows = FALSE,          # Don't cluster environmental variables
         cluster_cols = FALSE,          # Don't cluster functional types
         color = colorRampPalette(c("blue", "white", "red"))(100),  # Color scale
         main = "Correlation Heatmap: Environmental Variables vs Functional Types",
         fontsize_number = 10,           # Font size for numbers on the heatmap
         angle_col = 45,                # Rotate column names for readability
         breaks = seq(-1, 1, length.out = 101),  # Define breaks to ensure the color scale centers on 0
         display_numbers = TRUE,        # Display numbers where valid
         number_color = "black",        # Color for the numbers (set to black for better contrast)
         border_color = "grey",             # Optional: Removes borders between cells
         cellheight = 35,               # Adjust cell height for readability (optional)
         cellwidth = 35,                # Adjust cell width for readability (optional)
         na_col = "white",              # Ensure that NA values are colored white (i.e., blank)
         legend = TRUE,                 # Show the legend for the color scale
         legend_breaks = c(-1, -0.5, 0, 0.5, 1)  # Custom breaks for the color scale
)
##################################################################################
Genus_data <- read.csv("carbon-taxa.csv")

# Transpose the dataset (flip rows and columns)
Genus_transposed_data <- t(Genus_data)

# Convert it to a data frame
Genus_transposed_data <- as.data.frame(Genus_transposed_data)

transposed_data <- as.data.frame(Genus_transposed_data)

# Set the first row as the column names
colnames(Genus_transposed_data) <- Genus_transposed_data[1, ]

# Calculate the Simpson Diversity index (D) and then calculate 1/D
simpson_diversity_genus <- Genus_transposed_data %>%
  group_by(Sample) %>%
  summarise(
    D = sum((table(Species1) * (table(Species1) - 1)) / (sum(table(Species1)) * (sum(table(Species1)) - 1))),
    D = 1 / D  # Calculate 1/D (Diversity)
  )

##############################################################################
Counts_genus <- read.csv("Counts_genus.csv")

#install.packages('janitor')
library(janitor)

simpson_function <- function(n) {
  return (n*(n-1))
}

simpson_table <- sapply(Counts_genus[-1:-5],simpson_function)

# Convert it to a data frame
simpson_table <- as.data.frame(simpson_table)

#make summed row at bottom of column
cringe <- simpson_table %>%
  adorn_totals("row",,,,everything())




