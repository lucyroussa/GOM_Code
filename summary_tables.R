#load packages
library(dplyr)
library(ggplot2)
library(tidyr)
library(dunn.test)
#read in sheet
Carbon_data <- read.csv ("Carbon_data.csv")

#look at it 
summary(Carbon_data)

#make summary table of env variables by depth class
env_summary_by_depth <- Carbon_data %>%
  group_by(Depth_class) %>%
  summarize(
    Temp_range = range(TEMPERATURE, na.rm = TRUE),
    temp_mean = mean(TEMPERATURE, na.rm = TRUE),
    temp_sd = sd(TEMPERATURE, na.rm = TRUE),
    Sal_range = range(SALINITY, na.rm = TRUE),
    Sal_mean = mean(SALINITY, na.rm = TRUE),
    Sal_sd = sd(SALINITY, na.rm = TRUE),
    N_range = range(NITRATE_NITRITE_PPM, na.rm = TRUE),
    N_mean = mean(NITRATE_NITRITE_PPM, na.rm = TRUE),
    N_sd = sd(NITRATE_NITRITE_PPM, na.rm = TRUE),
    P_range = range(PHOSPHATE_PPM, na.rm = TRUE),
    P_mean = mean(PHOSPHATE_PPM, na.rm = TRUE),
    P_sd = sd(PHOSPHATE_PPM, na.rm = TRUE),
    Si_range = range(SILICATE_PPM, na.rm = TRUE),
    Si_mean = mean(SILICATE_PPM, na.rm = TRUE),
    Si_sd = sd(SILICATE_PPM, na.rm = TRUE),
    pH_range = range(pH, na.rm = TRUE),
    pH_mean = mean(pH, na.rm = TRUE),
    pH_sd = sd(pH, na.rm = TRUE),
    CO2_range = range(fCO2_MEA_UATM, na.rm = TRUE),
    CO2_mean = mean(fCO2_MEA_UATM, na.rm = TRUE),
    CO2_sd = sd(fCO2_MEA_UATM, na.rm = TRUE),
    DIC_range = range(DIC_UMOL_KG, na.rm = TRUE),
    DIC_mean = mean(DIC_UMOL_KG, na.rm = TRUE),
    DIC_sd = sd(DIC_UMOL_KG, na.rm = TRUE),
    TA_range = range(TA_UMOL_KG, na.rm = TRUE),
    TA_mean = mean(TA_UMOL_KG, na.rm = TRUE),
    TA_sd = sd(TA_UMOL_KG, na.rm = TRUE),
    O2_range = range(OXYGEN_PPM, na.rm = TRUE),
    O2_mean = mean(OXYGEN_PPM, na.rm = TRUE),
    O2_sd = sd(OXYGEN_PPM, na.rm = TRUE),
    chla_range = range(Chla_WSW, na.rm = TRUE),
    chla_mean = mean(Chla_WSW, na.rm = TRUE),
    chla_sd = sd(Chla_WSW, na.rm = TRUE),
    
    .groups = "drop"
  )

#print table
print(env_summary_by_depth)

# Export summary_stats to a CSV file
write.csv(env_summary_by_depth, "env_summary_by_depth.csv", row.names = FALSE)
################################################################################
#Statistical test to see if there are sig. dif. between carbon at different depth classes (associated with box plots)
#before doing stats, check if my data is normal to determine which stats test to use
#do sharpiro test for carbon  data
shapiro_test_C <- Carbon_data %>%
  group_by(Depth_class) %>%
  summarise(shapiro_test = shapiro.test(Total_C_standing_stocks)$p.value)

#the result was not normal (non-parametric), so I will be using a Kruskal-Wallis Test in place of a 
#one-way ANOVA

kruskal_CarbonBM_result <- kruskal.test(Total_C_standing_stocks ~ Depth_class, data = Carbon_data)

#The test reported a p-value less than 0.05, so we can do a
#post-hoc test to figure out which depth classes are sig. dif. from eachother

# Install dunn.test package if not already installed
install.packages("dunn.test")
library(dunn.test)

# Perform Dunn's test for pairwise comparisons
dunn_result <- dunn.test(C_data$Total_C_standing_stocks, C_data$Depth_class, 
                         kw = TRUE, label = TRUE, wrap = TRUE)

# View the results
dunn_result
################################################################################
#do the same process for chla
#do sharpiro test for carbon  data
shapiro_test_chla <- Carbon_data %>%
  group_by(Depth_class) %>%
  summarise(shapiro_test = shapiro.test(Chla_WSW)$p.value)
#the result was not normal (non-parametric), so I will be using a Kruskal-Wallis Test in place of a 
#one-way ANOVA

kruskal_Chl_result <- kruskal.test(Chla_WSW ~ Depth_class, data = Carbon_data)

#The test reported a p-value less than 0.05, so we can do a
#post-hoc test to figure out which depth classes are sig. dif. from eachother

# Install dunn.test package if not already installed

# Perform Dunn's test for pairwise comparisons
dunn_result_chla <- dunn.test(Carbon_data$Chla_WSW, Carbon_data$Depth_class, 
                         kw = TRUE, label = TRUE, wrap = TRUE)

# View the results
dunn_result_chla
################################################################################
#Do the same stats process for carbon and position from shore:
kruskal_CarbonBM_position_result <- kruskal.test(Total_C_standing_stocks ~ Position, data = Carbon_data)

dunn_result_position <- dunn.test(Carbon_data$Total_C_standing_stocks, Carbon_data$Position, 
                              kw = TRUE, label = TRUE, wrap = TRUE)
################################################################################
#same stats test with carbon, but splitting position from shore by depth class (more parsed out)
# Create a function to apply Kruskal-Wallis test for each Depth_class
kruskal_test_position_depthclass_results <- Carbon_data %>%
  group_by(Depth_class) %>%
  do({
    # Perform Kruskal-Wallis test within each Depth_class
    position_depthclass_test_result <- kruskal.test(Total_C_standing_stocks ~ Position, data = .)
    # Return the test result
    data.frame(Depth_class = unique(.$Depth_class), p_value = position_depthclass_test_result$p.value)
  })
print(kruskal_test_position_depthclass_results)
################################################################################
#statistical test to see if there is a significant relationship between nutrients & carbon
# Spearman Rank Correlation test
#Carbon standing stocks vs Nitrogen: 
cor.test(Carbon_data$NITRATE_NITRITE_PPM, Carbon_data$Total_C_standing_stocks, method = "spearman")
#Carbon standing stocks vs Phosphate:
cor.test(Carbon_data$PHOSPHATE_PPM, Carbon_data$Total_C_standing_stocks, method = "spearman")
#Carbon standing stocks vs N/P:
cor.test(Carbon_data$N_P_ratio, Carbon_data$Total_C_standing_stocks, method = "spearman")
#Do the same, but fur chla WSW
#Carbon standing stocks vs Nitrogen: 
cor.test(Carbon_data$NITRATE_NITRITE_PPM, Carbon_data$Chla_WSW, method = "spearman")
#Carbon standing stocks vs Phosphate:
cor.test(Carbon_data$PHOSPHATE_PPM, Carbon_data$Chla_WSW, method = "spearman")
#Carbon standing stocks vs N/P:
cor.test(Carbon_data$N_P_ratio, Carbon_data$Chla_WSW, method = "spearman")

################################################################################
#Statistical test to see if there are sig. dif. between LOG TRANSFORMED carbon at different depth classes (associated with box plots)
#before doing stats, check if my data is normal to determine which stats test to use
#do sharpiro test for carbon  data
shapiro_test_C <- Carbon_data %>%
  group_by(Depth_class) %>%
  summarise(shapiro_test = shapiro.test(C_BM_log)$p.value)

#the result was not normal (non-parametric), so I will be using a Kruskal-Wallis Test in place of a 
#one-way ANOVA

kruskal_CarbonBM_result <- kruskal.test(C_BM_log ~ Depth_class, data = Carbon_data)

#The test reported a p-value less than 0.05, so we can do a
#post-hoc test to figure out which depth classes are sig. dif. from eachother

# Install dunn.test package if not already installed
install.packages("dunn.test")
library(dunn.test)

# Perform Dunn's test for pairwise comparisons
dunn_result <- dunn.test(Carbon_data$C_BM_log, Carbon_data$Depth_class, 
                         kw = TRUE, label = TRUE, wrap = TRUE)

# View the results
dunn_result
