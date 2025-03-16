library(vegan)
ENV_data <- read.csv ("Count_data_surface_CSV.csv") #read CSV into R

ENV_numeric_only_with_NA <- ENV_data[, !(names(ENV_data) %in% c("Station", "Transect", "Region", "Position"))]

ENV_numeric_only_with_NA[is.na(ENV_numeric_only_with_NA)] <- ""

ENV_NArows_deleted <- na.omit(ENV_data) #take out rows with NA in them
ENV_NArows_deleted_ready <- ENV_NArows_deleted[, !(names(ENV_NArows_deleted) %in% c("Station", "Transect", "Region", "Position"))]

No_NA_nmds = metaMDS(ENV_NArows_deleted_ready, distance = "bray")

plot(No_NA_nmds)

#extract NMDS scores (x and y coordinates) to plot in ggplot2
data.scores = as.data.frame(scores(No_NA_nmds)$sites)

#add columns to data frame 
data.scores$Station = ENV_NArows_deleted$Station
data.scores$Transect = ENV_NArows_deleted$Transect
data.scores$Region = ENV_NArows_deleted$Region
data.scores$Position = ENV_NArows_deleted$Position

#check to see if the coordinates are associated with the sample info
head(data.scores)

library(ggplot2)
#nmds by transect and position from shore
nMDS_transect = ggplot(data.scores, aes(x = NMDS1, y = NMDS2)) + 
  geom_point(size = 4, aes( shape = Position, colour = Transect)) + 
  theme(axis.text.y = element_text(colour = "black", size = 12, face = "bold"),
        axis.text.x = element_text(colour = "black", face = "bold", size = 12), 
        legend.text = element_text(size = 12, face ="bold", colour ="black"), 
        legend.position = "right", axis.title.y = element_text(face = "bold", size = 14), 
        axis.title.x = element_text(face = "bold", size = 14, colour = "black"), 
        legend.title = element_text(size = 14, colour = "black", face = "bold"), 
        panel.background = element_blank(), 
        panel.border = element_rect(colour = "black", fill = NA, linewidth = 1.2),
        legend.key=element_blank()) + 
  labs(x = "NMDS1", colour = "Transect", y = "NMDS2", shape = "Position")

nMDS_transect

#nmds by region and position from shore
nMDS_region = ggplot(data.scores, aes(x = NMDS1, y = NMDS2)) + 
  geom_point(size = 4, aes( shape = Position, colour = Region)) + 
  theme(axis.text.y = element_text(colour = "black", size = 12, face = "bold"),
        axis.text.x = element_text(colour = "black", face = "bold", size = 12), 
        legend.text = element_text(size = 12, face ="bold", colour ="black"), 
        legend.position = "right", axis.title.y = element_text(face = "bold", size = 14), 
        axis.title.x = element_text(face = "bold", size = 14, colour = "black"), 
        legend.title = element_text(size = 14, colour = "black", face = "bold"), 
        panel.background = element_blank(), 
        panel.border = element_rect(colour = "black", fill = NA, linewidth = 1.2),
        legend.key=element_blank()) + 
  labs(x = "NMDS1", colour = "Region", y = "NMDS2", shape = "Position")

nMDS_region

