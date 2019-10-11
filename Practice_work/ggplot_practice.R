#read cleaned bird data 
clean <- read.csv("./Data/01_Cleaned_bird_data.csv")

library(ggplot2)

ggplot(clean, aes(x=Family, y=tarsus, color=Gender)) + 
  geom_boxplot() +
  coord_cartesian(ylim = c(0, 150))
