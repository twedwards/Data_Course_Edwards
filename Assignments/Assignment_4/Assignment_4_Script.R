
library(ggplot2)

df = read.delim("../../../Data_Course/Data/ITS_mapping.csv",header = TRUE,sep = "\t")

summary(df)






ggplot(df, aes(x=Ecosystem, y=Lat)) +
  geom_boxplot(na.rm = TRUE) + 
  theme(axis.text.x = element_text(angle = 90))
