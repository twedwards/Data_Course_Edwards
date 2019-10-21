
library(ggplot2)

df = read.delim("../../../Data_Course/Data/ITS_mapping.csv",header = TRUE,sep = "\t")

summary(df)

str(df)

df$BarcodeSequence <- as.numeric(df$BarcodeSequence)
df$LinkerPrimerSequence <- as.numeric(df$LinkerPrimerSequence)
df$Host_Type <- as.numeric(df$Host_Type)

df[is.na(df$BarcodeSequence),]

df[df == "NA"] <- NA







ggplot(df, aes(x=Ecosystem, y=Lat)) +
  geom_boxplot(na.rm = TRUE) + 
  theme(axis.text.x = element_text(angle = 90))
ggsave("../Assignment_4/silly_boxplot.png")



