library(ggplot2)


DNA = read.csv("DNA_Conc_by_Extraction_Date.csv")

ggplot(DNA, aes(x=DNA$DNA_Concentration_Katy)) +
  geom_histogram() +
  labs(x="DNA Concentrations", title = "Katy's DNA Concentrations")


ggplot(DNA, aes(x=DNA$DNA_Concentration_Ben)) +
  geom_histogram() +
  labs(x="DNA Concentrations", title = "Ben's DNA Concentrations")
  
ggplot(DNA, aes(x=as.factor(DNA$Year_Collected), y=DNA$DNA_Concentration_Katy)) +
  geom_boxplot(outlier.alpha = .6) +
  labs(x="YEAR", y="DNA Concentration", title= "Katy's Extractions")+
  theme(plot.title = element_text(hjust = .5))
ggsave("EDWARDS_Plot1.jpg")

ggplot(DNA, aes(x=as.factor(DNA$Year_Collected), y=DNA$DNA_Concentration_Ben)) +
  geom_boxplot(outlier.alpha = .6) +
  labs(x="YEAR", y="DNA Concentration", title= "Ben's Extractions")+
  theme(plot.title = element_text(hjust = .5))
ggsave("EDWARDS_Plot2.jpg")





