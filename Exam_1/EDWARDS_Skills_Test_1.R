library(ggplot2)
library(plyr)



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

summary(DNA$DNA_Concentration_Katy)
summary(DNA$DNA_Concentration_Ben)

Katy_mean=c()
x=1
for(i in unique(DNA$Year_Collected)){
  Katy_mean[x] = mean(DNA[DNA$Year_Collected == i, "DNA_Concentration_Katy"])
  x=x+1
}

Ben_mean=c()
x1=1
for(i in unique(DNA$Year_Collected)){
  Ben_mean[x1] = mean(DNA[DNA$Year_Collected == i, "DNA_Concentration_Ben"])
  x1=x1+1
}

as.factor(Ben_mean)



plot(Ben_mean, col="red", yaxt='n', ann=FALSE) 
par(new=TRUE)
plot(Katy_mean, col="green", yaxt='n', ylab="Ben and Katy's Data collections", xlab="Year",sub="2000-2012")


downstairs <- DNA[DNA$Lab == "Downstairs",]

ggplot(downstairs, aes(x=as.POSIXct(downstairs$Date_Collected), y=downstairs$DNA_Concentration_Ben)) +
  geom_point() +
  labs(x="Date Collected", y="Ben's Extractions", title = "Ben's DNA Extractions by Year") +
  theme(plot.title = element_text(hjust = .5))
ggsave("Ben_DNA_Over_Time.jpg")


  




