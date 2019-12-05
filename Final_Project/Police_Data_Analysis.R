library(tidyverse)
library(modelr)

clean <- read.csv("./Cleaned_Police_Data.csv")

# analyses ####


mod1 <- glm(data=clean, VehicleSearchConducted ~ DriverRace + Age, family="binomial")
clean <- add_predictions(clean,mod1,type="response",var="ProbOfSearchmod1")
summary(mod1)

mod2 <- glm(data=clean, VehicleSearchConducted ~ DriverRace + AgencyName, family="binomial")
clean <- add_predictions(clean, mod2, type="response",var="ProbOfSearchmod2")
summary(mod2)

mod3 <- glm(data=clean, VehicleContrabandFound ~ DriverRace + Age,family="binomial")
clean <- add_predictions(data=clean,mod3, type="response", var="ProbOfContramod3")
summary(mod3)

mod4 <- glm(data = clean, VehicleContrabandFound ~ DriverRace + Age + AgencyName, family="binomial")
clean <- add_predictions(data = clean,mod4,type="response", var="ProbOfContramod4")
summary(mod4)

ggplot(clean, aes(x=ProbOfSearchmod1)) +
  geom_histogram(aes(fill=clean$DriverRace))



plot1 <- ggplot(clean, aes(x=Age, y=ProbOfSearchmod1)) +
  geom_point() + facet_wrap(~DriverRace)+
  labs(title = "Vehicle Search Probablity by Race and Age", y="Probability of Vehicle Search")

plot2 <- ggplot(clean, aes(x=Age, y=ProbOfSearchmod2)) +
  geom_point() + facet_wrap(~DriverRace)+
  labs(title = "Vehicle Search Probability by Race, Age, and Agency", y="Probability of Vehicle Search")

plot1
plot2

plot3 <- ggplot(clean, aes(x=Age, y=ProbOfContramod3))+
  geom_point() + facet_wrap(~DriverRace) + 
  labs(title = "Contraband Found by Race and Age", y = "Probability of Contraband Found")

plot4 <- ggplot(clean, aes(x=Age, y=ProbOfContramod4, color=AgencyName))+
  geom_point() + facet_wrap(~DriverRace) + 
  labs(title = "Contraband Found by Race, Age, and Agency", y = "Probability of Contraband Found") 

plot3
plot4





























