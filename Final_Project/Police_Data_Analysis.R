library(tidyverse)
library(modelr)

clean <- read.csv("./Cleaned_Police_Data.csv")

mod1 <- glm(data=clean, VehicleSearchConducted ~ DriverRace + Age, family="binomial")
clean <- add_predictions(clean,mod1,type="response",var="ProbOfSearchmod1")

mod2 <- glm(data=clean, VehicleSearchConducted ~ DriverRace + AgencyCode, family="binomial")
clean <- add_predictions(clean, mod2, type="response",var="ProbOfSearchmod2")

mod3 <- lm(data=clean, VehicleContrabandFound ~ DriverRace)
clean <- add_predictions(data=clean,mod3, type="response", var="ProbOfSearchmod3")

ggplot(clean, aes(x=ProbOfSearchmod1)) +
  geom_histogram(aes(fill=clean$DriverRace))



plot1 <- ggplot(clean, aes(x=Age, y=ProbOfSearchmod1)) +
  geom_point() + facet_wrap(~DriverRace)

plot2 <- ggplot(clean, aes(x=Age, y=ProbOfSearchmod2)) +
  geom_point() + facet_wrap(~DriverRace)

plot2

ggplot(clean, aes(x=Age, y=ProbOfSearchmod3))+
  geom_point() + facet_wrap(~DriverRace)
unique(clean$VehicleContrabandFound)



summary(mod1)
summary(mod2)
summary(mod3)



























