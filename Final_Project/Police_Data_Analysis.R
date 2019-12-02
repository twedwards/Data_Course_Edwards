library(tidyverse)
library(modelr)



clean <- read.csv("./Cleaned_Police_Data.csv")

df1 <- clean[1:5000,]

mod1 <- glm(data=df1, VehicleSearchConducted ~ DriverRace + Age, family="binomial")
df1 <- add_predictions(df1,mod1,type="response",var="ProbOfSearchmod1")

mod2 <- aov(data=df1, VehicleSearchConducted ~ DriverRace + AgencyName)
df1 <- add_predictions(df1, mod2, type="response",var="ProbOfSearchmod2")

mod3 <- aov(data=clean, VehicleSearchConducted ~ DriverRace + AgencyName)

summary(mod1)
summary(mod2)


ggplot(clean, aes(x=ProbOfSearchmod1)) +
  geom_histogram(aes(fill=clean$DriverRace))



mod3 <- glm(data = clean, VehicleSearchConducted ~ AgencyName)
clean <- add_predictions(data = clean,mod3, type="response",var = "SearchByAgency")



ggplot(clean, aes(x=ProbOfContraFound, y=Age, color=DriverRace)) +
  geom_point()
summary(mod1)
summary(mod2)
summary(mod3)



























