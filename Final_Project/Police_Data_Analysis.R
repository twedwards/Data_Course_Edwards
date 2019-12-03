library(tidyverse)
library(modelr)

clean <- read.csv("./Cleaned_Police_Data.csv")

#### We need to change the names of the AgencyName values to something that is able to be used in a Model ####
clean$AgencyName <- as.character(clean$AgencyName)
clean$AgencyName

policerows = grep("POLICE",clean$AgencyName)
forestrows = grep("FOREST",clean[policerows,"AgencyName"])
universityrows = grep("UNIVERSITY", clean$AgencyName)
collegerows = grep("COLLEGE", clean$AgencyName)
countyrows = grep("SHERIFF", clean$AgencyName)
otherrows = grep("METROPOLITAN|TERMINAL|TRI-COUNTY",clean$AgencyName)

clean$AgencyName[policerows] <- "Municipal"
clean$AgencyName[collegerows] <- "College"
clean$AgencyName[forestrows] <- "Forest"
clean$AgencyName[universityrows] <- "University"
clean$AgencyName[countyrows] <- "County"
clean$AgencyName[otherrows] <- "Other"









#### Now we can get into some analyses ####


mod1 <- glm(data=clean, VehicleSearchConducted ~ DriverRace + Age, family="binomial")
clean <- add_predictions(clean,mod1,type="response",var="ProbOfSearchmod1")
summary(mod1)

mod2 <- glm(data=clean, VehicleSearchConducted ~ DriverRace + AgencyName, family="binomial")
clean <- add_predictions(clean, mod2, type="response",var="ProbOfSearchmod2")
summary(mod2)

mod3 <- glm(data=clean, VehicleContrabandFound ~ DriverRace + Age,family="binomial")
clean <- add_predictions(data=clean,mod3, type="response", var="ProbOfSearchmod3")
summary(mod3)

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



























