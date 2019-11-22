# load data and packages ####

library(readr)
library(tidyverse)
df = readr::read_delim("./Illinois_2017_Dataset/Illinois_Subset.txt",delim = "~",col_names = TRUE,na="0",
                       col_types = cols(
                         AgencyName = col_character(),
                         AgencyCode = col_double(),
                         DateOfStop = col_date(),
                         TimeOfStop = col_time(),
                         DurationOfStop = col_double(),
                         ZIP = col_double(),
                         VehicleMake = col_character(),
                         VehicleYear = col_double(),
                         DriversYearofBirth = col_double(),
                         DriverSex = col_factor(),
                         DriverRace = col_factor(),
                         ReasonForStop = col_factor(),
                         TypeOfMovingViolation = col_factor(),
                         ResultOfStop = col_factor(),
                         BeatLocationOfStop = col_factor(),
                         VehicleConsentSearchRequested = col_factor(),
                         VehicleConsentGiven = col_factor(),
                         VehicleSearchConducted = col_factor(),
                         VehicleSearchConductedBy = col_factor(),
                         VehicleContrabandFound = col_factor(),
                         VehicleDrugsFound = col_factor(),
                         VehicleDrugParaphernaliaFound = col_factor(),
                         VehicleAlcoholFound = col_factor(),
                         VehicleWeaponFound = col_factor(),
                         VehicleStolenPropertyFound = col_factor(),
                         VehicleOtherContrabandFound = col_factor(),
                         VehicleDrugAmount = col_factor(),
                         DriverConsentSearchRequested = col_factor(),
                         DriverConsentGiven = col_factor(),
                         DriverSearchConducted = col_factor(),
                         DriverSearchConductedBy = col_factor(),
                         PassengerConsentSearchRequested = col_factor(),
                         PassengerConsentGiven = col_factor(),
                         PassengerSearchConducted = col_factor(),
                         PassengerSearchConductedBy = col_factor(),
                         DriverPassengerContrabandFound = col_factor(),
                         DriverPassengerDrugsFound = col_factor(),
                         DriverPassengerDrugParaphernaliaFound = col_factor(),
                         DriverPassengerAlcoholFound = col_factor(),
                         DriverPassengerWeaponFound = col_factor(),
                         DriverPassengerStolenPropertyFound = col_factor(),
                         DriverPassengerOtherContrabandFound = col_factor(),
                         DriverPassengerDrugAmount = col_factor(),
                         PoliceDogPerformSniffOfVehicle = col_factor(),
                         PoliceDogAlertIfSniffed = col_factor(),
                         PoliceDogVehicleSearched = col_factor(),
                         PoliceDogContrabandFound = col_factor(),
                         PoliceDogDrugsFound = col_factor(),
                         PoliceDogDrugParaphernaliaFound = col_factor(),
                         PoliceDogAlcoholFound = col_factor(),
                         PoliceDogWeaponFound = col_factor(),
                         PoliceDogStolenPropertyFound = col_factor(),
                         PoliceDogOtherContrabandFound = col_factor(),
                         PoliceDogDrugAmount = col_factor()))




levels(df$DriverSex)

# change driver sex to M/F ####
df$DriverSex <-  plyr::mapvalues(df$DriverSex,from = c(2,1), to = c("Female","Male"))

levels(df$DriverRace) 

df$DriverRace <- plyr::mapvalues(x=df$DriverRace, from = levels(df$DriverRace),
                to = c("White", "Black", "Native", "Hispanic", "Asian", "Polynesian", "White",
                       "Black", "Native", "Hispanic", "Asian", "Polynesian"))


levels(df$ReasonForStop)

df$ReasonForStop <- plyr::mapvalues(x=df$ReasonForStop, from = levels(df$ReasonForStop),
                to = c("MovingViolation", "Equipment", "LicencePlate/Registration", "CommercialVehicle","MovingViolation", 
                       "Equipment", "LicencePlate/Registration", "CommercialVehicle"))

levels(df$TypeOfMovingViolation) 

df$TypeOfMovingViolation <- plyr::mapvalues(x=df$TypeOfMovingViolation, from = c("1","2","3","4","5","6","1.00","2.00","3.00","4.00","5.00","6.00"), 
                to = c("Speed", "LaneViolation", "SeatBelt","TrafficSign/Signal", "FollowTooClose", "Other"
                       ,"Speed", "LaneViolation", "SeatBelt","TrafficSign/Signal", "FollowTooClose", "Other"))





df$ResultOfStop <- plyr::mapvalues(x=df$ResultOfStop, from = levels(df$ResultOfStop),
                                    to = c("Citation", "WrittenWarning", "VerbalWarning",
                                           "Citation", "WrittenWarning", "VerbalWarning"))

# change logical columns ####
logical_cols <- c("VehicleConsentSearchRequested","VehicleConsentGiven","VehicleSearchConducted",
                  "VehicleContrabandFound","VehicleDrugsFound",
                  "VehicleDrugParaphernaliaFound","VehicleAlcoholFound","VehicleWeaponFound",
                  "VehicleStolenPropertyFound","VehicleOtherContrabandFound","DriverConsentSearchRequested","DriverConsentGiven","DriverSearchConducted",
                  "PassengerConsentSearchRequested","PassengerConsentGiven","PassengerSearchConducted","DriverPassengerContrabandFound",
                  "DriverPassengerDrugsFound","DriverPassengerDrugParaphernaliaFound","DriverPassengerAlcoholFound",
                  "DriverPassengerWeaponFound","DriverPassengerStolenPropertyFound","DriverPassengerOtherContrabandFound",
                  "PoliceDogPerformSniffOfVehicle","PoliceDogAlertIfSniffed",
                  "PoliceDogVehicleSearched","PoliceDogContrabandFound","PoliceDogDrugsFound",
                  "PoliceDogDrugParaphernaliaFound","PoliceDogAlcoholFound","PoliceDogWeaponFound",
                  "PoliceDogStolenPropertyFound","PoliceDogOtherContrabandFound")


# make list of all the levels found in those columns
z=list()
y=1
for(i in logical_cols){
  x=unique(df[,i])
  z[y] <- x
  y=y+1
}


names(z) <- logical_cols

#the following 2 lines were used to determine what the class of the logical_cols were,
#this allowed us to know to change the "levels portion of the above forloop to "unique"
#df3 = df1[,logical_cols]
#apply(df3,2,class)

z

# get list of all possible values in what should be: 0=NA, 1=TRUE, 2=FALSE
unique(unlist(z))

#df1 needs to be a data frame
df <- as.data.frame(df)

#before we can change the values, we need the logical columns in character form

for(i in logical_cols){
  df[,i] = as.character(df[,i])
}

# change 2,1,0 to T,F,NA in all logical columns

for(column in logical_cols){
  df[,column] <- as.logical(plyr::mapvalues(x = df[,column],
                                                    from = c("2","1","2.00","1.00","0.00"),
                                                    to=c(FALSE,TRUE,FALSE,TRUE,NA)))
}


# add age column for analysis help ####

df <- df %>% 
  mutate(Age = 2017 - DriversYearofBirth)


# write the data tables to csv files to begin analysis ####

write.csv(df, "../Final_Project/Cleaned_Police_Data.csv")











