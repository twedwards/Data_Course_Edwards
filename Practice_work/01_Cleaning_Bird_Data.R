library(carData)
library(tidyverse)


data("MplsDemo")
data("MplsStops")

names(MplsDemo)
names(MplsStops) ####because these data sets have a column in common, you can join the data sets into one set.

full <- full_join(MplsStops, MplsDemo, "neighborhood")
anti <- anti_join(MplsStops, MplsDemo, "neighborhood")

unique(anti$neighborhood)
unique(MplsDemo$neighborhood)

unique(anti$neighborhood) %in% unique(MplsDemo$neighborhood)

left <- left_join(MplsStops, MplsDemo, "neighborhood")
left2 <- left_join(MplsDemo, MplsStops, "neighborhood")



crappy <- read.csv("./Data/Bird_Measurements.csv")
names(crappy)


crappy2 <- crappy %>% 
  select(-ends_with("_N"))    ####this selects all columns except those ending with "_N"

names(crappy2)

#Create new subsets with each Gender

Male <- crappy2 %>%
  select(c("Family","Species_number","Species_name","English_name", 
           "Clutch_size", "Egg_mass", "Mating_System"), starts_with("M_"))

Female <- crappy2 %>%
  select(c("Family","Species_number","Species_name","English_name", 
           "Clutch_size", "Egg_mass", "Mating_System"), starts_with("F_"))

Unsexed <- crappy2 %>%
  select(c("Family","Species_number","Species_name","English_name", 
           "Clutch_size", "Egg_mass", "Mating_System"), contains("nsexed_"))

#Create new gender column and fill with appropriate values. This will allow to joins the above subsets to a single table again.

Male$Gender <- "Male"
Female$Gender <- "Female"
Unsexed$Gender <- "Unsexed"

#Rename the column names to remove the "M_", "F_", and "Unsexed_"

names(Male)
names(Female)
names(Unsexed)

names(Male) <- str_replace(names(Male), "M_", "")
names(Female) <- str_replace(names(Female), "F_", "")
names(Unsexed) <- str_replace(names(Unsexed), "unsexed", "Unsexed")
#names(Unsexed) <- str_replace(names(Unsexed), "Unsexed", "")
names(Unsexed) <- str_replace(names(Unsexed), "Unsexed_", "")

#join the newly subsetted datasets into one single data set

full <- full_join(Male,Female,"Species_number") #???????

full <- rbind(Male,Female,Unsexed)

write.csv(full,"./Data/01_Cleaned_bird_data.csv", quote = FALSE, row.names = FALSE)







