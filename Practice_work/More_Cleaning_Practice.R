library(stringr)
library(tidyverse)
library(ggplot2)


file1 <- read.csv("./Data/Messy_Take2/a_df.csv")

file2 <- read.csv("./Data/Messy_Take2/b_df.csv")

file3 <- read.csv("./Data/Messy_Take2/c_df.csv")

file4 <- read.csv("./Data/Messy_Take2/d_df.csv")

file5 <- read.csv("./Data/Messy_Take2/e_df.csv")

file6 <- read.csv("./Data/Messy_Take2/f_df.csv")

file7 <- read.csv("./Data/Messy_Take2/g_df.csv")

names(file1)[names(file1) == "Pass_Female"] <- "Pass_Male"
names(file1)[names(file1) == "Pass_Female.1"] <- "Pass_Female"
names(file1)

names(file2)[names(file2) == "Pass_Female"] <- "Pass_Male"
names(file2)[names(file2) == "Pass_Female.1"] <- "Pass_Female"

names(file3)[names(file3) == "Pass_Female"] <- "Pass_Male"
names(file3)[names(file3) == "Pass_Female.1"] <- "Pass_Female"

names(file4)[names(file4) == "Pass_Female"] <- "Pass_Male"
names(file4)[names(file4) == "Pass_Female.1"] <- "Pass_Female"

names(file5)[names(file5) == "Pass_Female"] <- "Pass_Male"
names(file5)[names(file5) == "Pass_Female.1"] <- "Pass_Female"

names(file6)[names(file6) == "Pass_Female"] <- "Pass_Male"
names(file6)[names(file6) == "Pass_Female.1"] <- "Pass_Female"

names(file7)[names(file7) == "Pass_Female"] <- "Pass_Male"
names(file7)[names(file7) == "Pass_Female.1"] <- "Pass_Female"

##make new columns for gender, date of birth, IQ, and days alive

full <- rbind(file1,file2,file3,file4,file5,file6,file7)

male <- full %>%
  select(ends_with("_Male"))

female <- full %>%
  select(ends_with("_Female"))

female <- female %>% mutate(Gender = "Female")
male <- male %>% mutate(Gender = "Male")

#rename columns

names(male)
names(male) <- str_replace(names(male), "_Male", "")
names(female) <- str_replace(names(female), "_Female", "")

cleaner <- rbind(male,female)


##save your work!!!
write.csv(cleaner, "./Data/Messy_Take2/cleaned_data.csv", row.names = FALSE, quote = FALSE)

saveRDS(object = cleaner, file = "./Data/Messy_Take2/cleaned_data.RDS")






