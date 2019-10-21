library(dplyr)
library(ggplot2)
library(modelr)

#1.  Read in salaries.csv (needs some tidying)
#This is faculty salary information from 1995 - Split up by university, state, faculty rank, and university tier
#Faculty "Rank" progresses from "AssistProf" to "AssocProf" to "FullProf"

salaries <- read.csv("../Exam_2/Exam_2_files/salaries.csv")
salaries

#2.  Convert to a usable tidy format so we can look at "Salary" as a dependent variable (10 points)

assist <- salaries %>% 
  select(FedID, UnivName, State, Tier, AssistProf)
assoc <- salaries %>%
  select(FedID, UnivName, State, Tier, AssocProf)
full <- salaries %>%
  select(FedID, UnivName, State, Tier, FullProf)

assist <- assist %>%
  mutate(Status = "AssistProf")

assoc <- assoc %>%
  mutate(Status = "AssocProf")

full <- full %>%
  mutate(Status = "FullProf")

names(full)[names(full) == "FullProf"] <- "Salary"
names(assoc)[names(assoc) == "AssocProf"] <- "Salary"
names(assist)[names(assist) == "AssistProf"] <- "Salary"

clean_salaries <- rbind(assist, assoc, full)

names(clean_salaries)[names(clean_salaries) == "Status"] <- "Rank"

#3.  Using ggplot, create boxplot of salary (y-axis) by University Tier (x-axis), filled by Faculty Rank (10 points)
#x-axis = Tier
#y-axis = Salary
#Boxplot fill color = Rank
#Title = "Faculty Salaries - 1995"

ggplot(clean_salaries, aes(x=Tier, y=Salary, fill=Rank)) +
  geom_boxplot() + 
  labs(title = "Faculty Salaries - 1995")

#4.  Export this delightful boxplot to a file named "LASTNAME_exam2_plot1.jpeg" (10 points)

ggsave("../Exam_2/EDWARDS_exam2_plot1.jpeg")

#1.  Read in atmosphere.csv (pretty clean data set)
#These are observations of fungal diversity (number of different species) found in air samples along a time series
#SampleID - The unique sample ID for the observation (dd-mm-YYYY)
#Year - What do you think?
  #Quarter - Q1 = Jan/Feb/Mar, Q2 = Apr/May/Jun, etc
#Month - This stands for "Magpie ovulation number..." no, it's just Month
#        Mday - Day of the month
#        BarcodeSequence - Not important
#        Aerosol_Density - Number of detectable particles in the air sample per cubic cm
#        CO2_Concentration - CO2 ppm on the day the sample was taken
#        Diversity - Number of different fungal species found in the air sample
#        Precip - Precipitation on the sampling day (mm)

atmosphere <- read.csv("../Exam_2/Exam_2_files/atmosphere.csv")

#2.  Create three different linear models with Diversity as the dependent variable. The three models should have different
#predictors, or at least different numbers of predictors, with or without interaction terms. (10 points)

mod1 <- lm(data = atmosphere, Diversity ~ CO2_Concentration)
mod2 <- lm(data = atmosphere, Diversity ~ Precip)
mod3 <- lm(data = atmosphere, Diversity ~ Aerosol_Density)

summary(mod1)
summary(mod2)
summary(mod3)

#3.  Compare the residuals of the three models and somehow document which has best explanatory power for the data (10 points)

mean(abs(residuals(mod1))) #376.3713
mean(abs(residuals(mod2))) #150.6311
mean(abs(residuals(mod3))) #409.3237

#### mod2 has the lowest residual mean, and therefore has the best explanatory power.


#4.  Use all your models to predict Diversity values in the data set (10 points)


atmosphere <- atmosphere %>% gather_predictions(mod1,mod2,mod3)

#5.  Make a plot showing actual Diversity values, along with the three models' predicted Diversity values.
#    Use color or some other aesthetic to differentiate the actual values and all three predictions (20 points)
#	Hint: gather_predictions()   ...wait, what is this magical function!? Maybe this hint should be for #4 ???

ggplot(atmosphere, aes(x=Precip, y=Diversity)) +
  geom_point() + 
  geom_point(aes(y=atmosphere$pred), color = "Red", alpha = .40) +
  facet_wrap(~model) +
  labs(title = "Precipitation's Effects on Diversity", subtitle = "including predicted values from 3 models", x="Precipitation")
ggsave("./ActualDiv_vs_PredictedDiv.jpg")

#6.  Write code to show the predicted values of Diversity for each model using the hypothetical new data 
#found in hyp_data.csv (10 points)

hyp_data <- read.csv("../Exam_2/Exam_2_files/hyp_data.csv")

hyp_pred1 <- hyp_data %>% add_predictions(mod1)
hyp_pred1 <- hyp_pred1 %>% mutate(model = "mod1")

hyp_pred2 <- hyp_data %>% add_predictions(mod2)
hyp_pred2 <- hyp_pred2 %>% mutate(model = "mod2")

hyp_pred3 <- hyp_data %>% add_predictions(mod3)
hyp_pred3 <- hyp_pred3 %>% mutate(model = "mod3")

full_hyp <- rbind(hyp_pred1, hyp_pred2, hyp_pred3)

#7.  Export a text file that contains the summary output from *both* your models to "model_summaries.txt" (10 points)
#(Hint: use the sink() function)

sink("./model_summaries.txt")
summary(mod1)
summary(mod2)
summary(mod3)
sink()


#BONUS

#8.  Add these hypothetical predicted values (from hypothetical data - Part II, Step 6) to a plot of actual data 
#and differentiate them by color. (10 bonus points possible for a pretty graph)

ggplot(hyp_data, aes(x=Precip, y=)) +
  geom_point() + geom_point(aes(y=hyp_data$pred), color="Red", alpha = .40) +
  facet_wrap(~model)

#9.  Split the atmosphere.csv data into training and testing sets, randomly. Train your single best model on 50% of the data and 
#test it on the remaining 50% of the data. Find some way to show how well it fits the data.
#This is the only cross-validation part of the exam. (10 bonus points for proper code)

library(caret)

set.seed(123)
trainingsamples <- createDataPartition(atmosphere$Diversity, p=0.5,list=FALSE)  

train <- atmosphere[trainingsamples,]
test <- atmosphere[-trainingsamples,]











  










