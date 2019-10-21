library(dplyr)
library(ggplot2)
library(modelr)

salaries <- read.csv("../Exam_2/Exam_2_files/salaries.csv")
salaries

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

ggplot(clean_salaries, aes(x=Tier, y=Salary, fill=Rank)) +
  geom_boxplot() + 
  labs(title = "Faculty Salaries - 1995")
ggsave("../Exam_2/EDWARDS_exam2_plot1.jpeg")



atmosphere <- read.csv("../Exam_2/Exam_2_files/atmosphere.csv")

mod1 <- lm(data = atmosphere, Diversity ~ CO2_Concentration)
mod2 <- lm(data = atmosphere, Diversity ~ Precip)
mod3 <- lm(data = atmosphere, Diversity ~ Aerosol_Density)

summary(mod1)
summary(mod2)
summary(mod3)

mean(abs(residuals(mod1))) #376.3713
mean(abs(residuals(mod2))) #150.6311
mean(abs(residuals(mod3))) #409.3237

#### mod2 has the lowest residual mean, and therefore has the best explanatory power.

atmosphere <- atmosphere %>% gather_predictions(mod1,mod2,mod3)

ggplot(atmosphere, aes(x=Precip, y=Diversity)) +
  geom_point() + 
  geom_point(aes(y=atmosphere$pred), color = "Red", alpha = .40) +
  facet_wrap(~model) +
  labs(title = "Precipitation's Effects on Diversity", subtitle = "including predicted values from 3 models", x="Precipitation")
ggsave("./ActualDiv_vs_PredictedDiv.jpg")



hyp_data <- read.csv("../Exam_2/Exam_2_files/hyp_data.csv")

hyp_data <- hyp_data %>% gather_predictions(mod1,mod2,mod3)

sink("./model_summaries.txt")
summary(mod1)
summary(mod2)
summary(mod3)
sink()


#BONUS

ggplot(hyp_data, aes(x=Precip, y=)) +
  geom_point() + geom_point(aes(y=hyp_data$pred), color="Red", alpha = .40) +
  facet_wrap(~model)

library(caret)

set.seed(123)
trainingsamples <- createDataPartition(atmosphere$Diversity, p=0.5,list=FALSE)  

train <- atmosphere[trainingsamples,]
test <- atmosphere[-trainingsamples,]











  










