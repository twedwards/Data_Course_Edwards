library(modelr)
library(broom)
library(dplyr)
library(tidyr)
library(ggplot2)



shrooms <- read.csv("../../../Data_Course/Data/mushroom_growth.csv")

str(shrooms)

ggplot(shrooms, aes(x=Nitrogen, y=GrowthRate)) +
  geom_point()

ggplot(shrooms, aes(x=as.factor(Light), y=GrowthRate)) +
  geom_boxplot()

ggplot(shrooms, aes(x=Humidity, y=GrowthRate)) +
  geom_boxplot()

mse <- function(mod){mean(residuals(mod)^2)}

mod1 <- aov(data=shrooms, GrowthRate ~ Nitrogen + Light)
summary(mod1)
mse(mod1)


mod2 <- aov(data=shrooms, GrowthRate ~ Temperature + Nitrogen)
summary(mod2)
mse(mod2)

mod3 <- lm(data=shrooms, GrowthRate ~ Temperature + Nitrogen + Light)
summary(mod3)
mse(mod3)

mod4 <- lm(data = shrooms, GrowthRate ~ Light * Humidity)
summary(mod4)
mse(mod4)

mod5 <- lm(data = shrooms, GrowthRate ~ Light + Humidity)
summary(mod5)
mse(mod5)

mse(mod1) #7697.293
mse(mod2) #9631.201
mse(mod3) #7605.163
mse(mod4) #5525.948
mse(mod5) #5828.882

## mod4 is the best model



df1 <- add_predictions(data = shrooms, model = mod3)
df2 <- add_predictions(data = shrooms, model = mod4)
df3 <- add_predictions(data = shrooms, model = mod5)

ggplot(shrooms, aes(x=Nitrogen, y=GrowthRate)) +
  geom_point() + geom_point(aes(y = df1$pred), color = "Red", alpha = .25) + geom_smooth(method = "lm")

ggplot(shrooms, aes(x=as.factor(Light), y=GrowthRate)) +
  geom_point() + geom_point(aes(y=df2$pred), color = "Red", alpha = .25) + facet_wrap(~Humidity) +
  geom_smooth(method = "lm")

ggplot(shrooms, aes(x=as.factor(Light), y=GrowthRate)) +
  geom_point() + geom_point(aes(y=df3$pred), color = "Red", alpha = .25)+
  geom_smooth(method = "lm")


ggplot(shrooms, aes(x=as.factor(Light), y=GrowthRate)) +
  geom_point() + facet_wrap(~Humidity) + geom_point(aes(y=df2$pred), color = "Red") +
  labs(title = "Growth Rate as a Function of Light and Humidity", y="Growth Rate", x="Light"
       , caption = "Note: Red points are plotted predictions")


#1. Mod 2 and 3 were both scientifically insignificant. They are insignificant because they're p-value was far above .05
#2. All were linear relationships























