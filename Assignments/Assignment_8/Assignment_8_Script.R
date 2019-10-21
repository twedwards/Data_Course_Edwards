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



df1 <- add_predictions(data = shrooms, model = mod1)
df2 <- add_predictions(data = shrooms, model = mod4)
df3 <- add_predictions(data = shrooms, model = mod5)

ggplot(df1, aes(x=Nitrogen, y=GrowthRate)) +
  geom_point() + geom_smooth(method = "lm")

ggplot(shrooms, aes(x=as.factor(Light), y=GrowthRate)) +
  geom_boxplot() + facet_wrap(~Humidity)

ggplot(df2, aes(x=as.factor(Light), y=GrowthRate)) +
  geom_point() + facet_wrap(~Humidity) + geom_point(aes(y=pred), color = "Red")


ggplot(df3, aes(x=as.factor(Light), y=GrowthRate)) +
  geom_point() + facet_wrap(~Humidity) + geom_point(aes(y=pred), color = "Red") +
  labs(title = "Growth Rate as a Function of Light and Humidity", y="Growth Rate", x="Light"
       , caption = "Note: Red points are plotted predictions")


mean(abs(residuals(mod1)))
mean(abs(residuals(mod2)))
mean(abs(residuals(mod3)))
mean(abs(residuals(mod4)))
mean(abs(residuals(mod5)))






















