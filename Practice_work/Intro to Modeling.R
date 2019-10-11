library(tidyverse)

fs <- readRDS("./Data/Messy_Take2/cleaned_data.RDS")

# intro to modeling

names(fs)
ggplot(fs, aes(x=DaysAlive, y=IQ, color=Gender)) +
  geom_point() + geom_smooth(method = "lm")


data(iris)

ggplot(iris, aes(x=Petal.Length, y=Sepal.Length)) + 
  geom_point() + geom_smooth(method = "lm")



mod1 <- lm(data=iris, Sepal.Length ~ Petal.Length)
mod1

summary(mod1)
residuals(mod1)
sum(abs(residuals(mod1)))
mean(residuals(mod1)^2)

ggplot(iris, aes(x=Petal.Length, y=Sepal.Length, color = Species)) +
  geom_point() + geom_smooth(method = "lm")



mod2 <- lm(data = iris, Sepal.Length ~ Petal.Length * Species)
mod2
summary(mod2)


mod3 <- lm(data = iris, Sepal.Length ~ Petal.Length + Species)
mod3
summary(mod3)


mean(abs(residuals(mod1)))
mean(abs(residuals(mod2)))
mean(abs(residuals(mod3)))













