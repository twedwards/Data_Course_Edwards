plot(x = dat$Mass, y = dat$Headwidth, main = "Mass to Head Width Ratio of Thatch Ants", xlab = "Mass", ylab = "Head Width", pch = 20, col = dat$Colony)


?file.copy()

##file.copy(from, to, recursive = TRUE) 
## recursive = TRUE will copy the directory in the from position and its contents to the directory in the to position.


library(dplyr)
library(ggplot2)
library(readxl)
library(tidyr)

?dplyr

iris.summary <- iris %>%
  group_by(Species) %>%
  summarise(misep.length = min(Sepal.Length),
            meansep.length = mean(Sepal.Length),
            maxsep.length = max(Sepal.Length),
            misep.width = min(Sepal.Width),
            meansep.width = mean(Sepal.Width),
            maxsep.width = max(Sepal.Width),
            minpet.width = min(Petal.Width),
            meanpet.width = mean(Petal.Width),
            maxpet.width = max(Petal.Width),
            minpet.length = min(Petal.Length),
            meanpet.length = mean(Petal.Length),
            maxpet.length = max(Petal.Length))


iris %>% 
  arrange(desc(Sepal.Length)) %>%
  group_by(Species)

iris.rearranged <- iris %>% 
  arrange(desc(Sepal.Length)) 

iris.greater.seven <- iris %>%
  arrange(desc(Sepal.Length)) %>%
  filter(Sepal.Length >= 7)

iris %>%
  arrange(desc(Sepal.Length)) %>%
  filter(Sepal.Length >= 7) %>%
  select(Species) %>%
  unique()

iris %>% 
  group_by(Species) %>%
  mutate(TOTALLENGTH = Sepal.Length + Petal.Length)

dat <- read.csv("../../Data_Course/Data/Utah_Religions_by_County.csv")
dat2 <- read_xlsx("../../Data_Course/Data/wide_data_example.xlsx")
  
datlong <- gather(dat2, key = Treatment, value = Mass, 2:3)


dat2$`Treatment 1`[1] <- "NA"
dat2$`Treatment 1` <- as.numeric(dat2$`Treatment 1`)

ggplot(dat2, aes(x=SampleID)) +
  geom_bar(stat = "identity") ####stat = "identity" makes sure that ggplot bar graph uses the actual values in the dataframe to create the
                              #height of the bar, instead of just creating a histogram like it innately wants to.

ggplot(dat2, aes(x=SampleID)) +
  geom_bar(stat = "identity", aes(y=`Treatment 1`), fill = "Blue") +
  geom_bar(stat = "identity", aes(y=`Treatment 2`),fill = "Red") 

datlong$Mass[1] <- "NA"

datlong %>%
  ggplot(aes(x=SampleID, y=as.numeric(Mass), fill=Treatment)) +
  geom_bar(stat="Identity")

religion.long <- dat %>%
  gather(key = Religion, value = Proportion, -c("County", "Pop_2010", "Religious"))

religion.long %>%
  ggplot(aes(x=County, y=Proportion, fill = Religion)) +
  geom_bar(stat = "Identity") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

religion.long %>%
  group_by(Religion) %>%
  summarise(MeanProportion = mean(Proportion)) %>%
  arrange(desc(MeanProportion)) %>%
  select(MeanProportion) %>% sum()
  
  
  
  
  
  
  




  












  