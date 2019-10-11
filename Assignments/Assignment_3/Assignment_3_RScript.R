dat <- read.csv("../../Data_Course/Data/thatch_ant.csv")

library(ggplot2)

Headwidth_Numeric <- as.numeric(dat$Headwidth)

colonyfactor=factor(dat$Colony,levels = c(3,4,5,6,7,8,9,10,11,1,2))
ggplot(dat, aes(x=dat$Headwidth..mm., y=dat$Mass, color=colonyfactor)) +
  geom_point(alpha=.35) +
  theme(axis.text.x = element_text(angle = 90)) + 
  labs(title="Ant Headwidth Compared to Mass", x = "Headwidth", y = "Mass")
ggsave("../Assignments/Assignment_3/Headwidth_Mass_plot.jpg")

col1_2 <- dat[dat$Colony %in% c(1,2),]
write.csv(col1_2, file = "../../Data_Course_Edwards/Assignments/Assignment_3/Colony1_2.csv",quote = FALSE,row.names = FALSE)

## quote = FALSE will take anything that is in quotations and remove the quotations, and row.names = FALSE will remove the numbers 
## that label the rows (try running the above without both of them to see what I mean)


colonyfactor=factor(dat$Colony,levels = c(3,4,5,6,7,8,9,10,11,1,2)) ####the levels=c() tells the order of levels for the legend (see above)
order(unique(dat$Colony))

dat$Mass
which(dat$Mass >50)


data("iris")
iris$Species

mean(iris[iris$Species == "versicolor", "Sepal.Length"])
mean(iris[iris$Species == "virginica", "Sepal.Length"])
mean(iris[iris$Species == "setosa", "Sepal.Length"])

## The above is a way to get the mean of a specified subset.

vec=c()
x=1
for(i in unique(iris$Species)){
  vec[x] = mean(iris[iris$Species == i, "Sepal.Length"])
  x=x+1
}

## The above also gets the mean, but through a for loop method instead of repetitive subsetting.










