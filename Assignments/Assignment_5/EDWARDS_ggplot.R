library(ggplot2)
data("iris")

ggplot(iris, aes(x=Sepal.Length, y=Petal.Length, col=Species)) +
  geom_point() + geom_smooth(method = "lm") +
  labs(title = "Sepal length vs Petal length", subtitle = "for three iris species")
ggsave("../Assignment_5/iris_fig1.png")

ggplot(iris, aes(x=Petal.Width, fill=Species)) +
  geom_density() + 
  labs(title = "Distribution of Petal Widths", subtitle = "for three iris species", x = "Petal Width")
ggsave("./iris_fig2.png")


iris$Ratio <- with(iris, Petal.Width / Sepal.Width)

ggplot(iris, aes(x=Species, y=Ratio, fill=Species)) +
  geom_boxplot() +
  labs(title = "Sepal- to Petal-Width Ratio", x="Species", y="Ratio of Sepal Width to Petal Width", subtitle = "for three iris species")

ggsave("./iris_fig3.png")

meansepal=mean(iris$Sepal.Length)

dev = iris$Sepal.Length - mean(iris$Sepal.Length)

iris$dev = dev

iris$id <- factor(1:nrow(iris), levels = 1:nrow(iris))

ggplot(iris, aes(x=reorder(id, dev), y=dev,fill=Species)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  theme(axis.text.y.left = element_blank(), axis.title.y = element_blank(), axis.ticks = element_blank()) +
  labs(title = "Sepal Length Deviance from the Mean of all Observations", 
       caption = "Note: Deviance = Sepal.Length - mean(Sepal.Length)", y = "Deviance from the Mean")
ggsave("../Assignment_5/iris_fig4.png")

















