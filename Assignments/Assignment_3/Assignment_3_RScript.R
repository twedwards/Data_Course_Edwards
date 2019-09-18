dat <- read.csv("../../Data_Course/Data/thatch_ant.csv")

library(ggplot2)

ggplot(dat, aes(x=dat$Headwidth, y=dat$Mass, color=Colony)) +
  geom_point(alpha=.35) +
  theme(axis.text.x = element_text(angle = 90)) + 
  labs(title="Ant Headwidth Compared to Mass", x = "Headwidth", y = "Mass")
ggsave("../Assignments/Assignment_3/Headwidth_Mass_plot.jpg")

col1_2 <- dat[dat$Colony <= 2,]
write.csv(col1_2, file = "../../Data_Course_Edwards/Assignments/Assignment_3/Colony1_2.csv")
