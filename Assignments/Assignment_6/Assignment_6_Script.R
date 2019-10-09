data("mtcars")
str(mtcars)

auto_only <- mtcars[mtcars$am == "0",]
write.csv(auto_only, "../Assignment_6/automatic_mtcars.csv")

library(ggplot2)

ggplot(auto_only, aes(x=hp, y=mpg)) + 
  geom_point() + geom_smooth(method = "lm") +
  labs(title = "Effects of Horsepower on MPG", x = "Horsepower", y = "MPG", subtitle = "in automatic transmission cars")
ggsave("../Assignment_6/mpg_vs_hp_auto.tiff")

ggplot(auto_only, aes(x=wt, y=mpg)) + 
  geom_point() + geom_smooth(method = "lm") +
  labs(title = "Effects of Weight on MPG", x = "Weight (thousand lbs)", y = "MPG", subtitle = "in automatic transmission cars")
ggsave("../Assignment_6/mpg_vs_wt_auto.tiff")

displacement <- mtcars[mtcars$disp <= 200,]
write.csv(displacement, "../Assignment_6/mtcars_max200_displ.csv")

disp_max <- max(displacement$mpg)
mtcars_max <- max(mtcars$mpg)
auto_max <- max(auto_only$mpg)

write.table(auto_max, "../Assignment_6/hp_maximums.txt", col.names = FALSE)
write.table(disp_max, "../Assignment_6/hp_maximums.txt", append = TRUE, col.names = FALSE)
write.table(mtcars_max, "../Assignment_6/hp_maximums.txt", append = TRUE, col.names = FALSE)



