# Tuesday #

library(ggplot2)
install.packages("viridis")
library("viridis")


# Git pull all student repos to grade

# Go over assignment 3 (keep it brief)

# practice *exploring* data using ./Data/landdata-states

  # summaries
  # histograms
  # boxplots
  # scatterplots
  # coloring by category
  # subsetting

df = read.csv("./Data/landdata-states.csv", stringsAsFactors = TRUE)
str(df)
summary(df)

plot(x=df$Year,y=df$Home.Value,col=df$region)

df$State[7803] ####Shows you the 7803rd state on that list
df$State[7804] ####Won't show you anything because there aren't 7804 units

# summary
summary(df$Home.Value)

# histogram of home value
hist(df$Home.Value,breaks = 50)

# histogram of state
hist(df$State)

# histogram of land value
hist(df$Land.Value,breaks=50)

plot(df$Home.Value,df$Land.Value)

# boxplot
plot(x=df$region,df$Home.Value,col="Red")

# boxplot ("quarter" needs to be a factor to get a boxplot)
plot(as.factor(df$Qrtr),df$Home.Value)

#
##Year, Home value, and one other variable

library(ggplot2)

df1 <- ggplot(na.omit(df), aes(x=Year, y=Home.Value)) + geom_point(aes(color=Land.Value), size=2, alpha=.5) + 
  geom_smooth(color="firebrick") +
  labs(title = "Home Value over Years", x = "Year", y = "Home Value") + facet_wrap(~region) +
  scale_color_viridis(option = "C", "Land Value") 

df1


#### for example: + scale_color_gradient(low = "#132B43", high = "#56B1F7")
#### for viridis color options -> scale_color_viridis(option = "A-D", n) where n = the label for the legend that it supplies

df$ID <- row.names(df)


# just look at homes in the "West" ... these should be the same, but give different results. Why?
west <- subset(df,region=="West")
west2 <- df[df$region == "West",]


plot(west$Year,west$Home.Value,col=west$State)


# Which state is that up at the top!