library(tidyverse)

##Once you get the file loaded into an R object as a data frame, feel free to do some exploratory visualizations or summaries to get a feel for the data if you like.
##Your first task, though, is to create separate histograms of the DNA concentrations for Katy and Ben. Make sure to add nice labels to these (x-axis and main title).

df <- read.csv("./DNA_Conc_by_Extraction_Date.csv")

ggplot(df, aes(x=DNA_Concentration_Katy)) +
  geom_histogram()+
  labs(title="Katy's Concentrations", x="Katy's DNA Concentrations")

ggplot(df, aes(x=DNA_Concentration_Ben)) +
  geom_histogram()+ 
  labs(title="Ben's Concentrations", x="Ben's DNA Concentrations")

##II. 	
##Your second task is to look at DNA concentrations from the different extraction years. 
##One way to do this is a separate figure for each student is demonstrated in those two files:	ZAHN_Plot1.jpeg and ZAHN_Plot2.jpeg 
##Open those files in some image viewing program and take a look. I'd like you to re-create these exactly, including the labels.
##This is tricky, so I'll give a hint: the plot() function behaves differently depending on the classes of vectors that are given to it.

# III.
# Once you have your code for creating the figures correctly, you need to save those two images in YOUR Exam_1 directory. Name them similarly to how I named mine, but with your LASTNAME
# Make sure your code is saving the files. Don't do it manually with the mouse!
# 

ggplot(df, aes(x=as.character(Year_Collected), y=DNA_Concentration_Katy))+
  geom_boxplot()+
  labs(title="Katy's Extractions", x="Year", y="DNA Concentration") +
  theme(plot.title=element_text(hjust=.5))
ggsave("./EDWARDS_Plot1.jpg")

ggplot(df, aes(x=as.character(Year_Collected), y=DNA_Concentration_Ben))+
  geom_boxplot()+
  labs(title="Ben's Extractions", x="Year", y="DNA Concentration") +
  theme(plot.title=element_text(hjust=.5))
ggsave("./EDWARDS_Plot2.jpg")


##IV.
##Take a look at Ben's concentrations vs Katy's concentrations. You can do this however you like... with a plot or with summary stats or both.
##It looks like Ben had consistently higher DNA yields than Katy did...but surely it wasn't uniformly better, right? With some samples, he only had a marginal improvement over Katy.
##With other samples, he had a relatively massive improvement over her.
##Your task here is to write some code that tells us: in which extraction YEAR, was Ben's performance the lowest RELATIVE TO Katy's performance?

difference <- df$DNA_Concentration_Ben - df$DNA_Concentration_Katy


max.difference <- which(difference == max(difference))


df[max.difference,"Year_Collected"]


##V.
##Do another subset of the data for me. Subset the data frame so it's just the "Downstairs" lab.
##Now, make a scatterplot of the downstairs lab data such that "Date_Collected" is on the x-axis and 
#"DNA_Concentration_Ben" is on the y-axis. Save this scatterplot as "Ben_DNA_over_time.jpg" in your Exam_1 
#directory. See the file "Downstairs.jpg" for an example of how yours should look. If it looks different, you 
#might need to do some class conversions so the plot() function treats things correctly. HintHintHint: POSIXct

downstairs <- df[df$Lab == "Downstairs",]

ggplot(downstairs, aes(x=as.POSIXct(Date_Collected), y=DNA_Concentration_Ben)) +
  geom_point() +
  labs(title="Ben's DNA Extractions by Year", x="Year Collected", y="DNA Concentrations") +
  theme(plot.title = element_text(hjust = .5))
ggsave("./Ben_DNA_Over_Time.jpg")


#VI.
#For this final (BONUS) problem, let's just look at Ben's DNA concentration values. I 
#think Katy messed up her PCRs, and at any rate, we can't use them for sequencing.
#Besides, our original purpose for this experiment was to see if DNA extractions sitting in a freezer degraded over time.
#To that end, I want you to make a new data frame (just using Ben's values) that has one 
#column containing the years that DNA extractions were made, 
#and another column that contains the AVERAGE of the values within that year.  
#Just to be clear, this data frame should have only 12 rows (one for each year)! You will need to 
#find a way to take the average of Ben's DNA values in each separate year. 
#A for-loop, or repeated subsetting, or some other way...
#Once you have this new data frame of averages by year, write some code that shows which extraction 
#year has the highest average DNA concentration (and what that concentration is) and then save the 12-row 
#dataframe as a new csv file called "Ben_Average_Conc.csv"


ben2000 <- mean(df[df$Year_Collected == 2000,"DNA_Concentration_Ben"])
ben2001 <- mean(df[df$Year_Collected == 2001,"DNA_Concentration_Ben"])
ben2002 <- mean(df[df$Year_Collected == 2002,"DNA_Concentration_Ben"])
ben2003 <- mean(df[df$Year_Collected == 2003,"DNA_Concentration_Ben"])
ben2004 <- mean(df[df$Year_Collected == 2004,"DNA_Concentration_Ben"])
ben2005 <- mean(df[df$Year_Collected == 2005,"DNA_Concentration_Ben"])
ben2006 <- mean(df[df$Year_Collected == 2006,"DNA_Concentration_Ben"])
ben2007 <- mean(df[df$Year_Collected == 2007,"DNA_Concentration_Ben"])
ben2008 <- mean(df[df$Year_Collected == 2008,"DNA_Concentration_Ben"])
ben2010 <- mean(df[df$Year_Collected == 2010,"DNA_Concentration_Ben"])
ben2011 <- mean(df[df$Year_Collected == 2011,"DNA_Concentration_Ben"])
ben2012 <- mean(df[df$Year_Collected == 2012,"DNA_Concentration_Ben"])


vec <- c(ben2000,ben2001,ben2002,ben2003,ben2004,ben2005,ben2006,
         ben2007,ben2008,ben2010,ben2011,ben2012)


ben_dat <- data.frame(Year = levels(as.factor(df$Year_Collected)),
                      Ben_Mean = vec)

ben_dat

max.row <- which(ben_dat$Ben_Mean == max(ben_dat$Ben_Mean))


ben_dat[max.row,]

write.csv(ben_dat, file = "./Ben_Average_Conc.csv")
