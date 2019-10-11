fns <- list.files("./Data", pattern = "Messy", full.names = TRUE)

read.delim(fns[2], sep = " ")

messy <- read.delim(fns[2], sep = " ")

library(skimr)
skim(messy)

####convert odd "NA" values (-99999999) into NA

bad <- which(messy$value_std_dev == -99.99)
messy$value_std_dev[bad] <- NA


bad1 <- which(messy$value == -999.99)
messy$value[bad1] <- NA

####Filter out all the rows that include * in the first position of qcFlag

messy <- messy %>% filter(qcflag != "*..")

####replace the column name for value with new name "CO2_ppm"

names(messy)
names(messy)[names(messy) == "value"] <- "CO2_ppm"

#make "Date" column

paste(messy$year, messy$month, messy$day, sep = "-")

messy <- messy %>% 
  mutate(Date = as.POSIXct(paste(year,month,day, sep="-")))
messy

ggplot(messy, aes(x=Date, y=CO2_ppm)) +
  geom_point()

?gather





































