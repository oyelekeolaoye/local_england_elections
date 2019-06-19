library(tidyr)
library(ggplot2)
library(dplyr)

#exploratory analysis for the brexit data frame
head(brexit)
tail(brexit)
glimpse(brexit)
summary(brexit)
boxplot(brexit[,-c(1,2)]) ##all initial continous variable boxplot....no unusual values apart of outliers...all variables have been standardized prior to data exploration
hist(brexit$AB)
hist(brexit$C1)
hist(brexit$C2)
hist(brexit$DE)
hist(brexit$ConVote)
hist(brexit$LabVote)
hist(brexit$LDVote)
hist(brexit$UKIPVote)
hist(brexit$GreenVote)
hist(brexit$Leave) #all initial histogram of variables...most variables not-normal

#creating new columns to begin merge
brexit$control_2019 <- NA 
brexit$seats_available <- NA
brexit$con_2019 <- NA
brexit$lab_2019 <- NA
brexit$ld_2019 <- NA
brexit$ukip_2019 <- NA
brexit$green_2019 <- NA

#exploring the list (results)
summary(results)
results[[2]]
str(results)
