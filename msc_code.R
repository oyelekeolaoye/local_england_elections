#libraries
require(data.table)
require(rgdal)
library(ggplot2)
library(dplyr)
library(GGally)
library(tidyr)
library(tidyverse)
library(purrr)
library(data.table)
library(roomba)
library(magrittr)
library(plyr)
library(sf)



load("brexit.rdata")

#backup
brexit_orig <- brexit
results_orig <- results

brexit_areas <- brexit$LAName
results_areas <- names(results)

missing_in_results_dataset <- brexit_areas[!(brexit_areas %in% results_areas)]
missing_in_brexit_dataset <- results_areas[!(results_areas %in% brexit_areas)]

# removing things that don't exist on brexit
results <- results[-which(results_areas %in% missing_in_brexit_dataset)]

i <- 1

merged_dataset <- list()

for (results_column in names(results)) {
  # flattening 1 level
  row <- as.list(unlist(results[[results_column]]))
  row_brexit <- as.list(brexit[brexit$LAName == results_column,])
  
  merged_dataset[[i]] <- append(row_brexit, row)
  
  i <- i + 1
}

numeric_columns <- 14:40
merged_dataset <- rbindlist(merged_dataset, fill=TRUE)
merged_dataset[, numeric_columns] <- lapply(numeric_columns, function(col) {
  as.numeric(merged_dataset[[col]])
})

##inspecting NA values in brexit data frame
summary(brexit) ##all na's are zero values for no recorded votes for parties associated in the area
##replacing NAs by zeros
merged_dataset[is.na(merged_dataset)] <- 0

#SAVING MERGED DATASET AS CSV FILE
write.csv(merged_dataset, "merged.csv")

#duplicating merged dataset
current_df <- merged_dataset


#column names of merged dataset contain special characters and spaces in multiple patterns
#renaming columns
var_names <- c("la_name", "region", "ab", "c1", "c2", "de", "con_vote", "lab_vote", "ld_vote", "ukip_vote", 
               "green_vote", "leave", "control", "elected_ind", "elected_con", "elected_lab", "elected_ukip", 
               "total_ind", "total_con", "total_lab", "total_ukip", "change_ind", "change_con", "change_lab", 
               "change_ukip", "elected_green", "total_green", "change_green", "elected_ld", "total_ld", 
               "change_ld", "elected_ra", "total_ra", "change_ra", "elected_lp", "total_lp", "change_lp", 
               "elected_ichc", "total_ichc", "change_ichc")

colnames(current_df) <- var_names


#putting columns together
#we will merge variables for minor parties to the independent column

#FOR ELECTED VARIABLE
current_df$elected_ind <- current_df$elected_ind + current_df$elected_ichc + current_df$elected_lp + current_df$elected_ra
#removing the merged columns
current_df <- current_df %>% select(-elected_ichc, -elected_lp, -elected_ra)

#FOR CHANGE VARIABLE
current_df$change_ind <- current_df$change_ind + current_df$change_ichc + current_df$change_lp + current_df$change_ra
#removing the merged columns
current_df <- current_df %>% select(-change_ichc, -change_lp, -change_ra)

#FOR TOTAL VARIABLE
current_df$total_ind <- current_df$total_ind + current_df$total_ichc + current_df$total_lp + current_df$total_ra
current_df <- current_df %>% select(-total_ichc, -total_lp, -total_ra)


#COLLAPSING ELECTED AND TOTAL VARIABLE TO AVAILABLE SEATS AND TOTAL SEATS
current_df$seats_available <- current_df$elected_con + current_df$elected_green + current_df$elected_ind + current_df$elected_lab +
                              current_df$elected_ld + current_df$elected_ukip
current_df$total_seats <- current_df$total_con + current_df$total_green + current_df$total_ind + current_df$total_lab + 
                          current_df$total_ld + current_df$total_ukip

#SEPARATING ELECTED AND TOTAL VARIABLES FOR POSSIBLE LATER USE
partial_df <- current_df %>% select(la_name, elected_con, elected_green, elected_ind, elected_lab, elected_ld, elected_ukip
                                    , total_con, total_green, total_ind, total_lab, total_ld, total_ukip)
current_df <- current_df %>% select(-elected_con, -elected_green, -elected_ind, -elected_lab, -elected_ld, -elected_ukip
                                    , -total_con, -total_green, -total_ind, -total_lab, -total_ld, -total_ukip)



#converting control and region to factor
current_df$control <- as.factor(current_df$control)
current_df$region <- as.factor(current_df$region)




#combining region classes to N,E,S,W and Yorkshire and the Humber (reduces levels from 8 to 5)
current_df$region <- str_replace(current_df$region, "East Midlands", "East")
current_df$region <- str_replace(current_df$region, "North East", "North")
current_df$region <- str_replace(current_df$region, "North West", "North")
current_df$region <- str_replace(current_df$region, "South East", "South")
current_df$region <- str_replace(current_df$region, "South West", "South")
current_df$region <- str_replace(current_df$region, "West Midlands", "West")

current_df$region <- as.factor(current_df$region)



##NOTES
##259 councils were scheduled for election this year according to BBC
#The brexit data frame consist of 342 local areas, since only 248 local areas held elections in May 2, 2019 
#we are considering these 248 in the analysis, of these 248 local areas for which election results are available for,
#there are no brexit information for (10) of these areas, which implies that we have complete information on
#238 local areas.
names2 <- names(results_orig)
names2[!(names(results_orig)%in%brexit$LAName)] #areas for which there are no brexit information

sum(current_df$seats_available)/sum(current_df$total_seats)
#for the 238 areas for which we have complete information, 71.5% of the total seats were put up for election


#***************************************************
#PROPORTION OF SEATS WON BY EACH PARTY OF AVAILABLE
#***************************************************
sum(merged_dataset$elected.Conservative)/sum(current_df$seats_available)
#CONSERVATIVES took 41.9% of the seats available

sum(merged_dataset$elected.Labour)/sum(current_df$seats_available)
#LABOUR took 24.5% of the seats available

sum(merged_dataset$`elected.Liberal Democrat`)/sum(current_df$seats_available)
#LIBDEM took 15.7% of the seats available

sum(merged_dataset$elected.Green)/sum(current_df$seats_available)
#Greens took 3% of the seats available

sum(merged_dataset$elected.Independent)/sum(current_df$seats_available)
#Independent candidates took 12.6% of the seats available

sum(merged_dataset$`elected.UK Independence Party`)/sum(current_df$seats_available)
#UKIP took 0.3% of the seats available

#other minor parties combined took 2% of the seats available


sum(current_df$seats_available < current_df$total_seats)
sum((current_df$seats_available/current_df$total_seats) == (1/3))
#93 of 238 councils did not put up all their seats up for election 
#of these 93, 55 had one-third of their seats up for election

sum(current_df$seats_available == current_df$total_seats)
#145 of 238 councils had all seats up for election




#************************************************
#INSIGHTS FROM THE DATA
#************************************************
# The conservatives lost the most seats in "Bournemouth, Christchurch & Poole council in the South Region"
# Most of the people in this council belong to the C1 social grade. In contrast, the CONSERVATIVES obtained 48.7%
# votes in this council in the 2017 general election. No party currently has an overall control in this council but Independent candidates made
#the most gains.











#************************************************
#PROPORTION OF SEATS OWNED BY EACH PARTY OF TOTAL
#************************************************
sum(merged_dataset$total.Conservative)/sum(current_df$total_seats)
#CONSERVATIVES have 38.6% of the total seats in the 238 councils

sum(merged_dataset$total.Labour)/sum(current_df$total_seats)
#LABOUR have 31.8% of the total seats in the 238 councils

sum(merged_dataset$`total.Liberal Democrat`)/sum(current_df$total_seats)
#LIBDEM have 14.7% of the total seats in the 238 councils

sum(merged_dataset$total.Green)/sum(current_df$total_seats)
#GREENS have 2.5% of the total seats in the 238 councils

sum(merged_dataset$`total.UK Independence Party`)/sum(current_df$total_seats)
#UKIP have 0.37% of the total seats in the 238 councils

sum(merged_dataset$total.Independent)/sum(current_df$total_seats)
#INDEPENDENT candidates have 10.2% of the total seats in the 238 councils

#Other minor parties combined own 1.83% of the total seats in the 238 councils



#******************************************************************************************************
#---DATA EXPLORATION
#******************************************************************************************************


#******************************************************************************************************
#---SUMMARY STATISTICS
#******************************************************************************************************
summary(current_df)
#from the summary stats, CONSERVATIVES held control in 83 councils, LABOUR held control in 53 councils
#CONSERVATIVES lost to No Overall Control in 33 Councils, LABOUR lost to No Overall Control in 10 councils
#LIBDEM gained from CONSERVATIVES in 7 councils

table(current_df$control)

mean(current_df$change.Conservative)
var(current_df$change.Conservative)



#******************************************************************************************************
#---BOXPLOTS FOR change variable FOR EACH PARTY
#******************************************************************************************************
ggplot(data = current_df, mapping = aes(y=current_df$change.Conservative)) +
  geom_boxplot()
#boxplot indicates 8 outliers/normal distribution

ggplot(data = current_df, mapping = aes(y=current_df$change.Green)) +
  geom_boxplot()
#boxplot indicates 6 outliers/heavy right skewness

ggplot(data = current_df, mapping = aes(y=current_df$change.Independent)) +
  geom_boxplot()
#boxplot indicates 10 outlying values/right skewness

ggplot(data = current_df, mapping = aes(y=current_df$change.Labour)) +
  geom_boxplot()
#boxplot indicates normal distribution/many outlying values

ggplot(data = current_df, mapping = aes(y=current_df$change.Liberal_Democrat)) +
  geom_boxplot()
#right skewness/couple of outlying values

ggplot(data = current_df, mapping = aes(y=current_df$`change.UK_Independence Party`)) +
  geom_boxplot()
ggplot(data = current_df, mapping = aes(x=Region, y=current_df$`change.UK_Independence Party`)) +
  geom_boxplot()
#most of the points for UKIP are zeros(185 total zeros of 238 observations)
#boxplot not very informative but most of the positives came from Yorkshire and the Humber after splitting plots by region




#******************************************************************************************************
#---scatterplots for examining relationship between social class AB & loss/gain
#*****************************************************************************************************
ggplot(current_df, aes(change.Conservative, AB)) +
  geom_point()
##conservatives seem to record less losses in areas with low proportion of social grade AB
##there seems to be a mild inverse relationship between social grade AB and loss for convervatives

ggplot(current_df, aes(change.Labour, AB)) +
  geom_point()
##most of the losses recorded by labour seem to be in areas with low proportion of social grade AB
##majority of the data points indicate a no change for labour and gains where recorded in areas with high proportion of social grade AB

ggplot(current_df, aes(change.Green, AB)) +
  geom_point()
##there is no clear pattern of a relationship between social grade AB and change for Green
##high gains for green seem to be from areas with median proportion (0.2-0.3) of social grade AB

ggplot(current_df, aes(current_df$`change.UK_Independence Party`, AB)) +
  geom_point()
##there doesn't seem to be a clear relationship between change for UKIP and social grade AB

ggplot(current_df, aes(current_df$change.Liberal_Democrat, AB)) +
  geom_point()
##there seems to be a positive relationship between proportion of social grade AB and gains for LD
##more gains where recorded in areas with more than 20% of people in social grade AB


ggplot(current_df, aes(current_df$change.Independent, AB)) +
  geom_point()
##there seems to be a negative relationship between AB and change for Independents
##more gains where recorded in areas with less than 30% of people in social grade AB

ggplot(current_df, aes(current_df$change.others, AB)) +
  geom_point()
##the pattern of relationship is not clear for other minor parties combined into "others" for social grade AB



#******************************************************************************************************
#---scatterplots for examining relationship between social class C1 & loss/gain
#*****************************************************************************************************
ggplot(current_df, aes(change.Conservative, C1)) +
  geom_point()
##the higher the losses came from areas with high proportion of C1 social grade for the Tories
ggplot(current_df, aes(current_df$change.Green, C1)) +
  geom_point()
##the pattern is not clear for C1 and change for Greens

ggplot(current_df, aes(current_df$change.Independent, C1)) +
  geom_point() 
##there seems to be a mild inverse relationship between C1 and gains for Independents
##more gains came from areas with less than 30% of people in social grade C1

ggplot(current_df, aes(current_df$change.Labour, C1)) +
  geom_point()
##labour seem to do well in areas with more people in social grade C1

ggplot(current_df, aes(current_df$change.Liberal_Democrat, C1)) +
  geom_point() 
#liberal democrats seem to be gain more in areas with more people in social grade C1
#although the pattern is not clear, there seems to be a slightly positive relationship
#between proportion of C1 grade and gains for LibDem
ggplot(current_df, aes(current_df$`change.UK_Independence Party`, C1)) +
  geom_point() 
#the pattern is not clear for UKIP and C1

ggplot(current_df, aes(current_df$change.others, C1)) +
  geom_point() 
#the pattern is not clear for other minor parties 

#******************************************************************************
#---scatterplots for examining relationship between social class C2 & loss/gain
#*******************************************************************************

ggplot(current_df, aes(current_df$change.Conservative, C2)) +
  geom_point() 
#there doesn't seem to be a linear relationship between proportion of C2 and change for conservatives

ggplot(current_df, aes(current_df$change.Green, C2)) +
  geom_point() 
#the pattern is not clear for C2 and greens

ggplot(current_df, aes(current_df$change.Independent, C2)) +
  geom_point() 
#there seems to be a slightly positive relationship between C2 and change for Independent candidates
#this relationship seems to be weak

ggplot(current_df, aes(current_df$change.Labour, C2)) +
  geom_point() 
#the pattern is not clear for C2 and change for labour

ggplot(current_df, aes(current_df$change.Liberal_Democrat, C2)) +
  geom_point() 
#the pattern is not clear for C2 and change for LibDem

ggplot(current_df, aes(current_df$change.others, C2)) +
  geom_point() 
#the pattern is not clear for C2 and change for other minor parties

ggplot(current_df, aes(current_df$`change.UK_Independence Party`, C2)) +
  geom_point() 
#the pattern is not clear for C2 and change for UKIP

#******************************************************************************
#---scatterplots for examining relationship between social class DE & loss/gain
#*******************************************************************************

ggplot(current_df, aes(current_df$change.Conservative, DE)) +
  geom_point() 
#there seem to be a positive relationship between proportion for DE and gain for Cons 
#they gained most in areas with more than 20% to 40% of people in social class DE

ggplot(current_df, aes(current_df$change.Independent, DE)) +
  geom_point() 
#the pattern is not clear for DE and change for Independent candidates

ggplot(current_df, aes(current_df$change.Green, DE)) +
  geom_point() 
#the pattern is not clear for DE and change for Greens

ggplot(current_df, aes(current_df$change.Labour, DE)) +
  geom_point() 
#there seems to be an inverse relationship for DE and change for labour 
#they gained more in areas with less than 30% people in social class DE
#*and lost most in areas with 30% to 40% of people in social class DE

ggplot(current_df, aes(current_df$change.Liberal_Democrat, DE)) +
  geom_point()
#the relationship between DE and change for LibDem seems to be inverse
#*they gained most in areas with less than 30% of people in grade DE

ggplot(current_df, aes(current_df$change.others, DE)) +
  geom_point()
#there is no clear pattern for other minor parties

ggplot(current_df, aes(current_df$`change.UK_Independence Party`, DE)) +
  geom_point()
#there is no clear pattern for DE and change for UKIP

#******************************************************************************
#---scatterplots for examining relationship between LEAVE votes & loss/gain
#*******************************************************************************

ggplot(current_df, aes(current_df$change.Conservative, Leave)) +
  geom_point()
##there seems to be a moderate positve relationship between leave votes and change for Cons
#Cons suffered less loss in areas with high proportion of leave votes
#their gains came from areas with more than 50% leave voters 

ggplot(current_df, aes(current_df$change.Green, Leave)) +
  geom_point()
#the pattern doesn't seem to be clear for leave and change for Greens
#there is a slight indication of an inverse relationship
#more of the gains came from areas with low proportion of leave votes 

ggplot(current_df, aes(current_df$change.Independent, Leave)) +
  geom_point()
#there is a slight indication of a positive relationship for Greens and leave votes
#more gains came from areas with high proportion of leave voters

ggplot(current_df, aes(current_df$change.Labour, Leave)) +
  geom_point()
#the pattern seems random for leave votes and change for labour

ggplot(current_df, aes(current_df$change.Liberal_Democrat, Leave)) +
  geom_point()
#there is a slight indication of an inverse relationship for leave votes and change for LibDem
#more gains came from areas with less than 60% of leave voters

ggplot(current_df, aes(current_df$change.others, Leave)) +
  geom_point()
#the pattern between leave votes and change for other minor parties is not clear

ggplot(current_df, aes(current_df$`change.UK_Independence Party`, Leave)) +
  geom_point()
#the pattern between leave votes and change for UKIP is not clear


#******************************************************************************************************
#---scatterplots for examining popularity in 2017 general election vs loss/gain in 2019 for each party
#******************************************************************************************************

ggplot(current_df, aes(current_df$change.Conservative, ConVote)) +
  geom_point()
#conservatives seem to lose most in areas which they were popular in 2017

ggplot(current_df, aes(current_df$change.Green, GreenVote)) +
  geom_point()
#the pattern is not clear for greens

ggplot(current_df, aes(current_df$change.Liberal_Democrat, LDVote)) +
  geom_point()
#the pattern is not clear for liberal democrats

ggplot(current_df, aes(current_df$change.Labour, LabVote)) +
  geom_point()
#labour seem to lose most in areas where they were popular in 2017
#similar pattern observed for Conservatives


ggplot(current_df, aes(current_df$`change.UK_Independence Party`, UKIPVote)) +
  geom_jitter()
#the pattern is not clear for UKIP, one outlier observed as they lost most in an area where they had the most popularity in 2017


#******************************************************************************************************
#---HISTOGRAMS FOR CHANGE VARIABLE TO CHECK FOR NORMALITY
#******************************************************************************************************

ggplot(data = current_df, mapping = aes(x = current_df$change.Conservative)) +
  geom_histogram(color = "white", fill = "steelblue") 
#histogram is roughly symmetric

ggplot(data = current_df, mapping = aes(x = current_df$change.Green)) +
  geom_histogram(color = "white", fill = "steelblue")
#histogram is skewed to the right

ggplot(data = current_df, mapping = aes(x = current_df$change.Independent)) +
  geom_histogram(color = "white", fill = "steelblue")
#histogram is skewed to the right

ggplot(data = current_df, mapping = aes(x = current_df$change.Labour)) +
  geom_histogram(color = "white", fill = "steelblue")
#histogram is roughly symmetric

ggplot(data = current_df, mapping = aes(x = current_df$change.Liberal_Democrat)) +
  geom_histogram(color = "white", fill = "steelblue")
#histogram is skewed to the right

ggplot(data = current_df, mapping = aes(x = current_df$`change.UK_Independence Party`)) +
  geom_histogram(color = "white", fill = "steelblue", binwidth = 10)
#histogram is skewed to the left

ggplot(data = current_df, mapping = aes(x = current_df$change.others)) +
  geom_histogram(color = "white", fill = "steelblue", binwidth = 10)
#histogram is skewed to the right

#******************************************************************************************************
#---HISTOGRAMS FOR CHANGE VARIABLE BY REGION
#******************************************************************************************************

ggplot(data = current_df, mapping = aes(x = current_df$change.Conservative)) +
  geom_histogram(color = "white", fill = "steelblue") +
  facet_wrap(~ Region)
#roughly normal distribution for CONS in each region, west and yorkshire are of relatively smaller sizes compared to others, so no surprises

ggplot(data = current_df, mapping = aes(x = current_df$change.Green)) +
  geom_histogram(color = "white", fill = "steelblue") +
  facet_wrap(~ Region)
#same right skewness observed for each region for the GREENS

ggplot(data = current_df, mapping = aes(x = current_df$change.Independent)) +
  geom_histogram(color = "white", fill = "steelblue") +
  facet_wrap(~ Region)
#same right skewness observed for each region, roughly normal in Yorkshire and the Humber for Independents

ggplot(data = current_df, mapping = aes(x = current_df$change.Labour)) +
  geom_histogram(color = "white", fill = "steelblue") +
  facet_wrap(~ Region)
#roughly normal distribution observed in each region for LABOURS

ggplot(data = current_df, mapping = aes(x = current_df$change.Liberal_Democrat)) +
  geom_histogram(color = "white", fill = "steelblue") +
  facet_wrap(~ Region)
#right skewness observed for each region for LIBDEM

ggplot(data = current_df, mapping = aes(x = current_df$`change.UK_Independence Party`)) +
  geom_histogram(color = "white", fill = "steelblue") +
  facet_wrap(~ Region)
#left skewness observed for UKIP


#******************************************************************************************************
#---FITTING NORMAL LINEAR MODELS FOR EACH PARTY
#******************************************************************************************************
model1 <- lm(current_df$change.Conservative ~ ConVote + Leave, data = current_df)
summary(model1)




#******************************************************************************************************
#---PAIRS PLOT
#******************************************************************************************************
#pairs plot for social grade and change variable
cormat <- current_df %>% select(-ConVote, -LabVote, -GreenVote, -UKIPVote, -LDVote, -LAName, -Region, -control, -seats_available, -total_seats)
ggpairs(cormat)

##pairs plot for proportion of 2017 elections and change variable
cormat2 <- current_df %>% select(-AB, -C1, -C2, -DE, -Leave, -LAName, -Region, -control, -seats_available, -total_seats)
ggpairs(cormat2)



#******************************************************************************************************
#---IMPORTING SHAPE FILE AND DRAWING MAPS
#******************************************************************************************************
map <- st_read("Local_Authority_Districts_December_2017_Full_Extent_Boundaries_in_United_Kingdom_WGS84.shp")
str(map)
