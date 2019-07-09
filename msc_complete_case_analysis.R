#libraries
require(data.table)
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
library(tmap)
library(tmaptools)
library(leaflet)
library(skimr)
library(moderndive)
library(snakecase)
library(olsrr)
library(car)
library(plotrix)


#displaying numbers and avoiding scientific notation
options(scipen = 999)

#load data file given for project
load("brexit.rdata")

#we will first consider a complete case of the brexit frame
#the total number of incomplete cases is 40 of 342

brexit <- subset(brexit, complete.cases(brexit))

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

#We have complete information of 207 observations of 248 that held local elections in 2019
#We will focus on these 207 observations for now...

##all na's after merging are zero values for no seats for represented parties in the area
##replacing NAs by zeros
merged_with_nas <- merged_dataset
merged_dataset[is.na(merged_dataset)] <- 0

#duplicating merged dataset
current_df <- merged_dataset


#column names of merged dataset contain special characters and white spaces in multiple patterns
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

#FOR ELECTED VARIabLE
current_df$elected_ind <- current_df$elected_ind + current_df$elected_ichc + current_df$elected_lp + current_df$elected_ra
#removing the merged columns
current_df <- current_df %>% select(-elected_ichc, -elected_lp, -elected_ra)

#FOR CHANGE VARIabLE
current_df$change_ind <- current_df$change_ind + current_df$change_ichc + current_df$change_lp + current_df$change_ra
#removing the merged columns
current_df <- current_df %>% select(-change_ichc, -change_lp, -change_ra)

#FOR TOTAL VARIabLE
current_df$total_ind <- current_df$total_ind + current_df$total_ichc + current_df$total_lp + current_df$total_ra
current_df <- current_df %>% select(-total_ichc, -total_lp, -total_ra)


#COLLAPSING ELECTED AND TOTAL VARIabLE TO AVAILabLE SEATS AND TOTAL SEATS
current_df$seats_available <- current_df$elected_con + current_df$elected_green + current_df$elected_ind + current_df$elected_lab +
  current_df$elected_ld + current_df$elected_ukip
current_df$total_seats <- current_df$total_con + current_df$total_green + current_df$total_ind + current_df$total_lab + 
  current_df$total_ld + current_df$total_ukip

#SEPARATING ELECTED AND TOTAL VARIabLES FOR POSSIBLE LATER USE
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
##248 councils were scheduled for election this year according to BBC
#The brexit data frame consist of 342 local authorities(including some local authorities in Wales), since only 248 local areas held elections in May 2, 2019 
#we are considering these 248 in the analysis, of these 248 local areas for which election results are available for,
#there are no brexit information for (10) of these areas, which implies that we have information on 238 local areas.
#Of 238 councils, we have complete information without NAs on 207 which we are considering now.

sum(current_df$seats_available)/sum(current_df$total_seats)
#for the 207 areas for which we have complete information, 72.5% of the total seats were put up for election

#***************************************************
#PROPORTION OF SEATS WON BY EACH PARTY OF AVAILabLE
#***************************************************
sum(merged_dataset$elected.Conservative)/sum(current_df$seats_available)
#CONSERVATIVES took 42.4% of the seats available

sum(merged_dataset$elected.Labour)/sum(current_df$seats_available)
#LabOUR took 23.4% of the seats available

sum(merged_dataset$`elected.Liberal Democrat`)/sum(current_df$seats_available)
#LIBdeM took 16.5% of the seats available

sum(merged_dataset$elected.Green)/sum(current_df$seats_available)
#Greens took 3.3% of the seats available

sum(merged_dataset$elected.Independent)/sum(current_df$seats_available)
#Independent candidates took 11.9% of the seats available

sum(merged_dataset$`elected.UK Independence Party`)/sum(current_df$seats_available)
#UKIP took 0.4% of the seats available

#other minor parties combined took 2.1% of the seats available

##CREATING A PIE CHART FOR PROPORTION WON
slices <- c(42.4, 23.4, 16.5, 3.3, 0.4, 11.9, 2.1) 
lbls <- c("Conservative-42.4%", "Labour-23.4%", "LibDem-16.5%", "Green-3.3%", "UKIP-0.4%", "Independents-11.9%", "Other-2.1%")
pie3D(slices,labels=lbls,explode=0.1,
      main="Proportion of Seats Won")

sum(current_df$seats_available < current_df$total_seats)
sum((current_df$seats_available/current_df$total_seats) == (1/3))
#78 of 207 councils did not put up all their seats up for election 
#of these 93, 43 had one-third of their seats up for election

sum(current_df$seats_available == current_df$total_seats)
#129 of 207 councils had all seats up for election


#************************************************
#PROPORTION OF SEATS OWNED BY EACH PARTY OF TOTAL
#************************************************
sum(merged_dataset$total.Conservative)/sum(current_df$total_seats)
#CONSERVATIVES have 39.4% of the total seats in the 207 councils

sum(merged_dataset$total.Labour)/sum(current_df$total_seats)
#LabOUR have 30.1% of the total seats in the 207 councils

sum(merged_dataset$`total.Liberal Democrat`)/sum(current_df$total_seats)
#LIBdeM have 15.7% of the total seats in the 207 councils

sum(merged_dataset$total.Green)/sum(current_df$total_seats)
#GREENS have 2.8% of the total seats in the 207 councils

sum(merged_dataset$`total.UK Independence Party`)/sum(current_df$total_seats)
#UKIP have 0.38% of the total seats in the 207 councils

sum(merged_dataset$total.Independent)/sum(current_df$total_seats)
#INdePENdeNT candidates have 9.8% of the total seats in the 207 councils

#Other minor parties combined own 1.82% of the total seats in the 238 councils


##CONVERTING CHANGE VARIABLE TO PROPORTION
current_df$change_con <- current_df$change_con/current_df$seats_available
current_df$change_ind <- current_df$change_ind/current_df$seats_available
current_df$change_lab <- current_df$change_lab/current_df$seats_available
current_df$change_ld <- current_df$change_ld/current_df$seats_available
current_df$change_ukip <- current_df$change_ukip/current_df$seats_available
current_df$change_green <- current_df$change_green/current_df$seats_available

#EXPLORATORY DATA ANALYSIS
summary(current_df)
skim(current_df)
