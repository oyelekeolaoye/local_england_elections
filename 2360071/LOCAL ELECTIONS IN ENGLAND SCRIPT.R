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
library(ResourceSelection)
library(spdep)
library(corrplot)
library(caTools)
library(e1071)
library(glmnet)
library(jtools)
library(BeSS)
library(gridExtra)
library(pROC)
library(sp)
library(rgdal)
set.seed(420)

#displaying numbers and avoiding scientific notation
options(scipen = 999)

#load data file given for project
load("brexit.rdata")

#we consider a complete case analysis
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
#We will focus on these 207 observations.

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




#combining region classes to N,E,S,W and Yorkshire and the Humber (reduces levels from 8 to 5)
current_df$region <- str_replace(current_df$region, "East Midlands", "East")
current_df$region <- str_replace(current_df$region, "North East", "North")
current_df$region <- str_replace(current_df$region, "North West", "North")
current_df$region <- str_replace(current_df$region, "South East", "South")
current_df$region <- str_replace(current_df$region, "South West", "South")
current_df$region <- str_replace(current_df$region, "West Midlands", "West")
current_df$region <- str_replace(current_df$region, "Yorkshire and The Humber", "North")

#converting control and region to factor
current_df$control <- as.factor(current_df$control)
current_df$region <- as.factor(current_df$region)

##CONVERTING CHANGE VARIABLE TO PROPORTION CHANGE
current_df$change_con <- current_df$change_con/current_df$seats_available
current_df$change_ind <- current_df$change_ind/current_df$seats_available
current_df$change_lab <- current_df$change_lab/current_df$seats_available
current_df$change_ld <- current_df$change_ld/current_df$seats_available
current_df$change_ukip <- current_df$change_ukip/current_df$seats_available
current_df$change_green <- current_df$change_green/current_df$seats_available

##NOTES
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


sum(current_df$seats_available < current_df$total_seats)
sum((current_df$seats_available/current_df$total_seats) == (1/3))
#78 of 207 councils did not put up all their seats up for election 
#of these 93, 43 had one-third of their seats up for election

sum(current_df$seats_available == current_df$total_seats)
#129 of 207 councils had all seats up for election


#************************************************
#INSIGHTS FROM THE DATA
#************************************************
# The conservatives lost the most seats in "Bournemouth, Christchurch & Poole council in the South region"
# Most of the people in this council belong to the c1 social grade. In contrast, the CONSERVATIVES obtained 48.7%
# votes in this council in the 2017 general election. No party currently has an overall control in this council but Independent candidates made
#the most gains.
current_df[which.min(current_df$change_con),]
current_df[which.min(current_df$change_lab),]
current_df[which.max(current_df$change_ld),]
current_df[which.max(current_df$change_ind),]
current_df[which.max(current_df$change_green),]

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
#GREENS have 2.7% of the total seats in the 207 councils

sum(merged_dataset$`total.UK Independence Party`)/sum(current_df$total_seats)
#UKIP have 0.37% of the total seats in the 207 councils

sum(merged_dataset$total.Independent)/sum(current_df$total_seats)
#INdePENdeNTs have 11.73% of the total seats in the 207 councils





#******************************************************************************************************
#---DATA EXPLORATION
#******************************************************************************************************


#******************************************************************************************************
#---SUMMARY STATISTICS
#******************************************************************************************************
summary(current_df)
skim(current_df)
#from the summary stats, CONSERVATIVES held control in 86 councils, LabOUR held control in 53 councils
#CONSERVATIVES lost to No Overall Control in 33 Councils, LabOUR lost to No Overall Control in 10 councils
#LIBdeM gained from CONSERVATIVES in 7 councils




#******************************************************************************************************
#---BOXPLOTS FOR change variable FOR EACH PARTY
#******************************************************************************************************
ggplot(data = current_df, mapping = aes(y=current_df$change_con)) +
  geom_boxplot() +
  ggtitle("BOXPLOT OF LOSS/GAIN FOR CONSERVATIVES")
#boxplot indicates 8 outliers/normal distribution

ggplot(data = current_df, mapping = aes(y=current_df$change_green)) +
  geom_boxplot() + 
  ggtitle("BOXPLOT OF LOSS/GAIN FOR GREENS")
#boxplot indicates 6 outliers/right skewness

ggplot(data = current_df, mapping = aes(y=current_df$change_ind)) +
  geom_boxplot() + 
  ggtitle("BOXPLOT OF LOSS/GAIN FOR INdePENdeNTS")
#boxplot indicates 7 outlying values/right skewness

ggplot(data = current_df, mapping = aes(y=current_df$change_lab)) +
  geom_boxplot() + 
  ggtitle("BOXPLOT OF LOSS/GAIN FOR LabOUR")
#boxplot indicates normal distribution/many outlying values

ggplot(data = current_df, mapping = aes(y=current_df$change_ld)) +
  geom_boxplot() +
  ggtitle("BOXPLOT OF LOSS/GAIN FOR LIBERAL deMOCRATS")
#right skewness/couple of outlying values

ggplot(data = current_df, mapping = aes(y=current_df$change_ukip)) +
  geom_boxplot()+ 
  ggtitle("BOXPLOT OF LOSS/GAIN FOR UKIP")
ggplot(data = current_df, mapping = aes(x=region, y=current_df$change_ukip)) +
  geom_boxplot() + 
  ggtitle("BOXPLOT OF LOSS/GAIN FOR UKIP BY REGION")
#most of the points for UKIP are zeros(185 total zeros of 238 observations)
#boxplot not very informative but most of the positives came from Yorkshire and the Humber after splitting plots by region

#******************************************************************************************************
#---scatterplots for examining relationship between social grade ab & loss/gain
#*****************************************************************************************************
ggplot(current_df, aes(change_con, ab)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)
##conservatives seem to record less losses in areas with low proportion of social grade ab
##there seems to be a mild inverse relationship between social grade ab and loss for convervatives

ggplot(current_df, aes(change_lab, ab)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)
##most of the losses recorded by labour seem to be in areas with low proportion of social grade ab
##majority of the data points indicate a no change for labour and gains where recorded in areas with high proportion of social grade ab

ggplot(current_df, aes(change_green, ab)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)
##there is no clear pattern of a relationship between social grade ab and change for Green
##high gains for green seem to be from areas with median proportion (0.2-0.3) of social grade ab

ggplot(current_df, aes(current_df$change_ukip, ab)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)
##there doesn't seem to be a clear relationship between change for UKIP and social grade ab

ggplot(current_df, aes(current_df$change_ld, ab)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)
##there seems to be a positive relationship between proportion of social grade ab and gains for LD
##more gains where recorded in areas with more than 20% of people in social grade ab


ggplot(current_df, aes(current_df$change_ind, ab)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)
##there seems to be a negative linear relationship between ab and change for Independents
##more gains where recorded in areas with less than 30% of people in social grade ab


#******************************************************************************************************
#---scatterplots for examining relationship between social grade c1 & loss/gain
#*****************************************************************************************************
ggplot(current_df, aes(change_con, c1)) +
  geom_point() + 
  ggtitle("SCATTERPLOT OF SOCIAL GRAde c1 VS CHANGE FOR CONS") +
  geom_smooth(method = "lm", se = FALSE)
##the higher the losses came from areas with high proportion of c1 social grade for the Tories
##roughly linear and negative relationship

ggplot(current_df, aes(current_df$change_green, c1)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)
##the pattern is not clear for c1 and change for Greens

ggplot(current_df, aes(current_df$change_ind, c1)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)
##there seems to be a mild inverse relationship between c1 and gains for Independents
##more gains came from areas with less than 30% of people in social grade c1

ggplot(current_df, aes(current_df$change_lab, c1)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)
##labour seem to do well in areas with more people in social grade c1
##scatterplot indicates positive linear relationship

ggplot(current_df, aes(current_df$change_ld, c1)) +
  geom_point()  +
  geom_smooth(method = "lm", se = FALSE)
#liberal democrats seem to be gain more in areas with more people in social grade c1
#although the pattern is not clear, there seems to be a slightly positive linear relationship
#between proportion of c1 grade and gains for Libdem

ggplot(current_df, aes(current_df$change_ukip, c1)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)
#the pattern is not clear for UKIP and c1


#******************************************************************************
#---scatterplots for examining relationship between social grade c2 & loss/gain
#*******************************************************************************

ggplot(current_df, aes(current_df$change_con, c2)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)
#there doesn't seem to be a linear relationship between proportion of c2 and change for conservatives
#pattern seems random

ggplot(current_df, aes(current_df$change_green, c2)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)
#the pattern is not clear for c2 and greens (no linear relationship)

ggplot(current_df, aes(current_df$change_ind, c2)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)
#there seems to be a slightly positive relationship between c2 and change for Independents
#this relationship seems to be weak

ggplot(current_df, aes(current_df$change_lab, c2)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)
#the pattern is not clear for c2 and change for labour

ggplot(current_df, aes(current_df$change_ld, c2)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)
#the pattern is not clear for c2 and change for Libdem


ggplot(current_df, aes(current_df$change_ukip, c2)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)
#the pattern is not clear for c2 and change for UKIP

#******************************************************************************
#---scatterplots for examining relationship between social grade de & loss/gain
#*******************************************************************************

ggplot(current_df, aes(current_df$change_con, de)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)
#there seem to be a positive linear relationship between proportion for de and gain for Cons 
#they gained most in areas with more than 20% of people in social grade de

ggplot(current_df, aes(current_df$change_ind, de)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)
#the pattern is not clear for de and change for Independent candidates

ggplot(current_df, aes(current_df$change_green, de)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)
#the pattern is not clear for de and change for Greens

ggplot(current_df, aes(current_df$change_lab, de)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)
#there seems to be an inverse relationship for de and change for labour 
#they gained more in areas with less than 30% people in social grade de
#*and lost most in areas with 30% to 40% of people in social grade de

ggplot(current_df, aes(current_df$change_ld, de)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)
#the relationship between de and change for Libdem seems to be inverse
#*they gained most in areas with less than 30% of people in grade de

ggplot(current_df, aes(current_df$change_ukip, de)) +
  geom_point()+
  geom_smooth(method = "lm", se = FALSE)
#there is no clear pattern for de and change for UKIP

#******************************************************************************
#---scatterplots for examining relationship between leave votes & loss/gain
#*******************************************************************************

leave1 <- ggplot(current_df, aes(current_df$change_con, leave)) +
  geom_point() +
  geom_smooth(method = "lm") +  
  labs(x="Proportion Change for Conservative", y="Leave EU") +
  theme(
    axis.title.x = element_text(color = "blue", size = 14, face = "bold"),
    axis.title.y = element_text(color = "red", size = 14, face = "bold")
  )
##there seems to be a moderate positve relationship between leave votes and change for Cons
#Cons suffered less loss in areas with high proportion of leave votes
#their gains came from areas with more than 50% leave voters, although not much

leave2 <- ggplot(current_df, aes(current_df$change_lab, leave)) +
  geom_point() +
  geom_smooth(method = "lm")  +  
  labs(x="Proportion Change for Labour", y="Leave EU") +
  theme(
    axis.title.x = element_text(color = "blue", size = 14, face = "bold"),
    axis.title.y = element_text(color = "red", size = 14, face = "bold")
  )
#the pattern seems random for leave votes and change for labour
#indication of a negative relationship perharps

leave3 <- ggplot(current_df, aes(current_df$change_ld, leave)) +
  geom_point() +
  geom_smooth(method = "lm")  +  
  labs(x="Proportion Change for Liberal Democrats", y="Leave EU") +
  theme(
    axis.title.x = element_text(color = "blue", size = 14, face = "bold"),
    axis.title.y = element_text(color = "red", size = 14, face = "bold")
  )
#there is a slight indication of an inverse relationship for leave votes and change for Libdem
#more gains came from areas with less than 60% of leave voters

leave4 <- ggplot(current_df, aes(current_df$change_ind, leave)) +
  geom_point() +
  geom_smooth(method = "lm") +  
  labs(x="Proportion Change for Independents", y="Leave EU") +
  theme(
    axis.title.x = element_text(color = "blue", size = 14, face = "bold"),
    axis.title.y = element_text(color = "red", size = 14, face = "bold")
  )
#there is a slight indication of a positive relationship for Greens and leave votes
#more gains came from areas with high proportion of leave voters


leave5 <- ggplot(current_df, aes(current_df$change_green, leave)) +
  geom_point()+
  geom_smooth(method = "lm") +  
  labs(x="Proportion Change for Green", y="Leave EU") +
  theme(
    axis.title.x = element_text(color = "blue", size = 14, face = "bold"),
    axis.title.y = element_text(color = "red", size = 14, face = "bold")
  )
#the pattern doesn't seem to be clear for leave and change for Greens
#there is a slight indication of an inverse relationship
#more of the gains came from areas with low proportion of leave votes 


grid.arrange(leave1, leave2, leave3, leave4, leave5, ncol=2, nrow=3)



#******************************************************************************************************
#---scatterplots for examining popularity in 2017 general election vs loss/gain in 2019 for each party
#******************************************************************************************************

ggplot(current_df, aes(current_df$change_con, con_vote)) +
  geom_point() +
  geom_smooth(method = "lm")
#conservatives seem to lose most in areas which they had high popularity in 2017

ggplot(current_df, aes(current_df$change_green, green_vote)) +
  geom_point() +
  geom_smooth(method = "lm")
#the pattern is not clear for greens

ggplot(current_df, aes(current_df$change_ld, ld_vote)) +
  geom_point() +
  geom_smooth(method = "lm")
#the pattern is not clear for liberal democrats

ggplot(current_df, aes(current_df$change_lab, lab_vote)) +
  geom_point() +
  geom_smooth(method = "lm")
#labour seem to lose most in areas where they were popular in 2017
#similar pattern observed for Conservatives, inverse linear relationship


#******************************************************************************************************
#---HISTOGRAMS FOR CHANGE VARIabLE TO CHECK FOR NORMALITY
#******************************************************************************************************

ggplot(data = current_df, mapping = aes(x = current_df$change_con)) +
  geom_histogram(color = "white", fill = "steelblue") 
#histogram is roughly symmetric

ggplot(data = current_df, mapping = aes(x = current_df$change_green)) +
  geom_histogram(color = "white", fill = "steelblue")
#histogram is skewed to the right

ggplot(data = current_df, mapping = aes(x = current_df$change_ind)) +
  geom_histogram(color = "white", fill = "steelblue")
#histogram is skewed to the right

ggplot(data = current_df, mapping = aes(x = current_df$change_lab)) +
  geom_histogram(color = "white", fill = "steelblue")
#histogram is roughly symmetric

ggplot(data = current_df, mapping = aes(x = current_df$change_ld)) +
  geom_histogram(color = "white", fill = "steelblue")
#histogram is skewed to the right

ggplot(data = current_df, mapping = aes(x = current_df$change_ukip)) +
  geom_histogram(color = "white", fill = "steelblue", binwidth = 10)
#histogram is skewed to the left


#******************************************************************************************************
#---HISTOGRAMS FOR CHANGE VARIabLE BY region
#******************************************************************************************************

ggplot(data = current_df, mapping = aes(x = current_df$change_con)) +
  geom_histogram(color = "white", fill = "steelblue") +
  facet_wrap(~ region)
#roughly normal distribution for CONS in each region, west and yorkshire are of relatively smaller sizes compared to others, so no surprises

ggplot(data = current_df, mapping = aes(x = current_df$change_green)) +
  geom_histogram(color = "white", fill = "steelblue") +
  facet_wrap(~ region)
#same right skewness observed for each region for the GREENS

ggplot(data = current_df, mapping = aes(x = current_df$change_ind)) +
  geom_histogram(color = "white", fill = "steelblue") +
  facet_wrap(~ region)
#same right skewness observed for each region, roughly normal in Yorkshire and the Humber for Independents

ggplot(data = current_df, mapping = aes(x = current_df$change_lab)) +
  geom_histogram(color = "white", fill = "steelblue") +
  facet_wrap(~ region)
#roughly normal distribution observed in each region for LabOURS

ggplot(data = current_df, mapping = aes(x = current_df$change_ld)) +
  geom_histogram(color = "white", fill = "steelblue") +
  facet_wrap(~ region)
#right skewness observed for each region for LIBdeM

ggplot(data = current_df, mapping = aes(x = current_df$change_ukip)) +
  geom_histogram(color = "white", fill = "steelblue") +
  facet_wrap(~ region)
#left skewness observed for UKIP


#******************************************************************************************************
#---CORRELATION MATRIX FOR LEAVE AND CHANGE FOR ALL PARTIES
#******************************************************************************************************

current_df %>%
  select(leave, change_ind, change_con, change_lab, change_ukip, change_ld, change_green) %>% 
  cor()
#the 2016 EU leave vote is positively correlated with change for Indp. and Conservative, although this is weak
#Negatively correlated with change for Labour, UKIP, LibDEM and Green


###SOCIAL GRADE INFLUENCE ON CONSERVATIVE LOSS

sc1 <- ggplot(conservative_frame, aes(x=change_con, y=ab)) +
  geom_point() +
  geom_smooth(method = "lm") + 
  labs(x="Proportion Change for Conservative", y="Social Grade AB") +
  theme(
    axis.title.x = element_text(color = "blue", size = 14, face = "bold"),
    axis.title.y = element_text(color = "red", size = 14, face = "bold")
  )


sc2 <- ggplot(conservative_frame, aes(x=change_con, y=c1)) +
  geom_point() +
  geom_smooth(method = "lm") + 
  labs(x="Proportion Change for Conservative", y="Social Grade C1") +
  theme(
    axis.title.x = element_text(color = "blue", size = 14, face = "bold"),
    axis.title.y = element_text(color = "red", size = 14, face = "bold")
  )

sc3 <- ggplot(conservative_frame, aes(x=change_con, y=c2)) +
  geom_point() +
  geom_smooth(method = "lm") + 
  labs(x="Proportion Change for Conservative", y="Social Grade C2") +
  theme(
    axis.title.x = element_text(color = "blue", size = 14, face = "bold"),
    axis.title.y = element_text(color = "red", size = 14, face = "bold")
  )

sc4 <- ggplot(conservative_frame, aes(x=change_con, y=de)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(x="Proportion Change for Conservative", y="Social Grade DE") +
  theme(
    axis.title.x = element_text(color = "blue", size = 14, face = "bold"),
    axis.title.y = element_text(color = "red", size = 14, face = "bold")
  )

library(gridExtra)

grid.arrange(sc1, sc3, sc2, sc4, ncol=2, nrow=2)


#SOCIAL GRADE INFLUENCE ON LABOUR
lb1 <- ggplot(current_df, aes(x=change_lab, y=ab)) +
  geom_point() +
  geom_smooth(method = "lm") + 
  labs(x="Proportion Change for Labour", y="Social Grade AB") +
  theme(
    axis.title.x = element_text(color = "blue", size = 14, face = "bold"),
    axis.title.y = element_text(color = "red", size = 14, face = "bold")
  )

lb2 <- ggplot(current_df, aes(x=change_lab, y=c1)) +
  geom_point() +
  geom_smooth(method = "lm") + 
  labs(x="Proportion Change for Labour", y="Social Grade C1") +
  theme(
    axis.title.x = element_text(color = "blue", size = 14, face = "bold"),
    axis.title.y = element_text(color = "red", size = 14, face = "bold")
  )

lb3 <- ggplot(current_df, aes(x=change_lab, y=c2)) +
  geom_point() +
  geom_smooth(method = "lm") + 
  labs(x="Proportion Change for Labour", y="Social Grade C2") +
  theme(
    axis.title.x = element_text(color = "blue", size = 14, face = "bold"),
    axis.title.y = element_text(color = "red", size = 14, face = "bold")
  )

lb4 <- ggplot(current_df, aes(x=change_lab, y=de)) +
  geom_point() +
  geom_smooth(method = "lm") + 
  labs(x="Proportion Change for Labour", y="Social Grade DE") +
  theme(
    axis.title.x = element_text(color = "blue", size = 14, face = "bold"),
    axis.title.y = element_text(color = "red", size = 14, face = "bold")
  )

grid.arrange(lb1, lb3, lb2, lb4, ncol=2, nrow=2)

# -------------------------------------------------------------------------
#CHECKING FOR COLLINEARITY WITH PAIRS PLOT

#color coded correlation matrix
M <- cor(conservative_frame[,-1])
corrplot(M, method="circle")

#Finding how many correlations are bigger than 0.70
k = 0
for(i in 1:11){
  for(r in 1:11){
    if(M[i,r]> 0.70 & i != r){
      k= k + 1
    }
  }  }
print(k/2)


pairs(covariate_frame)
#checking for multicollinearity among numeric covariates
ggpairs(covariate_frame,lower = list(continuous = wrap(ggally_points, size = .3)))
cor(covariate_frame)

#*CONSERVATIVES
ggpairs(conservative_frame[,-1],lower = list(continuous = wrap(ggally_points, size = .3)))
cor(conservative_frame[,-1])
#multicollinearity observed, response has a non-linear relationships with some covariates

#*LABOUR
ggpairs(labour_frame[,-1],lower = list(continuous = wrap(ggally_points, size = .3)))
#multicollinearity observed, response has a non-linear relationships with some covariates

#*LIBDEM
ggpairs(ld_frame[,-1],lower = list(continuous = wrap(ggally_points, size = .3)))
#multicollinearity observed, response has a non-linear relationships with some covariates

#*INDEPENDENTS
ggpairs(ind_frame[,-1],lower = list(continuous = wrap(ggally_points, size = .3)))
#multicollinearity observed, response has a non-linear relationships with some covariates

#*GREENS
ggpairs(green_frame[,-1],lower = list(continuous = wrap(ggally_points, size = .3)))
#multicollinearity observed, response has a non-linear relationships with some covariates


#CREATING SEPARATE DATA FRAMES FOR EACH PARTY 
conservative_frame <- current_df %>% select(-la_name, -control, -seats_available, -total_seats, -change_green, -change_ind, -change_lab, -change_ld, -change_ukip)
labour_frame <- current_df %>% select(-la_name, -control, -seats_available, -total_seats, -change_green, -change_ind, -change_con, -change_ld, -change_ukip)
green_frame <- current_df %>% select(-la_name, -control, -seats_available, -total_seats, -change_con, -change_ind, -change_lab, -change_ld, -change_ukip)
ld_frame <- current_df %>% select(-la_name, -control, -seats_available, -total_seats, -change_green, -change_ind, -change_lab, -change_con, -change_ukip)
ind_frame <- current_df %>% select(-la_name, -control, -seats_available, -total_seats, -change_green, -change_con, -change_lab, -change_ld, -change_ukip)

#CREATING A LOGIT FRAME FOR EACH PARTY FOR LOGISTIC REGRESSION MODELLING
#-----CONSERVATIVE LOGIT (a proportion loss more than 15% loss is considered large for Conservatives)
conservative_logit <- conservative_frame
conservative_logit$change_con <- ifelse(conservative_logit$change_con<(-0.15), 1, 0)
conservative_logit$change_con <- as.factor(conservative_logit$change_con)
levels(conservative_logit$change_con)[1] <- "not a large loss"
levels(conservative_logit$change_con)[2] <- "large loss"
conservative_logit[,2:11] <- conservative_logit[,2:11] * 100 #scaling covariates to %


#---LABOUR LOGIT (a proportion loss below 0% is considered large for Labour)
labour_logit <- labour_frame
labour_logit$change_lab <- ifelse(labour_logit$change_lab<0, 1, 0)
labour_logit$change_lab <- as.factor(labour_logit$change_lab)
levels(labour_logit$change_lab)[1] <- "not a large loss"
levels(labour_logit$change_lab)[2] <- "large loss"
labour_logit[,2:11] <- labour_logit[,2:11] * 100 #scaling covariates to %

#---INDEPENDENT LOGIT (a proportion gain above 4% is considered large for independents)
ind_logit <- ind_frame
ind_logit$change_ind <- ifelse(ind_logit$change_ind>0.04, 1, 0)
ind_logit$change_ind <- as.factor(ind_logit$change_ind)
levels(ind_logit$change_ind)[1] <- "not a large win"
levels(ind_logit$change_ind)[2] <- "large win"
ind_logit[,2:11] <- ind_logit[,2:11] * 100 #scaling covariates to %

#--LIBDEM LOGIT (a proportion gain above 5% is considered large for LibDem)
ld_logit <- ld_frame
ld_logit$change_ld <- ifelse(ld_logit$change_ld>0.05, 1, 0)
ld_logit$change_ld <- as.factor(ld_logit$change_ld)
levels(ld_logit$change_ld)[1] <- "not a large win"
levels(ld_logit$change_ld)[2] <- "large win"
ld_logit[,2:11] <- ld_logit[,2:11] * 100 #scaling covariates to %


#--GREEN LOGIT (a proportion gain above 1% is considered large for Greens)
green_logit <- green_frame
green_logit$change_green <- ifelse(green_logit$change_green>0.01, 1, 0)
green_logit$change_green <- as.factor(green_logit$change_green)
levels(green_logit$change_green)[1] <- "not a large win"
levels(green_logit$change_green)[2] <- "large win"
green_logit[,2:11] <- green_logit[,2:11] * 100 #scaling covariates to %

#*******************************************************
#-LOGISTIC REGRESSION FOR CONSERVATIVE
#*******************************************************

con_plot1 <- ggplot(conservative_logit, aes(change_con,fill = change_con))+
  geom_bar( position = position_dodge())

con_plot2 <- ggplot(conservative_logit, aes(region,fill = change_con))+
  geom_bar( position = position_dodge())

con_plot3 <- ggplot(conservative_logit, aes(ab,fill = change_con))+
  geom_density( position = position_dodge(), alpha=0.5)

con_plot4 <- ggplot(conservative_logit, aes(c1,fill = change_con))+
  geom_density( position = position_dodge(), alpha=0.5)

con_plot5 <- ggplot(conservative_logit, aes(c2,fill = change_con))+
  geom_density( position = position_dodge(), alpha=0.5)

con_plot6 <- ggplot(conservative_logit, aes(de,fill = change_con))+
  geom_density( position = position_dodge(), alpha=0.5)

con_plot7 <- ggplot(conservative_logit, aes(con_vote,fill = change_con))+
  geom_density( position = position_dodge(), alpha=0.5)

con_plot8 <- ggplot(conservative_logit, aes(lab_vote,fill = change_con))+
  geom_density( position = position_dodge(), alpha=0.5)

con_plot9 <- ggplot(conservative_logit, aes(ld_vote,fill = change_con))+
  geom_density( position = position_dodge(), alpha=0.5)

con_plot10 <- ggplot(conservative_logit, aes(ukip_vote,fill = change_con))+
  geom_density( position = position_dodge(), alpha=0.5)

con_plot11 <- ggplot(conservative_logit, aes(green_vote,fill = change_con))+
  geom_density( position = position_dodge(), alpha=0.5)

con_plot12 <- ggplot(conservative_logit, aes(leave,fill = change_con))+
  geom_density( position = position_dodge(), alpha=0.5)

grid.arrange(con_plot1,con_plot2,con_plot3,con_plot4,con_plot5,con_plot6,con_plot7,con_plot8,con_plot9,con_plot10,con_plot11,con_plot12, ncol=3, nrow=4)

#density plot suggest c2 is not of clinical importance we should drop c2 & ukip

#univariate logistic regression to select variables
summary(glm(formula = change_con ~ ab , family = binomial(link = "logit"), 
               data = conservative_logit))
summary(glm(formula = change_con ~ c1 , family = binomial(link = "logit"), 
            data = conservative_logit))
summary(glm(formula = change_con ~ c2 , family = binomial(link = "logit"), 
            data = conservative_logit)) #not important
summary(glm(formula = change_con ~ de , family = binomial(link = "logit"), 
            data = conservative_logit))
summary(glm(formula = change_con ~ con_vote , family = binomial(link = "logit"), 
            data = conservative_logit))
summary(glm(formula = change_con ~ lab_vote , family = binomial(link = "logit"), 
            data = conservative_logit))
summary(glm(formula = change_con ~ ld_vote , family = binomial(link = "logit"), 
            data = conservative_logit))
summary(glm(formula = change_con ~ green_vote , family = binomial(link = "logit"), 
            data = conservative_logit))
summary(glm(formula = change_con ~ ukip_vote , family = binomial(link = "logit"), 
            data = conservative_logit))
summary(glm(formula = change_con ~ leave , family = binomial(link = "logit"), 
            data = conservative_logit))
#c2 should be dropped


#logistic regression on conservative
glm_con <- glm(formula = change_con ~ . -c2 -ukip_vote , family = binomial(link = "logit"), 
               data = conservative_logit)
summary(glm_con)
step(glm_con, direction = "backward")
model_con <- glm(formula = change_con ~ ab + con_vote + ld_vote + leave, 
              family = binomial(link = "logit"), data = conservative_logit) #final model for conservative
summary(model_con)




#*******************************************************
#-LOGISTIC REGRESSION FOR LABOUR
#*******************************************************

lab_plot1 <- ggplot(labour_logit, aes(change_lab,fill = change_lab))+
  geom_bar( position = position_dodge())

lab_plot2 <- ggplot(labour_logit, aes(region,fill = change_lab))+
  geom_bar( position = position_dodge())

lab_plot3 <- ggplot(labour_logit, aes(ab,fill = change_lab))+
  geom_density( position = position_dodge(), alpha=0.5)

lab_plot4 <- ggplot(labour_logit, aes(c1,fill = change_lab))+
  geom_density( position = position_dodge(), alpha=0.5)

lab_plot5 <- ggplot(labour_logit, aes(c2,fill = change_lab))+
  geom_density( position = position_dodge(), alpha=0.5)

lab_plot6 <- ggplot(labour_logit, aes(de,fill = change_lab))+
  geom_density( position = position_dodge(), alpha=0.5)

lab_plot7 <- ggplot(labour_logit, aes(con_vote,fill = change_lab))+
  geom_density( position = position_dodge(), alpha=0.5)

lab_plot8 <- ggplot(labour_logit, aes(lab_vote,fill = change_lab))+
  geom_density( position = position_dodge(), alpha=0.5)

lab_plot9 <- ggplot(labour_logit, aes(ld_vote,fill = change_lab))+
  geom_density( position = position_dodge(), alpha=0.5)

lab_plot10 <- ggplot(labour_logit, aes(ukip_vote,fill = change_lab))+
  geom_density( position = position_dodge(), alpha=0.5)

lab_plot11 <- ggplot(labour_logit, aes(green_vote,fill = change_lab))+
  geom_density( position = position_dodge(), alpha=0.5)

lab_plot12 <- ggplot(labour_logit, aes(leave,fill = change_lab))+
  geom_density( position = position_dodge(), alpha=0.5)

grid.arrange(lab_plot1,lab_plot2,lab_plot3,lab_plot4,lab_plot5,lab_plot6,lab_plot7,lab_plot8,lab_plot9,lab_plot10,lab_plot11,lab_plot12, ncol=3, nrow=4)

#density plot suggest c2 is not of clinical importance we should drop c2 & ukip

#univariate logistic regression to select variables
summary(glm(formula = change_lab ~ ab , family = binomial(link = "logit"), 
            data = labour_logit))
summary(glm(formula = change_lab ~ c1 , family = binomial(link = "logit"), 
            data = labour_logit))
summary(glm(formula = change_lab ~ c2 , family = binomial(link = "logit"), 
            data = labour_logit)) #not important
summary(glm(formula = change_lab ~ de , family = binomial(link = "logit"), 
            data = labour_logit))
summary(glm(formula = change_lab ~ con_vote , family = binomial(link = "logit"), 
            data = labour_logit))
summary(glm(formula = change_lab ~ lab_vote , family = binomial(link = "logit"), 
            data = labour_logit))
summary(glm(formula = change_lab ~ ld_vote , family = binomial(link = "logit"), 
            data = labour_logit))
summary(glm(formula = change_lab ~ green_vote , family = binomial(link = "logit"), 
            data = labour_logit)) #not important
summary(glm(formula = change_lab ~ ukip_vote , family = binomial(link = "logit"), 
            data = labour_logit)) #not important
summary(glm(formula = change_lab ~ leave , family = binomial(link = "logit"), 
            data = labour_logit))

#c2, ukip_vote, green_vote should be dropped


#logistic regression on labour
glm_lab <- glm(formula = change_lab ~. -c2 -ukip_vote -green_vote  , family = binomial(link = "logit"), 
               data = labour_logit)
summary(glm_lab)
step(glm_lab, direction = "backward")
model_lab <- glm(formula = change_lab ~ region + lab_vote, family = binomial(link = "logit"), 
                 data = labour_logit) #final model for labour
summary(model_lab)
exp(cbind(coef(model_lab), confint(model_lab))) 


#*******************************************************
#-LOGISTIC REGRESSION FOR LIBERAL DEMOCRATS
#*******************************************************

ld_plot1 <- ggplot(ld_logit, aes(change_ld,fill = change_ld))+
  geom_bar( position = position_dodge())

ld_plot2 <- ggplot(ld_logit, aes(region,fill = change_ld))+
  geom_bar( position = position_dodge())

ld_plot3 <- ggplot(ld_logit, aes(ab,fill = change_ld))+
  geom_density( position = position_dodge(), alpha=0.5)

ld_plot4 <- ggplot(ld_logit, aes(c1,fill = change_ld))+
  geom_density( position = position_dodge(), alpha=0.5)

ld_plot5 <- ggplot(ld_logit, aes(c2,fill = change_ld))+
  geom_density( position = position_dodge(), alpha=0.5)

ld_plot6 <- ggplot(ld_logit, aes(de,fill = change_ld))+
  geom_density( position = position_dodge(), alpha=0.5)

ld_plot7 <- ggplot(ld_logit, aes(con_vote,fill = change_ld))+
  geom_density( position = position_dodge(), alpha=0.5)

ld_plot8 <- ggplot(ld_logit, aes(lab_vote,fill = change_ld))+
  geom_density( position = position_dodge(), alpha=0.5)

ld_plot9 <- ggplot(ld_logit, aes(ld_vote,fill = change_ld))+
  geom_density( position = position_dodge(), alpha=0.5)

ld_plot10 <- ggplot(ld_logit, aes(ukip_vote,fill = change_ld))+
  geom_density( position = position_dodge(), alpha=0.5)

ld_plot11 <- ggplot(ld_logit, aes(green_vote,fill = change_ld))+
  geom_density( position = position_dodge(), alpha=0.5)

ld_plot12 <- ggplot(ld_logit, aes(leave,fill = change_ld))+
  geom_density( position = position_dodge(), alpha=0.5)

grid.arrange(ld_plot1,ld_plot2,ld_plot3,ld_plot4,ld_plot5,ld_plot6,ld_plot7,ld_plot8,ld_plot9,ld_plot10,ld_plot11,ld_plot12, ncol=3, nrow=4)

#density plot suggests all covariates are of clinical importance except c2

#univariate logistic regression to select variables
summary(glm(formula = change_ld ~ ab , family = binomial(link = "logit"), 
            data = ld_logit))
summary(glm(formula = change_ld ~ c1 , family = binomial(link = "logit"), 
            data = ld_logit))
summary(glm(formula = change_ld ~ c2 , family = binomial(link = "logit"), 
            data = ld_logit)) 
summary(glm(formula = change_ld ~ de , family = binomial(link = "logit"), 
            data = ld_logit))
summary(glm(formula = change_ld ~ con_vote , family = binomial(link = "logit"), 
            data = ld_logit))
summary(glm(formula = change_ld ~ lab_vote , family = binomial(link = "logit"), 
            data = ld_logit))
summary(glm(formula = change_ld ~ ld_vote , family = binomial(link = "logit"), 
            data = ld_logit))
summary(glm(formula = change_ld ~ green_vote , family = binomial(link = "logit"), 
            data = ld_logit)) 
summary(glm(formula = change_ld ~ ukip_vote , family = binomial(link = "logit"), 
            data = ld_logit)) 
summary(glm(formula = change_ld ~ leave , family = binomial(link = "logit"), 
            data = ld_logit)) 

#univariate analysis suggest all variables are of clinical importance for LibDem


#logistic regression on libdem
glm_ld <- glm(formula = change_ld ~. , family = binomial(link = "logit"), 
               data = ld_logit)
summary(glm_ld)
step(glm_ld, direction = "backward")
model_ld <- glm(formula = change_ld ~ con_vote + ld_vote + leave, family = binomial(link = "logit"), 
                data = ld_logit) #final model for liberal democrats
summary(model_ld)
exp(cbind(coef(model_ld), confint(model_ld))) 

#*******************************************************
#-LOGISTIC REGRESSION FOR INDEPENDENTS
#*******************************************************

ind_plot1 <- ggplot(ind_logit, aes(change_ind,fill = change_ind))+
  geom_bar( position = position_dodge())

ind_plot2 <- ggplot(ind_logit, aes(region,fill = change_ind))+
  geom_bar( position = position_dodge())

ind_plot3 <- ggplot(ind_logit, aes(ab,fill = change_ind))+
  geom_density( position = position_dodge(), alpha=0.5)

ind_plot4 <- ggplot(ind_logit, aes(c1,fill = change_ind))+
  geom_density( position = position_dodge(), alpha=0.5)

ind_plot5 <- ggplot(ind_logit, aes(c2,fill = change_ind))+
  geom_density( position = position_dodge(), alpha=0.5)

ind_plot6 <- ggplot(ind_logit, aes(de,fill = change_ind))+
  geom_density( position = position_dodge(), alpha=0.5)

ind_plot7 <- ggplot(ind_logit, aes(con_vote,fill = change_ind))+
  geom_density( position = position_dodge(), alpha=0.5)

ind_plot8 <- ggplot(ind_logit, aes(lab_vote,fill = change_ind))+
  geom_density( position = position_dodge(), alpha=0.5)

ind_plot9 <- ggplot(ind_logit, aes(ld_vote,fill = change_ind))+
  geom_density( position = position_dodge(), alpha=0.5)

ind_plot10 <- ggplot(ind_logit, aes(ukip_vote,fill = change_ind))+
  geom_density( position = position_dodge(), alpha=0.5)

ind_plot11 <- ggplot(ind_logit, aes(green_vote,fill = change_ind))+
  geom_density( position = position_dodge(), alpha=0.5)

ind_plot12 <- ggplot(ind_logit, aes(leave,fill = change_ind))+
  geom_density( position = position_dodge(), alpha=0.5)

grid.arrange(ind_plot1,ind_plot2,ind_plot3,ind_plot4,ind_plot5,ind_plot6,ind_plot7,ind_plot8,ind_plot9,ind_plot10,ind_plot11,ind_plot12, ncol=3, nrow=4)

#density plot suggests that there is no clear pattern of large wins in the regions for the independents
#plot suggest we screen out DE and green_vote

#univariate logistic regression to select variables
summary(glm(formula = change_ind ~ ab , family = binomial(link = "logit"), 
            data = ind_logit))#not important
summary(glm(formula = change_ind ~ c1 , family = binomial(link = "logit"), 
            data = ind_logit))#not important
summary(glm(formula = change_ind ~ c2 , family = binomial(link = "logit"), 
            data = ind_logit)) 
summary(glm(formula = change_ind ~ de , family = binomial(link = "logit"), 
            data = ind_logit)) #not important
summary(glm(formula = change_ind ~ con_vote , family = binomial(link = "logit"), 
            data = ind_logit))
summary(glm(formula = change_ind ~ lab_vote , family = binomial(link = "logit"), 
            data = ind_logit))
summary(glm(formula = change_ind ~ ld_vote , family = binomial(link = "logit"), 
            data = ind_logit)) #not_important
summary(glm(formula = change_ind ~ green_vote , family = binomial(link = "logit"), 
            data = ind_logit)) #not important
summary(glm(formula = change_ind ~ ukip_vote , family = binomial(link = "logit"), 
            data = ind_logit)) 
summary(glm(formula = change_ind ~ leave , family = binomial(link = "logit"), 
            data = ind_logit)) 

#univariate analysis suggest ab,c1,de,ld_vote,green_vote are not of clinical importance


#logistic regression on independents
glm_ind <- glm(formula = change_ind ~.-ab-c1-de-ld_vote-green_vote , family = binomial(link = "logit"), 
              data = ind_logit)
summary(glm_ind)
step(glm_ind, direction = "backward")
model_ind <- glm(formula = change_ind ~ region + lab_vote + ukip_vote, family = binomial(link = "logit"), 
                  data = ind_logit) #final model for independents
summary(model_ind)
exp(cbind(coef(model_ind), confint(model_ind))) 


#*******************************************************
#-LOGISTIC REGRESSION FOR GREENS
#*******************************************************

green_plot1 <- ggplot(green_logit, aes(change_green,fill = change_green))+
  geom_bar( position = position_dodge())

green_plot2 <- ggplot(green_logit, aes(region,fill = change_green))+
  geom_bar( position = position_dodge())

green_plot3 <- ggplot(green_logit, aes(ab,fill = change_green))+
  geom_density( position = position_dodge(), alpha=0.5)

green_plot4 <- ggplot(green_logit, aes(c1,fill = change_green))+
  geom_density( position = position_dodge(), alpha=0.5)

green_plot5 <- ggplot(green_logit, aes(c2,fill = change_green))+
  geom_density( position = position_dodge(), alpha=0.5)

green_plot6 <- ggplot(green_logit, aes(de,fill = change_green))+
  geom_density( position = position_dodge(), alpha=0.5)

green_plot7 <- ggplot(green_logit, aes(con_vote,fill = change_green))+
  geom_density( position = position_dodge(), alpha=0.5)

green_plot8 <- ggplot(green_logit, aes(lab_vote,fill = change_green))+
  geom_density( position = position_dodge(), alpha=0.5)

green_plot9 <- ggplot(green_logit, aes(ld_vote,fill = change_green))+
  geom_density( position = position_dodge(), alpha=0.5)

green_plot10 <- ggplot(green_logit, aes(ukip_vote,fill = change_green))+
  geom_density( position = position_dodge(), alpha=0.5)

green_plot11 <- ggplot(green_logit, aes(green_vote,fill = change_green))+
  geom_density( position = position_dodge(), alpha=0.5)

green_plot12 <- ggplot(green_logit, aes(leave,fill = change_green))+
  geom_density( position = position_dodge(), alpha=0.5)

grid.arrange(green_plot1,green_plot2,green_plot3,green_plot4,green_plot5,green_plot6,green_plot7,green_plot8,green_plot9,green_plot10,green_plot11,green_plot12, ncol=3, nrow=4)

#plot suggest we screen out C2 and UKIP_vote, LD_VOTE

#univariate logistic regression to select variables
summary(glm(formula = change_green ~ ab , family = binomial(link = "logit"), 
            data = green_logit))
summary(glm(formula = change_green ~ c1 , family = binomial(link = "logit"), 
            data = green_logit)) #not important
summary(glm(formula = change_green ~ c2 , family = binomial(link = "logit"), 
            data = green_logit)) #not important
summary(glm(formula = change_green ~ de , family = binomial(link = "logit"), 
            data = green_logit)) 
summary(glm(formula = change_green ~ con_vote , family = binomial(link = "logit"), 
            data = green_logit))
summary(glm(formula = change_green ~ lab_vote , family = binomial(link = "logit"), 
            data = green_logit))
summary(glm(formula = change_green ~ ld_vote , family = binomial(link = "logit"), 
            data = green_logit)) 
summary(glm(formula = change_green ~ green_vote , family = binomial(link = "logit"), 
            data = green_logit)) 
summary(glm(formula = change_green ~ ukip_vote , family = binomial(link = "logit"), 
            data = green_logit)) 
summary(glm(formula = change_green ~ leave , family = binomial(link = "logit"), 
            data = green_logit)) 

#univariate analysis suggest we should drop C1 & C2


#logistic regression on greens
glm_green <- glm(formula = change_green ~.-c1 -c2 , family = binomial(link = "logit"), 
               data = green_logit)
summary(glm_green)
step(glm_green, direction = "backward")
model_green <-  glm(formula = change_green ~ con_vote + green_vote, family = binomial(link = "logit"), 
                    data = green_logit) #final model for greens
summary(model_green)
exp(cbind(coef(model_green), confint(model_green))) 


#PERFORMING HOSMER-LEMESHOW GOODNESS OF FIT TEST FOR LOGISTIC MODELS
hoslem.test(model_con$y, fitted(model_con), g=10)
hoslem.test(model_lab$y, fitted(model_lab), g=10)
hoslem.test(model_ld$y, fitted(model_ld), g=10)
hoslem.test(model_green$y, fitted(model_green), g=10)
hoslem.test(model_ind$y, fitted(model_ind), g=10)

##plotting ROC for the models
par(pty = "s")
par(mfrow=c(1,1))
r1 <- roc(conservative_logit$change_con, model_con$fitted.values, 
    plot=TRUE, legacy.axes=TRUE, percent = TRUE, xlab='False Positive Percentage', ylab="True Positive Percentage",
    col="blue", lwd=4, print.auc=TRUE, main="CONSERVATIVE MODEL") #excellent discrimination
r2 <- roc(labour_logit$change_lab, model_lab$fitted.values, plot=TRUE, legacy.axes=TRUE, percent = TRUE, xlab='False Positive Percentage', ylab="True Positive Percentage",
    col="blue", lwd=4, print.auc=TRUE, main="LABOUR MODEL") #excellent discrimination
r3 <- roc(ld_logit$change_ld, model_ld$fitted.values, plot=TRUE, legacy.axes=TRUE, percent = TRUE, xlab='False Positive Percentage', ylab="True Positive Percentage",
    col="blue", lwd=4, print.auc=TRUE, main="Liberal Democrat Model") #excellent discrimination
r4 <- roc(ind_logit$change_ind, model_ind$fitted.values, plot=TRUE, legacy.axes=TRUE, percent = TRUE, xlab='False Positive Percentage', ylab="True Positive Percentage",
    col="blue", lwd=4, print.auc=TRUE, main="Independents Model") #poor discrimination
r5 <- roc(green_logit$change_green, model_green$fitted.values, plot=TRUE, legacy.axes=TRUE, percent = TRUE, xlab='False Positive Percentage', ylab="True Positive Percentage",
    col="blue", lwd=4, print.auc=TRUE, main="Green Model") #poor discrimination

ggplot(data = NULL, mapping = aes(x=model_con$fitted.values, y=conservative_logit$change_con)) +
  geom_jitter() 




#SPATIAL ANALYSIS
#******************************************************************************************************
#---IMPORTING SHAPE FILE AND DRAWING MAPS FOR EXPLORATORY
#******************************************************************************************************
map <- st_read("Local_Authority_Districts_december_2017_Full_Extent_Boundaries_in_United_Kingdom_WGS84.shp")
map$lad17nm <- as.character(map$lad17nm)
colnames(map)[colnames(map)=="lad17nm"] <- "la_name"
map_and_data <- inner_join(map, current_df)
head(map_and_data)
##due to boundary changes, we have complete map information for 219 councils 

##PLOTTING MAPS WITH GGPLOT

theme_update(plot.title = element_text(hjust = 0.5)) #setting default alignment to center

ggplot(map_and_data) +
  geom_sf(aes(fill=seats_available)) +
  scale_fill_gradient(low="#56B1F7", high="#132B43")  +
  ggtitle("SEATS UP FOR ELECTION")               ###seats available

ggplot(map_and_data) +
  geom_sf(aes(fill=change_ld)) +
  scale_fill_gradient(low="#56B1F7", high="#132B43")  +
  ggtitle("LOSS/GAIN FOR CONSERVATIVES")      ###change for conservatives


##TEST
library(RColorBrewer)
ggplot(map_and_data) +
  geom_sf(aes(fill=change_con)) + 
  scale_fill_gradientn(colors = brewer.pal(n=9, name = "Blues"))


ggplot(map_and_data) +
  geom_sf(aes(fill=change_lab)) +
  scale_fill_gradient()  +
  ggtitle("LOSS/GAIN FOR LabOUR")      ###change for labour



##PLOTTING MAPS
theme_set(theme_bw())
ggplot(data = map_and_data) +
  geom_sf(aes(fill=change_con)) + 
  xlab("Longitude") + ylab("Latitude") +
  ggtitle("CHANGE FOR CONSERVATIVE") +
  scale_colour_manual(values=gs.pal(5))
gs.pal <- colorRampPalette(c("#FFFFFF","#000000"),bias=.1,space="rgb")

#viewing with tmap interactive function
tmap_tip()
tmap_mode("plot")

tm_shape(map_and_data) +
  tm_polygons("change_con", palette = "-Reds") +
  tm_symbols(col = "change_con", size = "change_con", legend.col.show = FALSE, legend.size.show = FALSE) + 
  tm_add_legend(type = "symbol", border.col = "grey40",
                col = tmaptools::get_brewer_pal("YlOrRd", 4, plot = FALSE), 
                size = sqrt(seq(1:4) / 4),
                labels = c("-80% to -60%", "-60% to -40%", "-40% t0 -20%", "-20% to 0%", "0% to +20%", "+20% to +40%"),
                title = "Conservative Change")

tm_shape(map_and_data) + 
  tm_bubbles("change_con", size.max = -0.8, scale = 2) + tm_legend(outside = TRUE, outside.position = "bottom")

tm1 <- tm_shape(map_and_data) + tm_polygons("change_con")
tm2 <- tm_shape(map_and_data) + tm_bubbles(size = "change_con", size.max = -0.8, scale = 2)

tmap_arrange(tm1, tm2)

tm3 <- tm_shape(map_and_data) + tm_polygons("leave")
tm4 <- tm_shape(map_and_data) + tm_bubbles(size = "leave", size.max = 0.9, scale = 1.5)

tmap_arrange(tm3, tm4)

tm5 <- tm_shape(map_and_data) + tm_polygons("change_ld")
tm6 <- tm_shape(map_and_data) + tm_bubbles(size = "change_ld", size.max = 0.8, scale = 2)

tmap_arrange(tm5, tm6)

tm7 <- tm_shape(map_and_data) + tm_polygons("change_lab")
tm8 <- tm_shape(map_and_data) + tm_bubbles(size = "change_lab", size.max = 0, scale = 2)

tmap_arrange(tm7, tm8)

 qtm(map_and_data, fill = "change_con", fill.pallete = "RdYlGn") +
  tm_symbols(col = "change_con", size = "change_con", legend.col.show = FALSE, legend.size.show = FALSE) + 
  tm_add_legend(type = "symbol", border.col = "grey40",
                col = tmaptools::get_brewer_pal("YlOrRd", 4, plot = FALSE), 
                size = sqrt(seq(1:4) / 4),
                labels = c("-80% to -60%", "-60% to -40%", "-40% t0 -20%", "-20% to 0%", "0% to +20%", "+20% to +40%"),
                title = "Conservative Change")

tm_shape(map_and_data) +
  tm_polygons("change_con", id = "la_name", pallete="Green")

ld_map <- tm_shape(map_and_data) +
  tm_polygons("change_ld", id = "la_name", pallete="Green")

ind_map <- tm_shape(map_and_data) +
  tm_polygons("change_ind", id = "la_name", pallete="Green")

green_map <- tm_shape(map_and_data) +
  tm_polygons("change_green", id = "la_name", pallete="Green")


grid.arrange(con_map, lab_map, ld_map, ind_map, green_map, ncol=2, nrow=3)


tm_shape(map_and_data) +
  tm_polygons("change_lab", id = "la_name", pallete="Green")
##plot indicates that labour biggest losses came from midlands and north

tm_shape(map_and_data) +
  tm_polygons("leave", id = "la_name", pallete="Red")
#one can see which areas voted most to leave the EU

tm_shape(map_and_data) +
  tm_polygons("ab", id = "la_name", pallete="Red")

tm_shape(map_and_data) +
  tm_polygons("de", id = "la_name", pallete="Red")

tm_shape(map_and_data) +
  tm_polygons("c2", id = "la_name", pallete="Red")

tmap_last()

#saving interactive map as html with tmap save
test_map <- tmap_last()
tmap_save(test_map, "test_map.html")




##PROBLEM!!!
##zeros are now meaning two things (either no loss/gain or no result available for party in that area)

##LIMITATIONS OF THE STUDY 
#Boundary changes in England makes interpretability and reliability of results a problem
#For example, the council where Conservatives lost the most is "Bournemout, Christchurch and Poole"
#according to the BBC, this local authority is new and is a merger of 3 councils. 
#BBC also stated that the council has never elected councillors before and the change figures was calculated based
#on predictions on what the results would have been if the council was in existence as at the time of the previous election.

#The loss/gain figures represented by the "change" variable could be an overestimation of loss/gain and an innacurate description of electorates change of support for parties
#as it was obtained by comparing 2019 elections to 2015, where some councils hold elections every year and the changes in those relations was not accounted for in this change variable


#using spdep package
shape <- readOGR(dsn = "Local_Authority_Districts_December_2017_Full_Extent_Boundaries_in_United_Kingdom_WGS84.shp")
shape@data$lad17nm <- as.character(shape@data$lad17nm)
sp.dat <- merge(shape, as.data.frame(current_df), all.x=FALSE, by.y="la_name", by.x="lad17nm")
W.nb <- poly2nb(sp.dat, row.names = rownames(sp.dat@data))

W.list <- nb2listw(W.nb, style = "B", zero.policy = TRUE)
W <- nb2mat(W.nb, style = "B", zero.policy = TRUE)

which(rowSums(W)==0)

centroids <- as.data.frame(getSpPPolygonsLabptSlots(sp.dat))
colnames(centroids) <- c("long", "lat")

centroids$zone67 <- sqrt(((centroids$lat-centroids$lat[67])^2) + ((centroids$long-centroids$long[1])^2))
centroids$zone13 <- sqrt(((centroids$lat-centroids$lat[13])^2) + ((centroids$long-centroids$long[13])^2))
centroids$zone39 <- sqrt(((centroids$lat-centroids$lat[39])^2) + ((centroids$long-centroids$long[39])^2))
centroids$zone49 <- sqrt(((centroids$lat-centroids$lat[49])^2) + ((centroids$long-centroids$long[49])^2))

centroids$zone1[1] <- Inf
centroids$zone13[13] <- Inf
centroids$zone39[39] <- Inf
centroids$zone49[49] <- Inf

centroids[which.min(centroids$zone1),] #zone2
centroids[which.min(centroids$zone13),]#zone138
centroids[which.min(centroids$zone39),]#zone40
centroids[which.min(centroids$zone49),]#zone48









con_moran <- conservative_logit
rownames(con_moran) <- current_df$la_name
con_moran <- con_moran[which(rownames(con_moran) %in% sp.dat@data$lad17nm),]
model <- glm(formula = change_con ~ ab + con_vote + ld_vote + leave, 
             family = binomial(link = "logit"), data = con_moran) #final model for conservative
summary(model)

con_test <- conservative_frame
rownames(con_test) <- current_df$la_name
con_test <- con_test[which(rownames(con_test) %in% sp.dat@data$lad17nm),]
test <- lm(change_con ~ con_vote + lab_vote + leave, data = con_test)
step(test, direction = "backward")
summary(test)

moran.mc(x = residuals(test), listw = W.list, nsim = 10000, zero.policy = TRUE)

lab_moran <- labour_logit
rownames(lab_moran) <- current_df$la_name
lab_moran <- lab_moran[which(rownames(lab_moran) %in% sp.dat@data$lad17nm),]
model2 <- glm(formula = change_lab ~ region + lab_vote, family = binomial(link = "logit"), 
                 data = lab_moran)
summary(model2)
moran.mc(x = residuals(model2), listw = W.list, nsim = 10000, zero.policy = TRUE)

ld_moran <- ld_logit
rownames(ld_moran) <- current_df$la_name
ld_moran <- ld_moran[which(rownames(ld_moran) %in% sp.dat@data$lad17nm),]
model3 <- glm(formula = change_ld ~ con_vote + ld_vote + leave, family = binomial(link = "logit"), 
                data = ld_moran)
summary(model3)
moran.mc(x = residuals(model3), listw = W.list, nsim = 10000, zero.policy = TRUE)

ind_moran <- ind_logit
rownames(ind_moran) <- current_df$la_name
ind_moran <- ind_moran[which(rownames(ind_moran) %in% sp.dat@data$lad17nm),]
model4 <- glm(formula = change_ind ~ region + lab_vote + ukip_vote, family = binomial(link = "logit"), 
                 data = ind_moran)
summary(model4)
moran.mc(x = residuals(model4), listw = W.list, nsim = 100000, zero.policy = TRUE)

#at first glance, there seem to be an indication of spatial autocorrelation as cluster of areas with similar range of values are observed
#but on conducting the moranI tests for the proposed models, there seem to be no spatial structure in the residuals after accounting for the covariate effects.

green_moran <- green_logit
rownames(green_moran) <- current_df$la_name
green_moran <- green_moran[which(rownames(green_moran) %in% sp.dat@data$lad17nm),]
model5 <-  glm(formula = change_green ~ con_vote + green_vote, family = binomial(link = "logit"), 
                    data = green_moran)
summary(model5)
moran.mc(x = residuals(model5), listw = W.list, nsim = 10000, zero.policy = TRUE)

library(ggplot2)
library(rgeos)
library(maptools)

sp.dat@data$objectid <- rownames(sp.dat@data)
temp1 <- fortify(sp.dat, region="objectid")
names(sp.dat@data)[names(sp.dat@data) == "objectid"] <- "id"
sp.dat2 <- merge(temp1, sp.dat@data, by="id")

ggplot(data = sp.dat2, aes(x=long.y, y=lat.y, goup=group, fill = change_con)) +
  geom_polygon()
