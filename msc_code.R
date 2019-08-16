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





#displaying numbers and avoiding scientific notation
options(scipen = 999)

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
merged_with_nas <- merged_dataset
merged_dataset[is.na(merged_dataset)] <- 0

#SAVING MERGED DATASET AS CSV FILE
write.csv(merged_dataset, "merged.csv")

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
#there are no brexit information for (10) of these areas, which implies that we have complete information on
#238 local areas.
names2 <- names(results_orig)
names2[!(names(results_orig)%in%brexit$LAName)] #areas for which there are no brexit information

sum(current_df$seats_available)/sum(current_df$total_seats)
#for the 238 areas for which we have complete information, 71.5% of the total seats were put up for election


#***************************************************
#PROPORTION OF SEATS WON BY EACH PARTY OF AVAILabLE
#***************************************************
sum(merged_dataset$elected.Conservative)/sum(current_df$seats_available)
#CONSERVATIVES took 41.9% of the seats available

sum(merged_dataset$elected.Labour)/sum(current_df$seats_available)
#LabOUR took 24.5% of the seats available

sum(merged_dataset$`elected.Liberal democrat`)/sum(current_df$seats_available)
#LIBdeM took 15.7% of the seats available

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


#******************************************************************************************************
#---CORRELATION MATRIX FOR SOCIAL GRADE AND CHANGE FOR ALL PARTIES
#******************************************************************************************************
current_df %>%
  select(change_ind, ab, c1, c2, de) %>% 
  cor()
#weak correlation observed for social grades and change for independents

current_df %>%
  select(change_con, ab, c1, c2, de) %>% 
  cor()
#ab has a moderate negative correlation with change for TORIES while de has a moderate positive correlation 

current_df %>%
  select(change_lab, ab, c1, c2, de) %>% 
  cor()
#weak correlation observed for social grades and change for lab
#only ab and c1 has positive correlations

current_df %>%
  select(change_ld, ab, c1, c2, de) %>% 
  cor()
#ab is positively correlated with change for LIBDEM while de has a negative correlation of rhe same magnitude

current_df %>%
  select(change_ukip, ab, c1, c2, de) %>% 
  cor()
#weak correlations observed for social grades and change for UKIP

current_df %>%
  select(change_green, ab, c1, c2, de) %>% 
  cor()
#weak correlations observed for social grades and change for GREENS



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
  geom_sf(aes(fill=change_con)) +
  scale_fill_gradient(low="#56B1F7", high="#132B43")  +
  ggtitle("LOSS/GAIN FOR CONSERVATIVES")      ###change for conservatives

ggplot(map_and_data) +
  geom_sf(aes(fill=change_lab)) +
  scale_fill_gradient()  +
  ggtitle("LOSS/GAIN FOR LabOUR")      ###change for labour



##PLOTTING MAPS WITH T.MAP 

#viewing with tmap interactive function
tmap_mode("view")

tm_shape(map_and_data) +
  tm_polygons("seats_available", id = "la_name", pallete="Green")

con_map <- tm_shape(map_and_data) +
            tm_polygons("change_con", id = "la_name", pallete="Green") ##interactive plot for conservative change
##plot indicates that the biggest losses came from the south

lab_map <- tm_shape(map_and_data) +
  tm_polygons("change_lab", id = "la_name", pallete="Green")

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
