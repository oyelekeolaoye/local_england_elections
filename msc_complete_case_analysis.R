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

proportion_won <- data.frame(party=c("Conservative", "Labour", "LibDem", "Green", "UKIP", "Independents"),
                             value=c(42.4, 23.4, 16.5, 3.3, 0.4, 14))
ggplot(data=proportion_won, aes(x=value, fill=party)) +
  geom_histogram()

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


##CONVERTING CHANGE VARIABLE TO PROPORTION CHANGE
current_df$change_con <- current_df$change_con/current_df$seats_available
current_df$change_ind <- current_df$change_ind/current_df$seats_available
current_df$change_lab <- current_df$change_lab/current_df$seats_available
current_df$change_ld <- current_df$change_ld/current_df$seats_available
current_df$change_ukip <- current_df$change_ukip/current_df$seats_available
current_df$change_green <- current_df$change_green/current_df$seats_available

#EXPLORATORY DATA ANALYSIS
skim(current_df)
summary(current_df)
#on the average, 22.6% are in the AB social grade, 30.4% are in the C1, 22.3% in C2, 24.5% in DE
#Social Grades across areas shows symmetric distribution as median almost equals mean for each grade classification
#Statistics reveal that Conservatives had a maximum loss of 72.2% and a maximum gain of 31.2% of the seats that were up for election
#Labour had a maximum loss of 46.1% and a maximum gain of 45.4%
#The independents(including other minor parties) had a maximum loss of 7% and a maximum gain of 46.1%
#UKIP had a maximum loss of 58.9% and a maximum gain of 11.7%
#Green Party had a maximum loss of 6% and a maximum gain of 20%
#Liberal Democrat had a maximum loss of 9% and a maximum gain of 64.2%
#Average statistics reveal that Conservatives, Labour, and UKIP are the parties that suffered losses, the rest parties on an average made a gain.


table(current_df$control)

#******************************************************************************************************
#---BOXPLOTS FOR change variable FOR EACH PARTY
#******************************************************************************************************
ggplot(data = current_df, mapping = aes(y=current_df$change_con)) +
  geom_boxplot() +
  ggtitle("BOXPLOT OF LOSS/GAIN FOR CONSERVATIVES")
#boxplot indicates some outliers/normal distribution

ggplot(data = current_df, mapping = aes(y=current_df$change_green)) +
  geom_boxplot() + 
  ggtitle("BOXPLOT OF LOSS/GAIN FOR GREENS")
#boxplot indicates outliers/right skewness

ggplot(data = current_df, mapping = aes(y=current_df$change_ind)) +
  geom_boxplot() + 
  ggtitle("BOXPLOT OF LOSS/GAIN FOR INdePENdeNTS")
#boxplot indicates outlying values/right skewness

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
#---HISTOGRAMS FOR NUMERIC COVARIATES
#******************************************************************************************************

covariate_frame <- current_df %>% select(ab, c1, c2, de, con_vote, lab_vote, green_vote, ukip_vote,
                                         ld_vote, leave)

ggplot(gather(covariate_frame), aes(value)) + 
  geom_histogram(bins = 10) + 
  facet_wrap(~key, scales = 'free_x')


#******************************************************************************************************
#---BOXPLOT OF CONSERVARIVE CHANGE WITH COVARIATES
#******************************************************************************************************
par(mfrow = c(5,2))


#CREATING SEPARATE DATA FRAMES FOR EACH PARTY TO BEGIN MODELLING
conservative_frame <- current_df %>% select(-la_name, -control, -seats_available, -total_seats, -change_green, -change_ind, -change_lab, -change_ld, -change_ukip)
labour_frame <- current_df %>% select(-la_name, -control, -seats_available, -total_seats, -change_green, -change_ind, -change_con, -change_ld, -change_ukip)
green_frame <- current_df %>% select(-la_name, -control, -seats_available, -total_seats, -change_con, -change_ind, -change_lab, -change_ld, -change_ukip)
ld_frame <- current_df %>% select(-la_name, -control, -seats_available, -total_seats, -change_green, -change_ind, -change_lab, -change_con, -change_ukip)
ind_frame <- current_df %>% select(-la_name, -control, -seats_available, -total_seats, -change_green, -change_con, -change_lab, -change_ld, -change_ukip)
ukip_frame <- current_df %>% select(-la_name, -control, -seats_available, -total_seats, -change_green, -change_ind, -change_lab, -change_ld, -change_con)



# -------------------------------------------------------------------------
#CHECKING FOR COLLINEARITY WITH PAIRS PLOT

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



# -------------------------------------------------------------------------
#LINEAR REGRESSION FOR CONSERVATIVE
model_con <- lm(change_con ~ ., data = conservative_frame)
model.selection <- ols_step_best_subset(model_con)
model.selection   
plot(model.selection)
test_con <- lm(change_con ~ con_vote + lab_vote + leave, data = conservative_frame)
summary(test_con)
plot(test_con) #checking residual plots
#fit is not too bad
step(model_con) #stepwise selection
##stepwise selection matches 

#LINEAR REGRESSION FOR LABOUR
model_lab <- lm(change_lab ~ ., data = labour_frame)
model.selection_lab <- ols_step_best_subset(model_lab)
model.selection_lab  
plot(model.selection_lab) #model 7 is preferred
test_lab <- lm(change_lab ~ region + ab + de + con_vote + ukip_vote + green_vote + leave, data = labour_frame)
summary(test_lab)
plot(test_lab) #residual plot indicates lack of fit
step(model_lab) #stepwise selection does not match
lab_stepwise <- lm(formula = change_lab ~ region + ab + c1 + con_vote + ukip_vote + 
                     green_vote + leave, data = labour_frame)
summary(lab_stepwise)
plot(lab_stepwise)


#LINEAR REGRESSION FOR INDEPENDENT
model_ind <- lm(change_ind ~ ., data = ind_frame)
model.selection_ind <- ols_step_best_subset(model_ind)
model.selection_ind  
plot(model.selection_ind) #model 6 is preferred
test_ind <- lm(change_ind ~ region + ab + c1 + con_vote + lab_vote + ld_vote, data = ind_frame)
summary(test_ind)
plot(test_ind) #residual plot indicates lack of fit
step(model_ind) #stepwise selection agrees

#LINEAR REGRESSION FOR LIBDEM
model_ld <- lm(change_ld ~ ., data = ld_frame)
model.selection_ld <- ols_step_best_subset(model_ld)
model.selection_ld
plot(model.selection_ld) #model 2 is preferred
test_ld <- lm(change_ld ~  ab + ld_vote, data = ld_frame)
summary(test_ld)
plot(test_ld) #residual plot indicates lack of fit

step(model_ld)  #stepwise selection agrees

#stepwise GREEN
model_green <- lm(change_green ~ ., data = green_frame)
model.selection_green <- ols_step_best_subset(model_green)
model.selection_green
plot(model.selection_green) #model 4 is preferred
test_green <- lm(change_green ~ region + c2 + green_vote + leave, data = green_frame)
summary(test_green)
plot(test_ld) #residual plot indicates lack of fit

step(model_green) #stepwise selection does not agree
step_green <- lm(formula = change_green ~ region + ab + c2 + green_vote, data = green_frame)
summary(step_green)
plot(step_green)

#stepwise UKIP
model_ukip <- lm(change_ukip ~ ., data = ukip_frame)
model.selection_ukip <- ols_step_best_subset(model_ukip)
model.selection_ukip
plot(model.selection) #model 3 is preferred
test_ukip <- lm(change_ukip ~ region + ukip_vote + leave, data = ukip_frame)
summary(test_ukip)
plot(test_ukip) #residual plot indicates lack of fit

step(model_ukip) #stepwise selection does not agree
step_ukip <- lm(formula = change_ukip ~ ab + c1 + con_vote + ukip_vote + leave, 
                data = ukip_frame)
summary(step_ukip)
plot(step_ukip)


##FITTING A LOGISTIC REGRESSION TO THE DATA
#We set an arbitrary large loss to 15% for conservatives
conservative_logit <- conservative_frame
conservative_logit$change_con <- ifelse(conservative_logit$change_con<(-0.20), 1, 0)

#logistic regression on conservative
glm_con <- glm(formula = change_con ~ ., family = binomial(link = "logit"), 
            data = conservative_logit)
step(glm_con)

glm_step_con <- glm(formula = change_con ~ ab + c2 + lab_vote + ld_vote + ukip_vote + 
                      green_vote + leave, family = binomial(link = "logit"), data = conservative_logit) #model for 15%

glm_step_con <- glm(formula = change_con ~ c1 + c2 + con_vote + leave, family = binomial(link = "logit"), 
                    data = conservative_logit) #model for 5%

glm_step_con <- glm(formula = change_con ~ ab + con_vote + lab_vote + leave, family = binomial(link = "logit"), 
                    data = conservative_logit) #model for 20%

plot(glm_step_con)
summary(glm_step_con$fitted.values)
hist(glm_step_con$fitted.values)
