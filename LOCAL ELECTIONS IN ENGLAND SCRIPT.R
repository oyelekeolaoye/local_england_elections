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
#c2 should be dropped


#logistic regression on conservative
glm_con <- glm(formula = change_con ~. -c2 -ukip_vote , family = binomial(link = "logit"), 
               data = conservative_logit)
summary(glm_con)
step(glm_con, direction = "backward")
model1 <- glm(formula = change_con ~ ab + con_vote + ld_vote + leave, 
              family = binomial(link = "logit"), data = conservative_logit2) #final model for conservative
summary(model1)




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

#c2, ukip_vote, green_vote should be dropped


#logistic regression on labour
glm_lab <- glm(formula = change_lab ~. -c2 -ukip_vote -green_vote  , family = binomial(link = "logit"), 
               data = labour_logit)
summary(glm_lab)
step(glm_lab, direction = "backward")
model_lab <- glm(formula = change_con ~ ab + con_vote + ld_vote + leave, 
              family = binomial(link = "logit"), data = conservative_logit2) #final model for conservative
summary(model1)

