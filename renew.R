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


par(mfrow=c(1,1))
plot(data=conservative_frame, leave~change_con) #conservatives had better chances in high leave areas
plot(data=conservative_frame, ab~change_con) #loss came from high proportion of social grade ab
plot(data=conservative_frame, c1~change_con) #loss came from high proportion of social grade c1
plot(data=conservative_frame, c2~change_con) 
plot(data=conservative_frame, de~change_con)
plot(data=conservative_frame, con_vote~change_con)
plot(data=conservative_frame, lab_vote~change_con)
plot(data=conservative_frame, ld_vote~change_con)
plot(data=conservative_frame, green_vote~change_con) #N
plot(data=conservative_frame, ukip_vote~change_con) 

table(conservative_frame$region, conservative_logit$change_con)


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

##FITTING A LOGISTIC REGRESSION TO THE DATA
#We set an arbitrary large loss to 20% for conservatives
conservative_logit <- conservative_frame
conservative_logit$change_con <- ifelse(conservative_logit$change_con<(-0.15), 1, 0)
sum(conservative_logit$change_con==1)

#logistic regression on conservative
glm_con <- glm(formula = change_con ~ ., family = binomial(link = "logit"), 
               data = conservative_logit)
summary(glm_con)
step(glm_con)
glm_step_con <- glm(formula = change_con ~ ab + con_vote + lab_vote + leave, family = binomial(link = "logit"), 
                    data = conservative_logit) #model for 20%
summary(glm_step_con)



plot(data=labour_frame, leave~change_lab) 
plot(data=labour_frame, ab~change_lab) 
plot(data=labour_frame, c1~change_lab) 
plot(data=labour_frame, c2~change_lab) 
plot(data=labour_frame, de~change_lab) 
plot(data=labour_frame, con_vote~change_lab) 
plot(data=labour_frame, lab_vote~change_lab) 
plot(data=labour_frame, ld_vote~change_lab) # N
plot(data=labour_frame, green_vote~change_lab) #N
plot(data=labour_frame, ukip_vote~change_lab) 

plot(data=ld_frame, leave~change_ld)
plot(data=ld_frame, ab~change_ld)
plot(data=ld_frame, c1~change_ld)
plot(data=ld_frame, c2~change_ld)
plot(data=ld_frame, de~change_ld)
plot(data=ld_frame, con_vote~change_ld)
plot(data=ld_frame, lab_vote~change_ld)
plot(data=ld_frame, ld_vote~change_ld)
plot(data=ld_frame, green_vote~change_ld)
plot(data=ld_frame, ukip_vote~change_ld)
cor(ld_frame$green_vote, ld_frame$change_ld)

plot(data=ind_frame, leave~change_ind)
plot(data=ind_frame, ab~change_ind) #N
plot(data=ind_frame, c1~change_ind) #N
plot(data=ind_frame, c2~change_ind) 
plot(data=ind_frame, de~change_ind) #N
plot(data=ind_frame, con_vote~change_ind)
plot(data=ind_frame, lab_vote~change_ind)
plot(data=ind_frame, ld_vote~change_ind)
plot(data=ind_frame, green_vote~change_ind)
plot(data=ind_frame, ukip_vote~change_ind)

plot(data=green_frame, leave~change_green) #more wins from areas with less proportion of leave voters
plot(data=green_frame, ab~change_green) #N
plot(data=green_frame, c1~change_green) #N
plot(data=green_frame, c2~change_green) #N
plot(data=green_frame, de~change_green) #N
plot(data=green_frame, con_vote~change_green)
plot(data=green_frame, lab_vote~change_green)
plot(data=green_frame, ld_vote~change_green)
plot(data=green_frame, green_vote~change_green)
plot(data=green_frame, ukip_vote~change_green)

cor(green_frame$green_vote, green_frame$change_green)

north_frame <- current_df %>% filter(current_df$region=='North')
ggplot(current_df, aes(y=de)) +
  geom_boxplot() +
  facet_wrap(~region)

ggplot(current_df, aes(y=ab)) +
  geom_boxplot() +
  facet_wrap(~region)

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

#region versus conservative loss
l <- ggplot(conservative_logit, aes(region,fill = conservative_logit$change_con))
l <- l + geom_bar( position = position_dodge())
print(l)
tapply(conservative_logit$change_con, conservative_logit$region,mean)



#prediction
split <- sample.split(conservative_logit$change_con, SplitRatio = 0.80) 
train <- subset(conservative_logit, split == T) 
test <- subset(conservative_logit, split == F)

model_glm <- glm(change_con ~ ., data = train, family='binomial') 
model_glm <- glm(formula = change_con ~ ab + con_vote + lab_vote + leave, family = binomial(link = "logit"), 
                    data = conservative_logit)

summary(model_glm)
predicted_glm <- predict(model_glm, test, type='response')
predicted_glm <- ifelse(predicted_glm > 0.5,1,0)

table(test$change_con, predicted_glm)


