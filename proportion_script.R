

##CONVERTING CHANGE VARIABLE TO PROPORTION
current_df_prop <- current_df
current_df_prop$change_con <- current_df$change_con/current_df$seats_available
current_df_prop$change_ind <- current_df$change_ind/current_df$seats_available
current_df_prop$change_lab <- current_df$change_lab/current_df$seats_available
current_df_prop$change_ld <- current_df$change_ld/current_df$seats_available
current_df_prop$change_ukip <- current_df$change_ukip/current_df$seats_available
current_df_prop$change_green <- current_df$change_green/current_df$seats_available

#CREATING SEPARATE DATA FRAMES FOR EACH PARTY TO BEGIN MODELLING
conservative_frame <- current_df_prop %>% select(-la_name, -region, -control, -seats_available, -total_seats, -change_green, -change_ind, -change_lab, -change_ld, -change_ukip)
labour_frame <- current_df_prop %>% select(-la_name, -region, -control, -seats_available, -total_seats, -change_green, -change_ind, -change_con, -change_ld, -change_ukip)
green_frame <- current_df_prop %>% select(-la_name, -region, -control, -seats_available, -total_seats, -change_con, -change_ind, -change_lab, -change_ld, -change_ukip)
ld_frame <- current_df_prop %>% select(-la_name, -region, -control, -seats_available, -total_seats, -change_green, -change_ind, -change_lab, -change_con, -change_ukip)
ind_frame <- current_df_prop %>% select(-la_name, -region, -control, -seats_available, -total_seats, -change_green, -change_con, -change_lab, -change_ld, -change_ukip)
ukip_frame <- current_df_prop %>% select(-la_name, -region, -control, -seats_available, -total_seats, -change_green, -change_ind, -change_lab, -change_ld, -change_con)

#LINEAR REGRESSION FOR CONSERVATIVE
model_con <- lm(change_con ~ ., data = conservative_frame)
model.selection <- ols_step_best_subset(model_con)
model.selection   
plot(model.selection)
test_con <- lm(change_con ~ de + lab_vote + leave, data = conservative_frame)
summary(test_con)
plot(test_con)

step(model_con) #stepwise selection

model_con_step <- lm(change_con ~ lab_vote + leave, data = conservative_frame)
print(summary(model_con_step),concise=TRUE)
plot(model_con_step)

ggcoef(model_con,vline_color = "red",
       vline_linetype =  "solid",
       errorbar_color = "blue",
       errorbar_height = .25,exclude_intercept = TRUE)

ggpairs(conservative_frame,lower = list(continuous = wrap(ggally_points, size = .3)))

outlierTest(lm(change_con ~ lab_vote + leave, data = conservative_frame)) #outlier test

#stepwise LABOUR
model_lab <- lm(change_lab ~ ., data = labour_frame)
model.selection_lab <- ols_step_best_subset(model_lab)
model.selection_lab  
plot(model.selection_lab)
test_lab <- lm(change_lab ~ ab + c1 + con_vote + lab_vote + ld_vote + ukip_vote + leave, data = labour_frame)
summary(test_lab)
plot(test_lab)
step(model_lab) #stepwise selection

ggcoef(model_con,vline_color = "red",
       vline_linetype =  "solid",
       errorbar_color = "blue",
       errorbar_height = .25,exclude_intercept = TRUE)

ggpairs(conservative_frame[,-1],lower = list(continuous = wrap(ggally_points, size = .3)))

print(summary(lakes.step),concise=TRUE)

#stepwise INDEPENDENT
model_ind <- lm(change_ind ~ ., data = ind_frame)
model.selection_ind <- ols_step_best_subset(model_ind)
model.selection_ind  
plot(model.selection_ind)
test_ind <- lm(change_ind ~ c1 + de + con_vote + lab_vote + ld_vote + green_vote, data = ind_frame)
summary(test_ind)
plot(test_ind)
step(model_ind) #stepwise selection

#stepwise LIBDEM
model_ld <- lm(change_ld ~ ., data = ld_frame)
model.selection_ld <- ols_step_best_subset(model_ld)
model.selection_ld
plot(model.selection_ld)
test_ld <- lm(change_ld ~  ab + ld_vote, data = ld_frame)
summary(test_ld)

step(model_ld)  #stepwise selection


#stepwise GREEN
model_green <- lm(change_green ~ ., data = green_frame)
model.selection_green <- ols_step_best_subset(model_green)
model.selection_green
plot(model.selection_green)
test_green <- lm(change_green ~ c2 + green_vote + leave, data = green_frame)
summary(test_green)

step(model_green) #stepwise selection


#stepwise UKIP
model_ukip <- lm(change_ukip ~ ., data = ukip_frame)
model.selection_ukip <- ols_step_best_subset(model_ukip)
model.selection_ukip
plot(model.selection)
test_ukip <- lm(change_ukip ~ c2 + ukip_vote + leave, data = ukip_frame)
summary(test_ukip)

step(model_ukip) #stepwise selection




#******************************************************************************
#---scatterplots for examining relationship between leave votes & loss/gain
#*******************************************************************************

ggplot(current_df, aes(conservative_frame$change_con, leave)) +
        geom_point() +
        geom_smooth(method = "lm", se = FALSE)
##there seems to be a moderate positve relationship between leave votes and change for Cons
#Cons suffered less loss in areas with high proportion of leave votes
#their gains came from areas with more than 50% leave voters, although not much



ggplot(current_df, aes(current_df$change_green, leave)) +
        geom_point()+
        geom_smooth(method = "lm", se = FALSE)
#the pattern doesn't seem to be clear for leave and change for Greens
#there is a slight indication of an inverse relationship
#more of the gains came from areas with low proportion of leave votes 

ggplot(current_df, aes(current_df$change_ind, leave)) +
        geom_point() +
        geom_smooth(method = "lm", se = FALSE)
#there is a slight indication of a positive relationship for Greens and leave votes
#more gains came from areas with high proportion of leave voters

ggplot(current_df, aes(current_df$change_lab, leave)) +
        geom_point() +
        geom_smooth(method = "lm", se = FALSE)
#the pattern seems random for leave votes and change for labour
#indication of a negative relationship perharps

ggplot(ld_frame, aes(ld_frame$change_ld, leave)) +
        geom_point() 
#there is a slight indication of an inverse relationship for leave votes and change for Libdem
#more gains came from areas with less than 60% of leave voters


ggplot(current_df, aes(current_df$change_ukip, leave)) +
        geom_point() +
        geom_smooth(method = "lm", se = FALSE)
#the pattern between leave votes and change for UKIP is not clear


 ld_frame$remain <- 1-ld_frame$leave
ggplot(ld_frame, aes(ld_frame$change_ld, remain)) +
        geom_point() + 
        geom_smooth(method = "lm", se = FALSE)
