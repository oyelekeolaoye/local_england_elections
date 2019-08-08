conservative_logit$change_con <- as.factor(conservative_logit$change_con)
str(conservative_logit)

levels(conservative_logit$change_con)[1] <- "not a large loss"
levels(conservative_logit$change_con)[2] <- "large loss"

conservative_logit[,2:11] <- conservative_logit[,2:11] * 100
conservative_logit[,2:11] <- scale(conservative_logit[,2:11])

pl1 <- ggplot(conservative_logit, aes(change_con,fill = change_con))+
  geom_bar( position = position_dodge())

pl2 <- ggplot(conservative_logit, aes(region,fill = change_con))+
  geom_bar( position = position_dodge())

pl3 <- ggplot(conservative_logit, aes(ab,fill = change_con))+
  geom_density( position = position_dodge(), alpha=0.5)

pl4 <- ggplot(conservative_logit, aes(c1,fill = change_con))+
  geom_density( position = position_dodge(), alpha=0.5)

pl5 <- ggplot(conservative_logit, aes(c2,fill = change_con))+
  geom_density( position = position_dodge(), alpha=0.5)

pl6 <- ggplot(conservative_logit, aes(de,fill = change_con))+
  geom_density( position = position_dodge(), alpha=0.5)

pl7 <- ggplot(conservative_logit, aes(con_vote,fill = change_con))+
  geom_density( position = position_dodge(), alpha=0.5)

pl8 <- ggplot(conservative_logit, aes(lab_vote,fill = change_con))+
  geom_density( position = position_dodge(), alpha=0.5)

pl9 <- ggplot(conservative_logit, aes(ld_vote,fill = change_con))+
  geom_density( position = position_dodge(), alpha=0.5)

pl10 <- ggplot(conservative_logit, aes(ukip_vote,fill = change_con))+
  geom_density( position = position_dodge(), alpha=0.5)

pl11 <- ggplot(conservative_logit, aes(green_vote,fill = change_con))+
  geom_density( position = position_dodge(), alpha=0.5)

pl12 <- ggplot(conservative_logit, aes(leave,fill = change_con))+
  geom_density( position = position_dodge(), alpha=0.5)

library(gridExtra)

grid.arrange(pl1,pl2,pl3,pl4,pl5,pl6,pl7,pl8,pl9,pl10,pl11,pl12, ncol=3, nrow=4)



pairs(conservative_logit[,2:11],col = c("blue", "red")[conservative_logit$change_con])

pairs(conservative_logit2[,2:10])

ggpairs(conservative_logit2[,2:10],lower = list(continuous = wrap(ggally_points, size = .3)))

levels(conservative_logit$change_con)[1] <- "not a large loss"
levels(conservative_logit$change_con)[2] <- "large loss"

conservative_logit2 <- conservative_logit[,-4]
conservative_logit2[,2:10] <- conservative_logit2[,2:10]*100


#logistic regression on conservative
glm_con <- glm(formula = change_con ~ . - ukip_vote , family = binomial(link = "logit"), 
               data = conservative_logit2)
summary(glm_con)
step(glm_con, direction = "backward")
model1 <- glm(formula = change_con ~ ab + con_vote + ld_vote + leave, 
              family = binomial(link = "logit"), data = conservative_logit2) #model for 15%
summary(model1)

#PERFORMING HOSMER-LEMESHOW GOODNESS OF FIT TEST FOR LOGISTIC MODELS
hl <- hoslem.test(model1$y, fitted(model1), g=10)
hl #there is no evidence of lack of fit
cbind(hl$expected, hl$observed)


install.packages("jtools")
library(jtools)
install.packages("BeSS")
library(BeSS)
# Best subset selection
fit1 <- bess(conservative_logit2[,2:10], conservative_logit2$change_con, family = "binomial")
print(fit1)
coef(fit1, sparse=TRUE) # The estimated coefficients
bestmodel <- fit1$bestmodel
summary(bestmodel)

summ(model1, vifs = TRUE) #checking for variance inflation factor
