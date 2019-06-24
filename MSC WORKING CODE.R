library(tidyr)
library(ggplot2)
library(dplyr)
library(tidyverse)
library(purrr)
library(data.table)
library(roomba)
library(magrittr)
library(plyr)
install.packages("devtools")
devtools::install_github("ropenscilabs/roomba")



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
head(results)

new2 <- results %>% 
  map_df(as_tibble)
new <- do.call(rbind, lapply(results, as.data.frame))

write.csv(new, 'new_list.csv')
new_list <- read.csv("new_list.csv")

head(bind_rows(results))
g <- results %>%
  map(flatten) %>%
  bind_rows()    ###collapsing list 'results' into a data frame

class(g$control)
control_vec <- g$control
la_name <- names(results)
test <- cbind.data.frame(la_name, control_vec)
class(test)
test2 <- merge(brexit, test, by = brexit$LAName, all.x = T)

for (i in 1:length(brexit)) {
  for(j in 1:length(test))
  if (brexit$LAName[i]!=test[j])
    brexit$control_2019 = NA
  if (brexit$LAName[i]==test[j])
    brexit$control_2019 = test$control_vec
}

## mutating the "result" structure to understand better

list1 <- list(control = 'NOC NO CHANGE')
list2 <- list(elected = c('Independent', 'Conservative', 'Labour UK', 'Independence Party'))
list3 <- list(total = c('Independent', 'Conservative', 'Labour UK', 'Independence Party'))
list4 <- list(change = c('Independent', 'Conservative', 'Labour UK', 'Independence Party'))
first <- list(allerdale = c(list1, list2, list3, list4))
first
str(first)
str(results[1])
results[[3]][[2]]


# mayra separate columns
library(dplyr)
library(tidyr)

df %>% unnest(elected) %>% group_by(rn) %>% mutate(col=seq_along(rn)) %>% spread(key=col, val=elected)
df$elected[1]

# mayra not R way

for (column in names(results)) #{
  print(column)
  print(results[[column]])
  
  #brexit[brexit$LAName == column,]$control_2019 <- 
  print(brexit[brexit$LAName == column,])
  
  brexit[brexit$LAName == column,]$control_2019 <- results[[column]]$control
  print(results[[column]]$control)
}
elected_vec <- rep(NA, 248)


df <- data.frame(t(sapply(results,c)))
write.table(df,"nested_frame.txt",sep="|")
setDT(df, keep.rownames = TRUE)


str(df$elected)
df$control <- as.data.frame(df$control)
new <- unlist(df$elected)
df2 <- as.data.frame(df$elected)



## subsetting the data set
results_names <- names(results)
brexit_filtered <- brexit[brexit$LAName %in% results_names,]
results_names[!(results_names %in% brexit_names)]
brexit_names[!(brexit_names %in% results_names)]

brexit_names <- brexit_filtered$LAName



##subsetting df to a data frame with just control and elected as variables
filter_df <- df[,1:3]
str(filter_df)
z <- as.tibble(filter_df)


cbind.data.frame(id=unique(z$elected), do.call(rbind, z$elected))
                 