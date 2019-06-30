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


# mayra separate columns
library(dplyr)
library(tidyr)

df %>% unnest(elected) %>% group_by(rn) %>% mutate(col=seq_along(rn)) %>% spread(key=col, val=elected)
df$elected[1]






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
                 