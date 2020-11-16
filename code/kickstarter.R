if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")
if(!require(hexbin)) install.packages("hexbin", repos = "http://cran.us.r-project.org")
if(!require(stringr)) install.packages("stringr", repos = "http://cran.us.r-project.org")
if(!require(lubridate)) install.packages("lubridate", repos = "http://cran.us.r-project.org")
if(!require(ggrepel)) install.packages("ggrepel", repos = "http://cran.us.r-project.org")
if(!require(ggplot2)) install.packages("ggplot2", repos = "http://cran.us.r-project.org")
if(!require(cowplot)) install.packages("cowplot", repos = "http://cran.us.r-project.org")
if(!require(dplyr)) install.packages("dplyr", repos = "http://cran.us.r-project.org")
if(!require(here)) install.packages("here", repos = "http://cran.us.r-project.org")
if(!require(formatR)) install.packages("formatR", repos = "http://cran.us.r-project.org")
if(!require(archivist)) install.packages("archivist", repos = "http://cran.us.r-project.org")
if(!require(devtools)) install.packages("devtools", repos = "http://cran.us.r-project.org")
if(!require(moderndive)) install.packages("moderndive", repos = "http://cran.us.r-project.org")


library(tidyverse)
library(caret)
library(data.table)
library(stringr)
library(lubridate)
library(ggrepel)
library(ggplot2)
library(cowplot)
library(dplyr)
library(here)
library(formatR)
library(archivist)
library(devtools)
library(moderndive)
library(ModelMetrics)
library(gridExtra)

data_dir <- paste(getwd(), "/data/", sep = "")

ds <- read_csv(paste(data_dir, "ks-projects-201801.csv", sep = ""))

glimpse(ds)
#There aree 378,661 rows

#Let's rename some cuomns for consistancy to remove blanks
#according to the documentatino, `usd pledged` isn't needed, so we'll remove it
ds <- ds %>% select(-`usd pledged`)

#Add a campaign length in days
ds <- ds %>% mutate(campaign_length_days = deadline - as.Date(launched) ,
                    launched = as.Date(launched),
                    pledged_ratio = round(usd_pledged_real) / round(usd_goal_real))




#Exploratory Data Analysis (EDA)

#let's see if there are missing valuese
summary(ds)

#noticing something funny - the launched dates start in 1970 - let's look closer
ds %>% distinct(launched) %>% arrange(launched)

#let's remove the dates on 1/1/1970
ds <- ds %>% filter(launched >= "2009-04-21")

#let's look at items with a pledge ration of less then 1
failed <- ds %>% filter(pledged_ratio < 1) 

failed %>% distinct(state)
#it appeaers that some are succeessful despite having not met their goal!
failed %>% filter(state=="successful")

#interesting , but both proejcets raised a significant maount of money and continued on. we'll leave them in

#we have no NA values. excellent!
#Now we can examine the fields
glimpse(ds)

#It looks like "category, "main_category", Currency, state and country can be made into factors
ds$category <- as.factor(ds$category)
ds$main_category <- as.factor(ds$main_category)
ds$currency <- as.factor(ds$currency)
ds$state <- as.factor(ds$state)
ds$country  <- as.factor(ds$country )

#let's look at the levels of each of these factors
levels(ds$category)
levels(ds$main_category)
levels(ds$currency)
levels(ds$state)
levels(ds$country)

#Country "N,0\"" seems strange - we'll keep an eye out for that later.
#Let's remove those
ds <- ds %>% filter(country != "N,0\"")
glimpse(ds)

#we're down to 374,864 rows

#Which main_category projects are most frequently proposed?
freq <- ds %>%
  group_by(main_category) %>%
  summarize(count=n()) %>%
  arrange(desc(count))

freq$main_category <- factor(freq$main_category, levels=freq$main_category)

freq %>% ggplot( aes(main_category, count, fill=count)) + geom_bar(stat="identity") + 
  ggtitle("Projects by Main Category") + xlab("Main Category") + ylab("Project Count") + 
  theme(plot.title=element_text(hjust=0.5), axis.title=element_text(size=12, face="bold"), 
        axis.text.x=element_text(size=12, angle=90))

#In terms of the main categorie, Film & video projects seem to be the most wide-spreead, while Dance is the least.

#what does this look like in terms of money raised, per main category?
raised <- ds %>% group_by(main_category) %>% summarise(total_raised = sum(usd_pledged_real)) %>% 
  arrange(desc(total_raised))

raised$main_category <- factor(raised$main_category, levels=raised$main_category)

raised %>% ggplot( aes(main_category, total_raised/1000000, fill=total_raised)) + geom_bar(stat="identity") + 
  ggtitle("UDS Pledged by by Main Category In Millions") + xlab("Main Category") + 
  ylab("Cumulatice USD Raised Per Main Category in Millions") + 
  theme(legend.position = "none", plot.title=element_text(hjust=0.5, size = 15, face="bold"), 
        axis.title=element_text(size=12, face="bold"), 
        axis.text.x=element_text(size=12, angle=90)) 

#let's examine by sub cattegory
freq <- ds %>%
  group_by(category, main_category) %>%
  summarize(count=n()) %>%
  arrange(desc(count))


freq <- freq %>% mutate(category = paste(category, "/", main_category, sep = ""))

freq$category <- factor(freq$category, levels=freq$category)

freq %>% head(15) %>% ggplot( aes(category , count, fill=count)) + geom_bar(stat="identity") + 
  ggtitle("Projects by Sub / Main Category") + xlab("Sub Category / Main Category") + ylab("Project Count") + 
  theme(plot.title=element_text(hjust=0.5), axis.title=element_text(size=12, face="bold"), 
        axis.text.x=element_text(size=12, angle=90))


#film/video are the most popular while Dance is the least popular in terms of hard numbers. 
#How do these numbers break down in terms of states of projects? 


ds %>% group_by(state) %>% summarize(count = n(), percentage = paste(round(count* 100/(dim(ds)[1]) ), "%", sep="")) %>%
  arrange(desc(count)) %>%  ggplot(aes(x = reorder(state , desc(count)) , y = count , fill = state , label = percentage)) + 
  geom_col() + geom_text(size = 6 , vjust = -0.2) + 
  labs(x = '' , y = 'Count', title = "Cumulative Projects By Status") + 
  theme_classic() + theme(legend.position = 'none') + 
  theme(axis.text.x = element_text(size = 13 , face = 'bold'))

#Let's examine the distribution of successful vs failed variables 

ds1 <- ds %>% filter(state %in% c('successful' , 'failed')) %>% select(state , usd_pledged_real) %>%
  ggplot(aes(usd_pledged_real , fill = state)) + geom_density(alpha = 0.65) + 
  labs(x = 'Pledged Amount (USD)' , title = 'Pledged Value Distribution') + 
  scale_fill_manual(values = c('yellow' , 'blue'))

ds2 <- ds %>% filter(state %in% c('successful' , 'failed')) %>% select(state , usd_pledged_real) %>%
  ggplot(aes(log(usd_pledged_real+1) , fill = state)) + geom_density(alpha = 0.65) + 
  labs(x = 'log(Pledged Amount + 1) (USD)' , title = 'Logarithmic Pledged Value Distribution') + 
  scale_fill_manual(values = c('yellow' , 'blue')) + 
  theme(legend.position = 'none')

ds3 <- ds %>% filter(state %in% c('successful' , 'failed')) %>% select(state , usd_goal_real) %>%
  ggplot(aes(usd_goal_real , fill = state)) + geom_density(alpha = 0.65) + 
  labs(x = 'Goal Amount (USD)' , title = 'Goal Value Distribution') + 
  scale_fill_manual(values = c('yellow' , 'blue'))

ds4 <- ds %>% filter(state %in% c('successful' , 'failed')) %>% select(state , usd_goal_real) %>%
  ggplot(aes(log(usd_goal_real+1) , fill = state)) + geom_density(alpha = 0.65) + 
  labs(x = 'log(Goal Amount + 1) (USD)' , title = 'Logarithmic Goal Value Distribution') + 
  scale_fill_manual(values = c('yellow' , 'blue')) + 
  theme(legend.position = 'none')

grid.arrange(p1 , p2 , p3 , p4 , nrow = 2)


#let's partition the data to start analyzing it
set.seed(1, sample.kind="Rounding")
test_index <- createDataPartition(y = ds$state, times = 1, p = 0.1, list = FALSE)
kickstarter <- ds[-test_index,]
temp <- ds[test_index,]

#we will use RMSE function in the ModelMetrics package to calculate the accuracy

# Make sure ID in validation set are also in edx set
validation <- temp %>% semi_join(kickstarter, by = "ID")

# Add rows removed from validation set back into kickstarter set
removed <- anti_join(temp, validation)
kickstarter <- rbind(kickstarter, removed)






#splitting the edx data set into testing and training , making sure to exclude the validation data created before.
test_index <- createDataPartition(y = kickstarter$state, times = 1, p = 0.1, list = FALSE)
train_set <- kickstarter[-test_index,]
test_set <- kickstarter[test_index,]


## ---- mu_hat --------
#start out slow with the most basic status
mu_hat <- mean(train_set$state)
mu_hat














