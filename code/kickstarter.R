if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(stringr)) install.packages("stringr", repos = "http://cran.us.r-project.org")
if(!require(lubridate)) install.packages("lubridate", repos = "http://cran.us.r-project.org")
if(!require(ggrepel)) install.packages("ggrepel", repos = "http://cran.us.r-project.org")
if(!require(ggplot2)) install.packages("ggplot2", repos = "http://cran.us.r-project.org")
if(!require(dplyr)) install.packages("dplyr", repos = "http://cran.us.r-project.org")
if(!require(formatR)) install.packages("formatR", repos = "http://cran.us.r-project.org")
if(!require(devtools)) install.packages("devtools", repos = "http://cran.us.r-project.org")
if(!require(moderndive)) install.packages("moderndive", repos = "http://cran.us.r-project.org")
if(!require(gridExtra)) install.packages("gridExtra", repos = "http://cran.us.r-project.org")
if(!require(randomForest)) install.packages("randomForest", repos = "http://cran.us.r-project.org")


library(randomForest)
#library(xgboost)
library(tidyverse)
library(stringr)
library(lubridate)
library(ggrepel)
library(ggplot2)
library(dplyr)
library(formatR)
library(devtools)
library(moderndive)
library(ModelMetrics)
library(gridExtra)
library(GGally)
library(broom)

data_dir <- paste(getwd(), "/data/", sep = "")

data <- read_csv(paste(data_dir, "ks-projects-201801.csv", sep = ""))

glimpse(data)
#There aree 378,661 rows

#Let's rename some cuomns for consistancy to remove blanks
#according to the documentatino, `usd pledged` isn't needed, so we'll remove it
ds <- data %>% select(-`usd pledged`)

#Add a campaign length in days
ds <- ds %>% mutate(time_int = as.numeric(deadline - as.Date(launched)) ,
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


mean_median_pledged <- ds %>% filter(state %in% c('successful' , 'failed')) %>% group_by(state) %>% 
  summarize(mean = round(mean(usd_pledged_real)), median = round(median(usd_pledged_real)), max=round(max(usd_pledged_real))) 

mean_median_goal <- ds %>% filter(state %in% c('successful' , 'failed')) %>% group_by(state) %>% 
  summarize(mean = round(mean(usd_goal_real)), median = round(median(usd_goal_real)), max=round(max(usd_goal_real))) 


#ds_pledged_xlabs <- c(0, mean_median_pledged[2,]["median"], mean_median_pledged[2,]["max"])
#ds_goal_xlabs <- c(0, mean_median_goal[2,]["median"], mean_median_pledged[2,]["max"])


ds1 <- ds %>% filter(state %in% c('successful' , 'failed')) %>% select(state , usd_pledged_real) %>%
  ggplot(aes(usd_pledged_real , fill = state)) + geom_density(alpha = 0.65) + 
  labs(x = 'Pledged Amount (USD)' , title = 'Pledged Value Distribution') +
  scale_x_discrete(labels=function(l) {    paste0(round(l/1e6,2),"m")  }) 

ds2 <- ds %>% filter(state %in% c('successful' , 'failed')) %>% select(state , usd_pledged_real) %>%
  ggplot(aes(usd_pledged_real +1 , fill = state)) + geom_density(alpha = 0.65) + 
  scale_x_log10(labels=function(l) {    paste0(round(l/1e3,2),"k")  }) +
  labs(x = "log-tranform Goal Amount (K's USD)", title = "Logarithmic Pledged Goal Distribution in $K's") + 
  theme(legend.position = 'none') +
  geom_vline(data = mean_median_pledged, aes(xintercept = median,  linetype = state), size=1, color="darkred") + 
  geom_vline(data = mean_median_pledged, aes(xintercept = mean,  linetype = state), size=1, color="darkgreen") +
  
  geom_text_repel(data = mean_median_pledged %>% select(state, mean), 
                  aes(x=mean, y =1, label=paste(state, " mean \nof $",mean), color=state, angle=30), nudge_x = 5) +
  geom_text_repel(data = mean_median_pledged %>% select(state, median), 
                  aes(x=median, y =.5, label=paste(state, " median \nof $",median), color=state, angle=30),
                   nudge_x = -5) 
  
ds3 <- ds %>% filter(state %in% c('successful' , 'failed')) %>% select(state , usd_goal_real) %>%
  ggplot(aes(usd_goal_real , fill = state)) + geom_density(alpha = 0.65) + 
  labs(x = 'Goal Amount (USD)' , title = 'Goal Value Distribution') + 
  scale_x_discrete(labels=function(l) {    paste0(round(l/1e6,2),"m")  }) 

ds4 <- ds %>% filter(state %in% c('successful' , 'failed')) %>% select(state , usd_goal_real) %>%
  ggplot(aes(usd_goal_real +1 , fill = state)) + geom_density(alpha = 0.65) + 
  scale_x_log10(labels=function(l) {    paste0(round(l/1e6,2),"m")  }) +
  labs(x = "log-tranform Goal Amount (MM's USD)", title = "Logarithmic Pledged Goal Distribution in $MM's") + 
  theme(legend.position = 'none') +
  geom_vline(data = mean_median_goal, aes(xintercept = median,  linetype = state), size=1, color="darkred") + 
  geom_vline(data = mean_median_goal, aes(xintercept = mean,  linetype = state), size=1, color="darkgreen") +
  
  geom_text_repel(data = mean_median_goal %>% select(state, mean), 
                  aes(x=mean, y =1, label=paste(state, " mean \nof $",mean), color=state, angle=30), nudge_x = 10) +
  geom_text_repel(data = mean_median_goal %>% select(state, median), 
                  aes(x=median, y =.5, label=paste(state, " median \nof $",median), color=state, angle=30),
                  nudge_x = -5, nudge_y = 3 ) 


grid.arrange(ds1 , ds2 , ds3 , ds4 )
            
#Both the successful pledged and the goal amount seem to be pretty normally distributed on the logarithmic scale.
#It looks like the higher the pledged amount, the higher the chance of success. 
#HOwever, it also looks like the highere the goal, the less chance of success there is. 


ds %>% filter(state %in% c('successful' , 'failed')) %>%
  ggplot(aes(x = state , y = pledged_ratio , fill = state)) + geom_boxplot(alpha = 0.8) + 
  ylim(0 , 1.5) + theme_classic() + theme(legend.position = 'none') +
   labs(x = '' , y = 'Pledged Ratio', title="Pledge Ratios Box Plot By State")

# Quantiles of failed projects pledged_ratio
round(quantile(ds$pledged_ratio[ds$state == 'failed'] , probs = seq(0.1 , 0.9 , 0.1), na.rm = TRUE) , 4)
#Kickstarter has a requirement of 50% funding for a project. Here, we can see that the higherst quantile doens't even get into the 30%+ range

# Quantiles of succesesful projects pledged_ratio - all over 100%
round(quantile(ds$pledged_ratio[ds$state == 'successful'] , probs = seq(0.1 , 0.9 , 0.1), na.rm = TRUE) , 4)
#Kickstarter has a requirement of 50% funding for a project. Here, we can see that the higherst quantile doens't even get into the 30%+ range



#correlation of numerican feaetures:
#ds %>% select(usd_goal_real , usd_pledged_real , time_int , pledged_ratio ,  backers) %>% 
#  ggpairs()

#It looks like the only real correlation - .753 is shown between pledged amounts and the amount of backers. 
#This makes sense 

#first focus in only on the failed or succeessful projects
#ds <- ds %>% filter(state %in% c("failed", "successful"))

#create a binary field to show failed / successful campaign

ds <- ds %>% select(category ,  country , usd_goal_real , pledged_ratio, time_int , state) %>% 
  filter(state %in% c('successful' , 'failed')) %>% 
  mutate(state = as.factor(ifelse(state == 'successful' , 1 , 0))) %>%
  mutate_if(is.character , as.factor)

ds2 <- data.frame(model.matrix( ~ . -1 , ds))

colnames(ds2)[ncol(ds2)] = 'state'
ds2$state = as.factor(ds2$state)

index = sample(dim(ds2)[1] , dim(ds2)[1]*0.80 , replace = FALSE)
trainset = ds2[index , ]
testset = ds2[-index , ]

rm(data, ds1, ds3, ds4, failed)

log.fit <- glm(state ~ . , trainset, family = binomial(link = "logit") )
probs <- predict(log.fit , testset , type = 'response')

q1 <- qplot(probs , geom = 'density') + geom_density(fill = 'pink' , alpha = 0.7) +
  labs(x = 'Probabilities' , title = 'GLM REGRESSION - Probabilities assigned to test set')

set.seed(1, sample.kind="Rounding")
test_lm_index <- createDataPartition(y = ds$state, times = 1, p = 0.2, list = FALSE)
lm_ds <- ds[-test_lm_index,]
test_ds <- ds[test_lm_index,]
log2.fit  <- lm(factor(state) ~ ., lm_ds)
probs_lm <- predict(log2.fit, test_ds, type = "response")

q2 <- qplot(probs_lm , geom = 'density') + geom_density(fill = 'pink' , alpha = 0.7) +
  labs(x = 'Probabilities' , title = 'Linear REGRESSION - Probabilities assigned to test set')

plot_grid(q1, q2)

#augment(log.fit)


#rf.fit = randomForest(state ~ . , trainset[sample(dim(trainset)[1] , 50000) , ] , ntree = 500)
#rf.fit


