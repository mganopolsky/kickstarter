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
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(ranger)) install.packages("ranger", repos = "http://cran.us.r-project.org")
if(!require(naivebayes)) install.packages("naivebayes", repos = "http://cran.us.r-project.org")

library(randomForest)
library(naivebayes)
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
library(caret)
library(ranger)

data_dir <- paste(getwd(), "/data/", sep = "")

data <- read_csv(paste(data_dir, "ks-projects-201801.csv", sep = ""))

glimpse(data)
#There aree 378,661 rows

#according to the documentatino, `usd pledged` isn't needed since there is the usd_pledged_real, so I'll remove it
ds <- data %>% select(-`usd pledged`)

#Add a campaign length in days (time_int), pledged ratio (how much has been pledged vs what the goal is), 
#as well as the average pledge per backer, per project
#I will also like to explore whether the month or the day of the week of the posted project mught have something to do with its succeess, 
#so I have added the launched_month and launched_day_of_week columns.
ds <- ds %>% mutate(time_int = as.numeric(deadline - as.Date(launched)) ,
                    launched = as.Date(launched),
                    pledged_ratio = round(usd_pledged_real / usd_goal_real, 2),
                    avg_backer_pldg = round(usd_pledged_real/backers) ) %>%
            mutate(launched_month = as.factor(format(launched, "%m")),
                    launched_day_of_week = as.factor(format(launched, "%A")  ))



#############################33
#Exploratory Data Analysis (EDA)

#let's see if there are missing values in the data
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
ds$launched_month <- as.factor(ds$launched_month)
ds$launched_day_of_week <- as.factor(ds$launched_day_of_week)

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

#select all the necessary columns, with the "state" being the last one - important for the matrix of dummy variables  later
ds <- ds %>% select(category ,  country , usd_goal_real , pledged_ratio, time_int , backers, launched_day_of_week, launched_month, state ) %>% 
  filter(state %in% c('successful' , 'failed')) %>% 
  mutate(state = as.factor(ifelse(state == 'successful' , 1 , 0))) %>%
  mutate_if(is.character , as.factor)

results_GLM <- function(train_data, test_data, predicted_field_name, predictors_list) {

  fm_string <- paste(predicted_field_name, "~")
  for (i in 1:length(predictors_list)) {
    fm_string <- paste(fm_string, predictors_list[i])
    if (i < length(predictors_list))
      fm_string <- paste(fm_string, "+")
  }
  print(fm_string)
  fm <- as.formula(fm_string)
  
  glm.fit <- glm(fm , train_data, family = binomial)
  #aug_glm_model <- augment(glm.fit, newdata = test_data, type.predict = 'response')
  
  #result <- aug_glm_model %>% mutate(Prediction = factor(round(.fitted)), Reference = state) %>%   
  #  select(Reference , Prediction ) %>% table()
  
  #return (caret::confusionMatrix(result))
  
  return (get_confusion_matrix(model_fit= glm.fit, test_data = test_data))
}


get_confusion_matrix <- function(model_fit, test_data)
{
  aug_model <- augment(model_fit, newdata = test_data, type.predict = 'response')
  
  result <- aug_model %>% mutate(Prediction = factor(round(.fitted)), Reference = state) %>%   
    select(Reference , Prediction ) %>% table()
  
  return (caret::confusionMatrix(result))
}

get_accuracy <- function(cf_matrix) 
{
  cf_accuracy <- sum(diag(cf_matrix)) / sum(cf_matrix)
  return(cf_accuracy)
}

#since the outcome is a factor and not just numeric, we can't use simple linear regression to calculate this. we need to use GLM
#predictions based solely on category

set.seed(1, sample.kind="Rounding")
test_lm_index <- createDataPartition(y = ds$state, times = 1, p = 0.2) %>% unlist()
lm_ds <- ds[-test_lm_index,]
test_ds <- ds[test_lm_index,]

columns <- colnames(ds)
col_subset <- columns[columns %in% c("category")]

cf1 <- results_GLM(lm_ds, test_ds, "state", col_subset)
cf1
#################
#category AND time interval in days
col_subset <- columns[columns %in% c("category", "time_int", "country")]

cf2 <- results_GLM(lm_ds, test_ds, "state", col_subset)
cf2


#################
#category AND time AND country interval in days
col_subset <- columns[columns %in% c("category", "time_int", "country")]

cf3 <- results_GLM(lm_ds, test_ds, "state", col_subset)
cf3

##############
col_subset <- columns[columns %in% c("category", "time_int", "country", "usd_goal_real")]

cf4 <- results_GLM(lm_ds, test_ds, "state", col_subset)
cf4
##############
col_subset <- columns[columns %in% c("category", "time_int", "country", "usd_goal_real", "launched_month")]

cf5 <- results_GLM(lm_ds, test_ds, "state", col_subset)
cf5

##############
col_subset <- columns[columns %in% c("category", "time_int", "country", "usd_goal_real", "launched_month", "launched_day_of_week")]

cf6 <- results_GLM(lm_ds, test_ds, "state", col_subset)
cf6


cf6_accuracy <- get_accuracy(cf6$table)

accuracy_results <- list()
accuracy_results['GLM'] =  cf6_accuracy

#try a random forest prediction - this doesn't work because there are more then 53 categories?

ds <- ds %>% select(-backers, -pledged_ratio)
ds_matrix_data <- data.frame(model.matrix( ~ . -1 , ds))

#rename the last column to 'state'
colnames(ds_matrix_data)[ncol(ds_matrix_data)] = 'state'
ds_matrix_data$state = as.factor(ds_matrix_data$state)

set.seed(123)
idx <- sample(dim(ds_matrix_data)[1] , dim(ds_matrix_data)[1]*0.80 , replace = FALSE)
matrix_trainset <- ds_matrix_data[idx , ]
matrix_testset <- ds_matrix_data[-idx , ]



#we can now use the random forest with the various dummy variables
rf.fit <- randomForest(state ~ . , matrix_trainset[sample(dim(matrix_trainset)[1] , 50000) , ] , ntree = 500)

rf_preds <- predict(rf.fit , matrix_testset)
# Confusion Matrix of test set
rf_cf_matrix <- table(Actual = matrix_testset$state , Predictions = rf_preds)
rf_cf_matrix

cf_accuracy <- get_accuracy(rf_cf_matrix)

accuracy_results['Random Forest'] = cf_accuracy

#next, we'll try K Nearest Neighbors

knn_cl <- matrix_trainset[,ncol(matrix_trainset), drop = TRUE]
#knn.fit <- class::knn(train=lm_ds[-ncol(lm_ds)], test=test_ds[-ncol(test_ds)] , cl=knn_cl)

#this fails, since KNN expects all numerical factors, so more data manipulation is required.
knn.fit4 <- caret::knn3Train(train=matrix_trainset[-ncol(matrix_trainset)], test=matrix_testset[-ncol(matrix_testset)] ,
                            cl=knn_cl, k=4)

state_actual <- matrix_testset$state
knn_tbl <- table(state_actual, knn.fit)
knn_accuracy <- get_accuracy(knn_tbl)

# Compute the accuracy
#mean( state_actual == signs_pred)
accuracy_results['K Nearest Neighbors'] = knn_accuracy

#now we will try to use Naive Bayes Model
nb <- naive_bayes(state ~ ., lm_ds)
summary(nb)

# Classification
predict(nb, test_ds, type = "state")
nb %state% test_ds

# Posterior probabilities
predict(nb, test, type = "prob")
nb %prob% test

# Helper functions
tables(nb, 1)
get_cond_dist(nb)

nb_tbl <- table(state_actual, knn.fit)
nb_accuracy <- get_accuracy(knn_tbl)

accuracy_results['Naive Bayes'] = nb_accuracy


normalize <- function(x) {
  return((x - min(x)) / (max(x) - min(x)) )
}







