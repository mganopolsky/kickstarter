## ---- libraries --------

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
if(!require(GGally)) install.packages("GGally", repos = "http://cran.us.r-project.org")
if(!require(e1071)) install.packages("e1071", dependencies=TRUE)
if(!require(rpart)) install.packages("rpart", dependencies=TRUE)
if(!require(rpart.plot)) install.packages("rpart.plot", dependencies=TRUE)
if(!require(MASS)) install.packages("MASS", dependencies=TRUE)
if(!require(readr)) install.packages("readr", dependencies=TRUE)

library(readr)
library(MASS)
library(rpart)
library(rpart.plot)
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
library(scales)

## ---- load_data --------
#the data is stored in my github account, available publically.
file_path <- "https://raw.githubusercontent.com/mganopolsky/kickstarter/master/data/ks-projects-201801.csv"
data  <-read_csv(file_path)


## ---- glimpse_data --------
glimpse(data)

## ---- massage_data --------
#according to the documentatino, `usd pledged` isn't needed since there is the usd_pledged_real, so I'll remove it
ds <- data %>% dplyr::select(-`usd pledged`)

#Add a campaign length in days (time_int), pledged ratio (how much has been pledged vs what the goal is), 
#as well as the average pledge per backer, per project
#I will also like to explore whether the month or the day of the week of the posted project mught have something to do with its succeess, 
#so I have added the launched_month and launched_day_of_week columns.
ds <- ds %>% mutate(time_int = as.numeric(deadline - as.Date(launched)) ,
                    launched = as.Date(launched),
                    pledged_ratio = round(usd_pledged_real / usd_goal_real, 2),
                    avg_backer_pldg = ifelse(backers == 0, 0, round(usd_pledged_real/backers) )) %>%
            mutate(launched_month = as.factor(format(launched, "%m")),
                    launched_day_of_week = as.factor(format(launched, "%u")  ),
                   currency = as.factor(currency),
                   launched_year = as.factor(format(launched, "%Y")))


#############################33
#Exploratory Data Analysis (EDA)



#let's see if there are missing values in the data
## ---- summary_ds --------
summary(ds)

#noticing something funny - the launched dates start in 1970 - let's look closer
## ---- launched_ds --------
ds %>% distinct(launched) %>% arrange(launched) %>% head()

## ---- launched_1970 --------
ds %>% filter(launched=='1970-01-01') %>% 
  dplyr::select(launched, name, state, category, deadline, backers, goal)

## ---- delete_1970 --------
#let's remove the dates on 1/1/1970
ds <- ds %>% filter(launched >= "2009-04-21")

#let's look at items with a pledge ration of less then 1
## ---- pledge_less_1 --------
failed <- ds %>% filter(pledged_ratio < 1) %>% 
  dplyr::select(name,  state, category, deadline, goal, pledged, usd_pledged_real, usd_goal_real, pledged_ratio)
failed %>% distinct(state)
#it appeaers that some are succeessful despite having not met their goal!
## ---- pledge_less_2 --------
failed %>% filter(state=="successful")

#interesting , but both proejcets raised a significant maount of money and continued on. we'll leave them in

#what's the density distribution of the pledge ratio that most successful projectsc have?

## ---- ECDF --------
ds %>% filter(state=="successful") %>% 
  ggplot(aes(x=pledged_ratio)) + 
  stat_ecdf() + 
  scale_x_continuous(trans="pseudo_log", breaks = c(10, 100, 1000, 10000, 100000), labels=comma) + 
  scale_y_continuous(labels=percent) + 
  theme_bw() + 
  labs(x="Pledged Ratio", y="Percentage of Projects", title = "Cumulative Distribution of Pledge Ratios for Succeessful Projects")


#Now we can examine the fields
## ---- glimpse2 --------
glimpse(ds)

#It looks like most of the fields can be made into factors
## ---- factors3 --------
ds$category <- as.factor(ds$category)
ds$main_category <- as.factor(ds$main_category)
ds$currency <- as.factor(ds$currency)
ds$state <- as.factor(ds$state)
ds$country  <- as.factor(ds$country )
ds$launched_month <- as.factor(ds$launched_month)
ds$launched_day_of_week <- as.factor(ds$launched_day_of_week)
ds$launched_year <- as.factor(ds$launched_year)
#let's look at the levels of each of these factors
## ---- levels --------
levels(ds$category)
levels(ds$main_category)
levels(ds$currency)
levels(ds$state)
levels(ds$launched_month)
levels(ds$launched_day_of_week)
levels(ds$launched_year)
levels(ds$country)

## ---- filtere_N0 --------
#Country "N,0\"" seems strange - we'll keep an eye out for that later.
#Let's remove those
ds <- ds %>% filter(country != "N,0\"")
glimpse(ds)



#Which main_category projects are most frequently proposed?

## ---- freq --------
freq <- ds %>%
  group_by(main_category) %>%
  summarize(count=n()) %>%
  arrange(desc(count))

freq$main_category <- factor(freq$main_category, levels=freq$main_category)

freq %>% ggplot( aes(main_category, count, fill=count)) + geom_bar(stat="identity") + 
  ggtitle("Projects by Main Category") + xlab("Main Category") + ylab("Project Count") + 
  theme(plot.title=element_text(hjust=0.5), axis.title=element_text(size=12, face="bold"),axis.text.x=element_text(size=12, angle=90))

#In terms of the main categorie, Film & video projects seem to be the most wide-spreead, while Dance is the least.

#what does this look like in terms of money raised, per main category?
## ---- raised --------
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
## ---- raised_by_sub_code --------
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

## ---- proj_by_state --------
ds %>% group_by(state) %>% summarize(count = n(), percentage = paste(round(count* 100/(dim(ds)[1]) ), "%", sep="")) %>%
  arrange(desc(count)) %>%  ggplot(aes(x = reorder(state , desc(count)) , y = count , fill = state , label = percentage)) + 
  geom_col() + geom_text(size = 6 , vjust = -0.2) + 
  labs(x = '' , y = 'Count', title = "Cumulative Projects By Status") + 
  theme_classic() + theme(legend.position = 'none') + 
  theme(axis.text.x = element_text(size = 13 , face = 'bold'))

#Let's examine the distribution of successful vs failed variables 

## ---- medians_log_distribution --------
mean_median_pledged <- ds %>% filter(state %in% c('successful' , 'failed')) %>% group_by(state) %>% 
  summarize(mean = round(mean(usd_pledged_real)), median = round(median(usd_pledged_real)), max=round(max(usd_pledged_real))) 

mean_median_goal <- ds %>% filter(state %in% c('successful' , 'failed')) %>% group_by(state) %>% 
  summarize(mean = round(mean(usd_goal_real)), median = round(median(usd_goal_real)), max=round(max(usd_goal_real))) 

ds1 <- ds %>% filter(state %in% c('successful' , 'failed')) %>% dplyr::select(state , usd_pledged_real) %>%
  ggplot(aes(usd_pledged_real , fill = state)) + geom_density(alpha = 0.65) + 
  labs(x = 'Pledged Amount (USD)' , title = 'Pledged Value Distribution') +
  scale_x_discrete(labels=function(l) {    paste0(round(l/1e6,2),"m")  }) 

ds2 <- ds %>% filter(state %in% c('successful' , 'failed')) %>% dplyr::select(state , usd_pledged_real) %>%
  ggplot(aes(usd_pledged_real +1 , fill = state)) + geom_density(alpha = 0.65) + 
  scale_x_log10(labels=function(l) {    paste0(round(l/1e3,2),"k")  }) +
  labs(x = "log-tranform Goal Amount (K's USD)", title = "Logarithmic Pledged Goal Distribution in $K's") + 
  theme(legend.position = 'none') +
  geom_vline(data = mean_median_pledged, aes(xintercept = median,  linetype = state), size=1, color="darkred") + 
  geom_vline(data = mean_median_pledged, aes(xintercept = mean,  linetype = state), size=1, color="darkgreen") +
  
  geom_text_repel(data = mean_median_pledged %>% dplyr::select(state, mean), 
                  aes(x=mean, y =1, label=paste(state, " mean \nof $",mean), color=state, angle=30), nudge_x = 5) +
  geom_text_repel(data = mean_median_pledged %>% dplyr::select(state, median), 
                  aes(x=median, y =.5, label=paste(state, " median \nof $",median), color=state, angle=30),
                   nudge_x = -5) 
  
ds3 <- ds %>% filter(state %in% c('successful' , 'failed')) %>% dplyr::select(state , usd_goal_real) %>%
  ggplot(aes(usd_goal_real , fill = state)) + geom_density(alpha = 0.65) + 
  labs(x = 'Goal Amount (USD)' , title = 'Goal Value Distribution') + 
  scale_x_discrete(labels=function(l) {    paste0(round(l/1e6,2),"m")  }) 

ds4 <- ds %>% filter(state %in% c('successful' , 'failed')) %>% dplyr::select(state , usd_goal_real) %>%
  ggplot(aes(usd_goal_real +1 , fill = state)) + geom_density(alpha = 0.65) + 
  scale_x_log10(labels=function(l) {    paste0(round(l/1e6,2),"m")  }) +
  labs(x = "log-tranform Goal Amount (MM's USD)", title = "Logarithmic Pledged Goal Distribution in $MM's") + 
  theme(legend.position = 'none') +
  geom_vline(data = mean_median_goal, aes(xintercept = median,  linetype = state), size=1, color="darkred") + 
  geom_vline(data = mean_median_goal, aes(xintercept = mean,  linetype = state), size=1, color="darkgreen") +
  
  geom_text_repel(data = mean_median_goal %>% dplyr::select(state, mean), 
                  aes(x=mean, y =1, label=paste(state, " mean \nof $",mean), color=state, angle=30), nudge_x = 10) +
  geom_text_repel(data = mean_median_goal %>% dplyr::select(state, median), 
                  aes(x=median, y =.5, label=paste(state, " median \nof $",median), color=state, angle=30),
                  nudge_x = -5, nudge_y = 3 ) 


grid.arrange(ds1 , ds2 , ds3 , ds4 )
            
#Both the successful pledged and the goal amount seem to be pretty normally distributed on the logarithmic scale.
#It looks like the higher the pledged amount, the higher the chance of success. 
#HOwever, it also looks like the highere the goal, the less chance of success there is. 

## ---- pledged_ratios --------
ds %>% dplyr::filter(state %in% c('successful' , 'failed')) %>%
  ggplot(aes(x = state , y = pledged_ratio , fill = state)) + geom_boxplot(alpha = 0.8) + 
  ylim(0 , 1.5) + theme_classic() + theme(legend.position = 'none') +
   labs(x = '' , y = 'Pledged Ratio', title="Pledge Ratios Box Plot By State")

# Quantiles of failed projects pledged_ratio
## ---- failed_quantiled --------
round(quantile(ds$pledged_ratio[ds$state == 'failed'] , probs = seq(0.1 , 0.9 , 0.1), na.rm = TRUE) , 4)
#Kickstarter has a requirement of 50% funding for a project. Here, we can see that the higherst quantile doens't even get into the 30%+ range

# Quantiles of succesesful projects pledged_ratio - all over 100%
## ---- suc_quantiled --------
round(quantile(ds$pledged_ratio[ds$state == 'successful'] , probs = seq(0.1 , 0.9 , 0.1), na.rm = TRUE) , 4)
#Kickstarter has a requirement of 50% funding for a project. Here, we can see that the lowest quantile is over 100%, which is more then double the required limit!



#correlation of some of the features and overview of their relationship is shown here. The only items correlated very slightly are the time interval and the goal amount in USD. This makes sense intuitively as the planners expect to raise some amount of money in a specific time. 
## ---- ggpairs --------
ds %>% dplyr::select(state, usd_goal_real , time_int, launched_month, launched_day_of_week, launched_year) %>% 
  ggpairs(title="Kickstarter Data Set Info & Correlation")

#It looks like the only real correlation - .753 is shown between pledged amounts and the amount of backers. 
#This makes sense 

#first focus in only on the failed or succeessful projects
#ds <- ds %>% filter(state %in% c("failed", "successful"))

#create a binary field to show failed / successful campaign

#select all the necessary columns, with the "state" being the last one - important for the matrix of dummy variables  later
## ---- final_ds_selection --------
ds <- ds %>% dplyr::select(category ,  country , usd_goal_real ,  time_int ,  launched_day_of_week, 
                           launched_month, currency, launched_year , state) %>% 
  filter(state %in% c('successful' , 'failed')) %>% 
  mutate(state = as.factor(ifelse(state == 'successful' , 1 , 0))) %>%
  mutate_if(is.character , as.factor)


#function that will take in a testing and training dataset, as well as the field we are trying to predict, a list of predictors, and a model family if needed (defaults to binomail)
#this function will return a trained model based on the predictors and predicted field, which will be dynamically built. ( I will be running 7 diffeerent GLM models here and they will all be dynamically generated)

## ---- GLM_functions --------
results_GLM <- function(train_data, predicted_field_name, 
                        predictors_list, model_family=binomial) {
  fm_string <- paste(predicted_field_name, "~")
  for (i in 1:length(predictors_list)) {
    fm_string <- paste(fm_string, predictors_list[i])
    if (i < length(predictors_list))
      fm_string <- paste(fm_string, "+")
  }
  print(fm_string)
  fm <- as.formula(fm_string)
  
  glm_fit <- glm(fm , train_data, family = model_family)
  return (glm_fit)
}


get_confusion_matrix <- function(model_fit, test_data)
{
  aug_model <- augment(model_fit, newdata = test_data, 
                       type.predict = 'response')
  
  result <- aug_model %>% 
    mutate(Prediction = factor(round(.fitted)), Reference = state) %>%   
    dplyr::select(Reference , Prediction ) %>% table()
  
  return (caret::confusionMatrix(result))
}

get_accuracy <- function(cf_matrix) 
{
  cf_accuracy <- sum(diag(cf_matrix)) / sum(cf_matrix)
  return(cf_accuracy)
}

## ---- data_split --------
#since the outcome is a factor and not just numeric, we can't use simple linear regression to calculate this. we need to use GLM
#predictions based solely on category
set.seed(1, sample.kind="Rounding")
#selecting a 20% test set vs 80% training set
test_lm_index <- createDataPartition(y = ds$state, times = 1, p = 0.2) %>% unlist()
lm_ds <- ds[-test_lm_index,]
test_ds <- ds[test_lm_index,]

#classification trees model
## ---- ct_model --------
ct_model <- rpart(state ~ ., data = lm_ds, method = "class", control = rpart.control(maxdepth = 7, minsplit=100))
ct_pred <- predict(ct_model, test_ds, type = "class")
#rpart.plot(ct_model)

# Plot the ct_model with customized settings
rpart.plot(ct_model, type = 3, box.palette = c("red", "green"), fallen.leaves = TRUE)
ct_table <- table(test_ds$state, ct_pred)
# Compute the accuracy on the test dataset
ct_accuracy <- get_accuracy(ct_table)
#accuracy_results['Classification Trees'] <-  ct_accuracy
model_results <- tibble(model = "Classification Trees mex depth=7,minsplit=100", accuracy = ct_accuracy) 



## ---- ct_pruned_model --------
# this accuracy of ct_accuracy is pretty low; but what if we try post-pruning the tree, 
#initially running with complexity parameter = 0, and then pruning the tree where the CP ends up being the lowest
ct_prune_model <- rpart(state ~ ., data = lm_ds, method = "class", control = rpart.control(cp=0))
#we examing the complexity parameter with the following complexity plot.
plotcp(ct_prune_model, minline = TRUE)
#now we find the actual min cp

#It's obvious here that the X-val relative error is around 0.8. We find that exact value, and prune the tree around the correstponding cp value.

cp.select <- function(big.tree) {
  min.x <- which.min(big.tree$cptable[, 4]) #column 4 is xerror
  for(i in 1:nrow(big.tree$cptable)) {
    if(big.tree$cptable[i, 4] < big.tree$cptable[min.x, 4] + big.tree$cptable[min.x, 5]) return(big.tree$cptable[i, 1]) #column 5: xstd, column 1: cp 
  }
}

cp <- cp.select(ct_prune_model)
pruned.tree <- prune(ct_prune_model, cp = cp)
ct_pred_pruned <- predict(pruned.tree, test_ds, type = "class")
ct_pruned_table <- table(test_ds$state, ct_pred_pruned)
ct_pruned_accuracy <- get_accuracy(ct_pruned_table)
cf_p_model_output <- tibble(model = paste('Classification Trees, default parameters & cp=0, post-pruned with min cp=', cp, sep=""), accuracy = ct_pruned_accuracy) 
model_results <- bind_rows(model_results, cf_p_model_output)


## ---- ct_model_2 --------
ct_model_2 <- rpart(state ~ ., data = lm_ds, method = "class")
ct_pred_2 <- predict(ct_model_2, test_ds, type = "class")

# Plot the ct_model with customized settings
ct_table2 <- table(test_ds$state, ct_pred_2)
ct_accuracy2 <- get_accuracy(ct_table2)
cf_p2_model_output <- tibble(model = "Classification Trees, default params, un-pruned", accuracy = ct_accuracy2) 
model_results <- bind_rows(model_results, cf_p2_model_output)

cp <- cp.select(ct_model_2)
pruned2.tree <- prune(ct_model_2, cp = cp)
ct_pred_pruned2 <- predict(pruned2.tree, test_ds, type = "class")
ct_pruned_table2 <- table(test_ds$state, ct_pred_pruned2)
ct_pruned_accuracy2 <- get_accuracy(ct_pruned_table2)

cf_p2_model_pruned_output <- tibble(model = paste('Classification Trees, default params, post-pruned cp=', cp, sep=""), accuracy = ct_pruned_accuracy2) 
model_results <- bind_rows(model_results, cf_p2_model_pruned_output)

## ---- randomForest --------
matrix_trainset <- data.frame(model.matrix( ~ . -1 , lm_ds))
matrix_testset <- data.frame(model.matrix( ~ . -1 , test_ds))
colnames(matrix_trainset)[ncol(matrix_trainset)] = 'state'
colnames(matrix_testset)[ncol(matrix_testset)] = 'state'
matrix_trainset$state = as.factor(matrix_trainset$state)
matrix_testset$state = as.factor(matrix_testset$state)

#set.seed(123)
#we can now use the random forest with the various dummy variables
#depending on how much memory the computer running this code has, use the following lines below. If not too much, uncomment the next 
#line and use with a sample of 100,000. If enough memory is available, there is no need to sample the matrix.
#rf.fit <- randomForest::randomForest(formula=state ~ . , data=matrix_trainset[sample(dim(matrix_trainset)[1] , 100000) , ] , ntree = 200)
rf.fit <- randomForest::randomForest(formula=state ~ . , data=matrix_trainset , ntree = 200)

rf_preds <- predict(rf.fit , matrix_testset)
rf_cf_matrix <- table(Actual = matrix_testset$state , Predictions = rf_preds)
rf_accuracy <- get_accuracy(rf_cf_matrix)
rf_model_output <- tibble(model = "Random Forest, ntree=200", accuracy = rf_accuracy) 
model_results <- bind_rows(model_results, rf_model_output)




#next, we'll try K Nearest Neighbors


#K Nearest Neighbors retursn a decent response - however, it takes 5 hours to run. will skip it here.
#knn_cl <- matrix_trainset[,ncol(matrix_trainset), drop = TRUE]
#knn.fit <- class::knn(train=lm_ds[-ncol(lm_ds)], test=test_ds[-ncol(test_ds)] , cl=knn_cl)


#knn algorithm geets a decent return for k=45. however, this takes many many hours. 
#knn.fit45 <- caret::knn3Train(train=matrix_trainset[-ncol(matrix_trainset)], test=matrix_testset[-ncol(matrix_testset)] ,
#                             cl=knn_cl, k=45)

#state_actual <- matrix_testset$state
#knn_tbl45 <- table(state_actual, knn.fit45)
#knn_accuracy45 <- get_accuracy(knn_tbl45)
#knn_model_output <- tibble(model = "K Nearest Neighbors - k=45", accuracy = knn_accuracy45) 
#model_results <- bind_rows(model_results, knn_model_output)


#now we will try to use Naive Bayes Model
## ---- nb_default --------
nb <- naive_bayes(state ~ ., lm_ds, laplace = 1)
summary(nb)

# Classification
nb_fit <- predict(nb, test_ds, type = "class")
nb_tbl <- table(test_ds$state, nb_fit)
nb_accuracy <- get_accuracy(nb_tbl)
nb_model_output <- tibble(model = "Naive Bayes", accuracy = nb_accuracy) 
model_results <- bind_rows(model_results, nb_model_output)

#trying NB with matrix dummy variables
#now we will try to use Naive Bayes Model
## ---- nb_matrix --------
nb_m <- naive_bayes(state ~ ., data=matrix_trainset, type="class")
nb_m_fit <- predict(nb_m, matrix_testset[-ncol(matrix_testset)], type = "class")
nb_m_tbl <- table(matrix_testset$state, nb_m_fit)
nb_m_accuracy <- get_accuracy(nb_m_tbl)
nb_d_model_output <- tibble(model = "Naive Bayes w/matrix dummy variables", accuracy = nb_m_accuracy) 
model_results <- bind_rows(model_results, nb_d_model_output)


## ---- glm_1 --------
columns <- colnames(ds)
col_subset <- columns[columns %in% c("category")]
glm_1 <- results_GLM(lm_ds,  "state", col_subset)
cf1 <- get_confusion_matrix(model_fit= glm_1, test_data = test_ds)
cf1_accuracy <- get_accuracy(cf1$table)
cf1_model_output <- tibble(model = "GLM 1 predictor", accuracy = cf1_accuracy) 
model_results <- bind_rows(model_results, cf1_model_output)

#################
#category AND time interval in days

## ---- glm_2 --------
col_subset <- columns[columns %in% c("category", "time_int")]

glm_2 <- results_GLM(lm_ds, "state", col_subset)
cf2 <- get_confusion_matrix(model_fit= glm_2, test_data = test_ds)
cf2_accuracy <- get_accuracy(cf2$table)
#accuracy_results['GLM cf2'] =  cf2_accuracy
cf2_model_output <- tibble(model = "GLM 2 predictors", accuracy = cf2_accuracy) 
model_results <- bind_rows(model_results, cf2_model_output)


#################
#category AND time AND country interval in days
## ---- glm_3 --------
col_subset <- columns[columns %in% c("category", "time_int", "country")]

glm_3 <- results_GLM(lm_ds, "state", col_subset)
cf3 <- get_confusion_matrix(model_fit= glm_3, test_data = test_ds)
cf3_accuracy <- get_accuracy(cf3$table)
#accuracy_results['GLM cf3'] =  cf3_accuracy
cf3_model_output <- tibble(model = "GLM 3 predictors", accuracy = cf3_accuracy) 
model_results <- bind_rows(model_results, cf3_model_output)


##############
## ---- glm_4 --------
col_subset <- columns[columns %in% c("category", "time_int", "country", "usd_goal_real")]

glm_4 <- results_GLM(lm_ds, "state", col_subset)
cf4 <- get_confusion_matrix(model_fit= glm_4, test_data = test_ds)
cf4_accuracy <- get_accuracy(cf4$table)
#accuracy_results['GLM cf4'] =  cf4_accuracy
cf4_model_output <- tibble(model = "GLM 4 predictors", accuracy = cf4_accuracy) 
model_results <- bind_rows(model_results, cf4_model_output)

##############
## ---- glm_5 --------
col_subset <- columns[columns %in% c("category", "time_int", "country", "usd_goal_real", "launched_month")]

glm_5 <- results_GLM(lm_ds, "state", col_subset)
cf5 <- get_confusion_matrix(model_fit= glm_5, test_data = test_ds)
cf5_accuracy <- get_accuracy(cf5$table)
#accuracy_results['GLM cf5'] <-  cf5_accuracy
cf5_model_output <- tibble(model = "GLM 5 predictors", accuracy = cf5_accuracy) 
model_results <- bind_rows(model_results, cf5_model_output)

##############
## ---- glm_6 --------
col_subset <- columns[columns %in% c("category", "time_int", "country", "usd_goal_real", "launched_month", "launched_day_of_week")]

glm_6 <- results_GLM(lm_ds, "state", col_subset)
cf6 <- get_confusion_matrix(model_fit= glm_6, test_data = test_ds)
cf6_accuracy <- get_accuracy(cf6$table)
#accuracy_results['GLM cf6'] =  cf6_accuracy
cf6_model_output <- tibble(model = "GLM 6 predictors", accuracy = cf6_accuracy) 
model_results <- bind_rows(model_results, cf6_model_output)

## ---- glm_7 --------
col_subset <- columns[columns %in% c("category", "time_int", "country", "usd_goal_real", "launched_month", "launched_day_of_week", "currency")]

glm_7 <- results_GLM(lm_ds, "state", col_subset)
cf7 <- get_confusion_matrix(model_fit= glm_7, test_data = test_ds)
cf7_accuracy <- get_accuracy(cf7$table)
#accuracy_results['GLM cf7'] =  cf7_accuracy
cf7_model_output <- tibble(model = "GLM 7 predictors", accuracy = cf7_accuracy) 
model_results <- bind_rows(model_results, cf7_model_output)

## ---- glm_8 --------
col_subset <- columns[columns %in% c("category", "time_int", "country", "usd_goal_real", "launched_month", "launched_day_of_week", "currency", "launched_year")]
glm_8 <- results_GLM(lm_ds, "state", col_subset)
cf8 <- get_confusion_matrix(model_fit= glm_8, test_data = test_ds)
cf8_accuracy <- get_accuracy(cf8$table)
#accuracy_results['GLM cf8'] =  cf8_accuracy
cf8_model_output <- tibble(model = "GLM 8 predictors", accuracy = cf8_accuracy) 
model_results <- bind_rows(model_results, cf8_model_output)


## ---- glm_9 --------
#Since we know from GLM7 that the "currency" variable isn't helpful, how will it affect the previous model if we remove it?
col_subset <- columns[columns %in% c("category", "time_int", "country", "usd_goal_real", "launched_month", "launched_day_of_week", "launched_year")]
glm_9 <- results_GLM(lm_ds, "state", col_subset)
cf9 <- get_confusion_matrix(model_fit= glm_9, test_data = test_ds)
cf9_accuracy <- get_accuracy(cf9$table)
#accuracy_results['GLM cf8'] =  cf8_accuracy
cf9_model_output <- tibble(model = "GLM 9 predictors", accuracy = cf9_accuracy) 
model_results <- bind_rows(model_results, cf9_model_output)


#model_results <- cf9_model_output


#sorted model results by accuracy 

## ---- final_results --------
model_results <- model_results %>% arrange(desc(accuracy))
top_model <- model_results[which.max(model_results$accuracy),]














