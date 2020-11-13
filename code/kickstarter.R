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

data_dir <- paste(getwd(), "/data/", sep = "")

ds <- read_csv(paste(data_dir, "ks-projects-201801.csv", sep = ""))

glimpse(ds)
#There aree 378,661 rows

#Let's rename some cuomns for consistancy to remove blanks
#according to the documentatino, `usd pledged` isn't needed, so we'll remove it
ds <- ds %>% select(-`usd pledged`)

#Add a campaign length in days
ds <- ds %>% mutate(campaign_length_days = deadline - as.Date(launched)  )


#Exploratory Data Analysis (EDA)

#let's see if there are missing valuese
summary(ds)

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
  ggtitle("Projects by Main Category") + xlab("Sub Category / Main Category") + ylab("Project Count") + 
  theme(plot.title=element_text(hjust=0.5), axis.title=element_text(size=12, face="bold"), 
        axis.text.x=element_text(size=12, angle=90))


#film/video are the most popular while Dance is the least popular in terms of hard numbers. 
#How do these numbers break down in terms of states of projects?


