# Shares prediction

## Purpose
The purpose of this project is to show how you can build and use predictive models and automate Markdown reports. We used the OnlineNewsPopularity dataset
to then automate the EDA (exploratory data analysis) along with predictive modeling for the number of shares based on what data channel we are looking at.

## Required packages list

The following packages were required in this project:

`tidyverse`: an R package for data science, with tons of tools for data visualization, manipulation, and analysis

`caret`: Classification and Regression Training

`dplyr`: A tool for working with dataframes

`readr`: A tool to read in the file

`ggplot2`: A package to build graphs/visuals with

## Output Files
- The analysis for [social media is available here](Reports/data_channel_is_socmed.md)

# Introduction

We are going to analyze the Online News Popularity dataset. It is a dataset which is used to predict the number of shares and article published on Mashable.com got on social media sites.We are going to use the following variables to predict Number of Shares.
We are going to analyze the Online News Popularity dataset. It is a dataset which is used to predict the number of shares and article published on Mashable.com got on social media sites.We are going to use the following variables to predict Number of Shares.
*num_imgs: Number of images  
*num_videos: Number of videos  
*average_token_length: Average length of the words in the content and  
*title_sentiment_polarity: Title polarity  

To predict the Number of Shares, we will use linear regression and ensemble based tree methods, including random forest and boosted trees.

# Import and Subset data

I am going to import the dataset using a relative path and then subset it to only include data from the social media channel.

```{r}
library(readr)
library(dplyr)
setwd("C:\\Users\\awarhus_piusxi\\Desktop\\ST558")
data0<-read_csv("OnlineNewsPopularity\\OnlineNewsPopularity.csv")
data0<-subset(data0, data_channel_is_socmed == 1)
head(data)
```

# Split data into training and test set

```{r}
library(caret)
set.seed(8758)
trainIndex <- createDataPartition(data0$shares, 
                                  p = 0.7, list = FALSE)
data<- data0[trainIndex, ]
datatest <- data0[-trainIndex, ]
```

# EDA 

It is of interest to know what some information about the number of images and the number of videos are on the most shared media. To do this, first I will examine the 5 number summary for shares to develop an understanding of what "a lot" of shares is vs. "few".
```{r}
sharesSumm<-data %>% 
  summarize("Min"=min(shares),
            "1st Quartile"=quantile(shares,0.25),
            "Median"=quantile(shares,0.5),
            "3rd Quartile"=quantile(shares,0.75),
            "Max"=max(shares)
            )
knitr ::kable(sharesSumm)
```

Based on this information, I will categorize the numbers of shares in the following way:
*Less that 1400 shares is "few"
*Between 1400 and 3800 shares is "some"
*More than 3800 is "many"
I will add a categorical variable to the data to reflect this categorization and use it to visualize the number of images and then videos in media.

```{r}
library(ggplot2)
data<- data %>% 
  mutate(sharecategory = ifelse(shares <1400, "few",
                      ifelse(shares <=3800, "some",
                             "many")))
g<-ggplot(data=data,aes(x=num_imgs,fill=sharecategory))
g+geom_bar(position="dodge")+
  labs(x="Number of Images")+
  theme(legend.title=element_blank(),
        axis.text.x=element_text(angle=45))+
  facet_wrap(~sharecategory)+
  theme(legend.position = "None")

g<-ggplot(data=data,aes(x=num_videos, fill=sharecategory))
g+geom_bar(position="dodge")+
  labs(x="Number of Videos")+
  theme(legend.title=element_blank(),
        axis.text.x=element_text(angle=45))+
  facet_wrap(~sharecategory)+
  theme(legend.position = "None")
```
We can inspect the trend of number of images and videos and how it affects number of shares. If the tallest and most concentrated chunk of bars is in a different spot for each share category, then we can conclude that the number of images videos (depending on the graph you are looking at) is related to the number of shares. If each of the three graphs looks the same, then we would conclude that images and videos do not necessarily impact the number of shares. 
```

I hypothesize that the shorter the average word length, the more popular a media item will be. So we will analyze word length next. First, let's get the mean and standard deviation of word length in all of the media items. Next, we can look at how word length differs based on share category.

```{r}
wordSumm<-data %>% 
  summarize("Mean"=mean(average_token_length),
            "Standard Deviation"=sd(average_token_length))
knitr ::kable(wordSumm)

wordSumm2<-data %>% group_by(sharecategory) %>%
  summarize("Mean"=mean(average_token_length),
            "Standard Deviation"=sd(average_token_length))
knitr ::kable(wordSumm2)
```

Finally, we'll explore the title polarity vs. the share category.

```{r}
g<-ggplot(data=data,aes(title_sentiment_polarity,color=title_sentiment_polarity))
g+geom_bar(aes(fill=title_sentiment_polarity),position="dodge")+labs(x="Title Polarity")+theme(legend.title=element_blank(), axis.text.x=element_text(angle=45))+scale_y_continuous(limits=c(0,35))+facet_wrap(~sharecategory)
```

# Linear Regression

I will analyze a model that includes all terms and all possible interactions. I will analyze this model using 10 fold cross validation.

```{r}
fit1<-lm(shares~num_imgs
                  *num_videos
                  *average_token_length
                  *title_sentiment_polarity
                  ,data = data)
summary(fit1)
train1<-train(shares~num_imgs
                  *num_videos
                  *average_token_length
                  *title_sentiment_polarity,
              data=data,
              method="lm",
              preProcess=c("center","scale"),
              trControl=trainControl(method="cv",number=10))
train1$results
```


# Ensemble Methods

## Boosted Tree Model

Boosting is a process where models are trained sequentially. The focus is put on situations where the model fails to predict correctly a statistically significant amount of the time. These models are given more weight so that they are more likely to appear in any given sample. Therefore, the tree will focus on getting these correct and therefore improve the overall prediction. As this process continues, the model gets stronger and stronger.Boosting is particularly robust against overfitting.

Here I will do boosting with 5 fold repeated cross validation (3 times). I will then print the confusion matrix on the test set.

```{r}
ctrl <- trainControl(method="repeatedcv",number=5, repeats = 3)
boostFit <- train(shares~num_imgs
                  +num_videos
                  +average_token_length
                  +title_sentiment_polarity
                  ,data = data, 
                method = "gbm", trControl = ctrl, 
                preProcess = c("center","scale"), 
                tuneGrid =  expand.grid(n.trees = 25, 
                              shrinkage = 0.1,
                              interaction.depth = c(1:4),
                              n.minobsinnode = 10))
confusionMatrix(data=datatest$shares,reference=predict(boostFit,newdata=datatest$shares))

# Automation from markdown

```{r}
library(tidyverse)
library(rmarkdown)
channelID<-c("data_channel_is_lifestyle",
             "data_channel_is_entertainment",
             "data_channel_is_bus",
             "data_channel_is_socmed",
             "data_channel_is_tech",
             "data_channel_is_world")
output_file<-paste0(channelID,".md")
params = lapply(channelID, FUN = function(x){list(channel = x)})
reports<-tibble(output_file,params)
apply(reports, MARGIN = 1, 
            FUN = function(x){
                render(input = "C:\\Users\\awarhus_piusxi\\Desktop\\ST558\\Project2\\Project2.rmd", output_file = x[[1]], params = x[[2]])
                })
```


```
