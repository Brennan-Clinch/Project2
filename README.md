# Shares prediction

## Purpose
The purpose of this project is to show how you can build and use predictive models and automate Markdown reports. We used the OnlineNewsPopularity dataset
to then automate the EDA (exploratory data analysis) along with predictive modeling for the number of shares based on what data channel we are looking at.

## Required packages list

The following packages were required in this project:

`tidyverse`: an R package for data science, with tons of tools for data visualization, manipulation, and analysis

`caret`: Classification and Regression Training

## Output files

## Automation from markdown


# Introduction

We are going to analyze the Online News Popularity dataset. It is a dataset which is used to predict the number of shares and article published on Mashable.com got on social media sites.We are going to use the following variables to predict Number of Shares.
*num_imgs: Number of images
*num_videos: Number of videos
* average_token_length: Average length of the words in the content
*weekday_is_ __: indicator that tells which day of the week the article was published on and
*title_sentiment_polarity: Title polarity

To predict the Number of Shares, we will use linear regression and ensemble based tree methods, including random forest and boosted trees.

# Import and Subset data

I am going to import the dataset using a relative path and then subset it to only include data from the social media channel.

```{r}
library(readr)
library(dplyr)
setwd("C:\\Users\\awarhus_piusxi\\Desktop\\ST558")
data<-read_csv("OnlineNewsPopularity\\OnlineNewsPopularity.csv")
data<-subset(data, data_channel_is_socmed == 1)
head(data)
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
g<-ggplot(data=data,aes(num_imgs,color=num_imgs))
g+geom_bar(aes(fill=num_imgs),position="dodge")+labs(x="Number of Images")+theme(legend.title=element_blank(), axis.text.x=element_text(angle=45))+facet_wrap(~sharecategory)

g<-ggplot(data=data,aes(num_videos,color=num_videos))
g+geom_bar(aes(fill=num_videos),position="dodge")+labs(x="Number of Videos")+theme(legend.title=element_blank(), axis.text.x=element_text(angle=45))+facet_wrap(~sharecategory)
```
