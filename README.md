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
- The analysis for [social media is available here](Social Media Analysis.html)
- The analysis for [entertainment is available here](Entertainment Analysis.html)
- The analysis for [lifestyle is available here](Lifestyle Analysis.html)
- The analysis for [business is available here](Business Analysis.html)
- The analysis for [world is available here](World Analysis.html)
- The analysis for [technology is available here](Tech Analysis.html)

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
```
## Automation from Markdown

```{r,eval = FALSE}

library(tidyverse)
library(rmarkdown)
channelID<-list("data_channel_is_lifestyle",
             "data_channel_is_entertainment",
             "data_channel_is_bus",
             "data_channel_is_socmed",
             "data_channel_is_tech",
             "data_channel_is_world")

for (channel in c(0,1,2,3,4,5,6)){
  rmarkdown::render(
    "Project_2.Rmd",
    output_file=paste0( channelID[[channel+1]]),
    params = list(
    channel = channel,
    name = channelID[[channel+1]]
    )
  )
}
```



