Lifestyle Analysis analysis
================
Brennan Clinch
10/29/2021

# Introduction

We are going to analyze the Online News Popularity dataset. It is a
dataset which is used to predict the number of shares and article
published on Mashable.com got on social media sites.We are going to use
the following variables to predict Number of Shares. We are going to
analyze the Online News Popularity dataset. It is a dataset which is
used to predict the number of shares and article published on
Mashable.com got on social media sites.We are going to use the following
variables to predict Number of Shares. *num\_imgs: Number of images  
*num\_videos: Number of videos  
*average\_token\_length: Average length of the words in the content
and  
*title\_sentiment\_polarity: Title polarity

To predict the Number of Shares, we will use linear regression and
ensemble based tree methods, including random forest and boosted trees.

# Import and Subset data

``` r
channelIDs <- unique(rawDataNew$data_channel)

output_file <- paste0(channelIDs, ".md")

params = lapply(channelIDs, FUN = function(x){list(data_channel = x)})

reports <- tibble(output_file, params)

library(rmarkdown)
apply(reports, MARGIN = 1, 
            FUN = function(x){
                render(input = "Project2.Rmd", output_file = x[[1]], params = x[[2]])
                })
```

``` r
## Read in Raw Data Using Relative Path
rawData <- read_csv("OnlineNewsPopularity.csv") 
rawData
```

    ## # A tibble: 39,644 x 61
    ##    url             timedelta n_tokens_title n_tokens_content n_unique_tokens
    ##    <chr>               <dbl>          <dbl>            <dbl>           <dbl>
    ##  1 http://mashabl~       731             12              219           0.664
    ##  2 http://mashabl~       731              9              255           0.605
    ##  3 http://mashabl~       731              9              211           0.575
    ##  4 http://mashabl~       731              9              531           0.504
    ##  5 http://mashabl~       731             13             1072           0.416
    ##  6 http://mashabl~       731             10              370           0.560
    ##  7 http://mashabl~       731              8              960           0.418
    ##  8 http://mashabl~       731             12              989           0.434
    ##  9 http://mashabl~       731             11               97           0.670
    ## 10 http://mashabl~       731             10              231           0.636
    ## # ... with 39,634 more rows, and 56 more variables: n_non_stop_words <dbl>,
    ## #   n_non_stop_unique_tokens <dbl>, num_hrefs <dbl>, num_self_hrefs <dbl>,
    ## #   num_imgs <dbl>, num_videos <dbl>, average_token_length <dbl>,
    ## #   num_keywords <dbl>, data_channel_is_lifestyle <dbl>,
    ## #   data_channel_is_entertainment <dbl>, data_channel_is_bus <dbl>,
    ## #   data_channel_is_socmed <dbl>, data_channel_is_tech <dbl>,
    ## #   data_channel_is_world <dbl>, kw_min_min <dbl>, kw_max_min <dbl>, ...

``` r
## Create a New Variable to Data Channel to use when automating.
rawDataNew <- rawData %>% mutate(data_channel =   if_else(data_channel_is_bus == 1, "Business Analysis",
       if_else(data_channel_is_entertainment == 1, "Entertainment Analysis",
               if_else(data_channel_is_lifestyle == 1, "Lifestyle Analysis",
                      if_else(data_channel_is_socmed == 1, "Social Media Analysis",
                              if_else(data_channel_is_tech == 1, "Tech Analysis", "World Analysis"))))))
## Subset Data for Respective Data Channel
subsetData <- rawDataNew %>% filter(data_channel == params$data_channel)
## Create Training and Test Data Sets
set.seed(500)
trainIndex <- createDataPartition(subsetData$shares, p = 0.7, list = FALSE)
trainData <- subsetData[trainIndex,]
testData <- subsetData[-trainIndex,]
trainData
```

    ## # A tibble: 1,472 x 62
    ##    url             timedelta n_tokens_title n_tokens_content n_unique_tokens
    ##    <chr>               <dbl>          <dbl>            <dbl>           <dbl>
    ##  1 http://mashabl~       731             11              103           0.689
    ##  2 http://mashabl~       731             10              243           0.619
    ##  3 http://mashabl~       731              8              204           0.586
    ##  4 http://mashabl~       730             12              499           0.513
    ##  5 http://mashabl~       729             11              223           0.662
    ##  6 http://mashabl~       729             11             1099           0.412
    ##  7 http://mashabl~       729             14              318           0.633
    ##  8 http://mashabl~       729              7              144           0.723
    ##  9 http://mashabl~       729             11             1058           0.410
    ## 10 http://mashabl~       729              8              211           0.608
    ## # ... with 1,462 more rows, and 57 more variables: n_non_stop_words <dbl>,
    ## #   n_non_stop_unique_tokens <dbl>, num_hrefs <dbl>, num_self_hrefs <dbl>,
    ## #   num_imgs <dbl>, num_videos <dbl>, average_token_length <dbl>,
    ## #   num_keywords <dbl>, data_channel_is_lifestyle <dbl>,
    ## #   data_channel_is_entertainment <dbl>, data_channel_is_bus <dbl>,
    ## #   data_channel_is_socmed <dbl>, data_channel_is_tech <dbl>,
    ## #   data_channel_is_world <dbl>, kw_min_min <dbl>, kw_max_min <dbl>, ...

``` r
testData
```

    ## # A tibble: 627 x 62
    ##    url             timedelta n_tokens_title n_tokens_content n_unique_tokens
    ##    <chr>               <dbl>          <dbl>            <dbl>           <dbl>
    ##  1 http://mashabl~       731              8              960           0.418
    ##  2 http://mashabl~       731             10              187           0.667
    ##  3 http://mashabl~       731             11              315           0.551
    ##  4 http://mashabl~       731             10             1190           0.409
    ##  5 http://mashabl~       731              6              374           0.641
    ##  6 http://mashabl~       729              7             1007           0.438
    ##  7 http://mashabl~       729              9              455           0.496
    ##  8 http://mashabl~       729             10              258           0.589
    ##  9 http://mashabl~       729              8             1020           0.413
    ## 10 http://mashabl~       729              8              123           0.717
    ## # ... with 617 more rows, and 57 more variables: n_non_stop_words <dbl>,
    ## #   n_non_stop_unique_tokens <dbl>, num_hrefs <dbl>, num_self_hrefs <dbl>,
    ## #   num_imgs <dbl>, num_videos <dbl>, average_token_length <dbl>,
    ## #   num_keywords <dbl>, data_channel_is_lifestyle <dbl>,
    ## #   data_channel_is_entertainment <dbl>, data_channel_is_bus <dbl>,
    ## #   data_channel_is_socmed <dbl>, data_channel_is_tech <dbl>,
    ## #   data_channel_is_world <dbl>, kw_min_min <dbl>, kw_max_min <dbl>, ...

=======

## Exploratory Data Analysis (EDA)

### Create New Variables for EDA

``` r
#Create New variable using weekday_is_() variables
trainDataNew <- trainData %>% mutate(day_of_the_week =   if_else(weekday_is_monday == 1, "Monday",
       if_else(weekday_is_tuesday == 1, "Tuesday",
               if_else(weekday_is_wednesday == 1, "Wednesday",
                      if_else(weekday_is_thursday == 1, "Thursday",
                              if_else(weekday_is_friday == 1, "Friday",
                                      if_else(weekday_is_saturday == 1, "Saturday", "Sunday"
                                              ))))))) 
trainDataNew
```

    ## # A tibble: 1,472 x 63
    ##    url             timedelta n_tokens_title n_tokens_content n_unique_tokens
    ##    <chr>               <dbl>          <dbl>            <dbl>           <dbl>
    ##  1 http://mashabl~       731             11              103           0.689
    ##  2 http://mashabl~       731             10              243           0.619
    ##  3 http://mashabl~       731              8              204           0.586
    ##  4 http://mashabl~       730             12              499           0.513
    ##  5 http://mashabl~       729             11              223           0.662
    ##  6 http://mashabl~       729             11             1099           0.412
    ##  7 http://mashabl~       729             14              318           0.633
    ##  8 http://mashabl~       729              7              144           0.723
    ##  9 http://mashabl~       729             11             1058           0.410
    ## 10 http://mashabl~       729              8              211           0.608
    ## # ... with 1,462 more rows, and 58 more variables: n_non_stop_words <dbl>,
    ## #   n_non_stop_unique_tokens <dbl>, num_hrefs <dbl>, num_self_hrefs <dbl>,
    ## #   num_imgs <dbl>, num_videos <dbl>, average_token_length <dbl>,
    ## #   num_keywords <dbl>, data_channel_is_lifestyle <dbl>,
    ## #   data_channel_is_entertainment <dbl>, data_channel_is_bus <dbl>,
    ## #   data_channel_is_socmed <dbl>, data_channel_is_tech <dbl>,
    ## #   data_channel_is_world <dbl>, kw_min_min <dbl>, kw_max_min <dbl>, ...

``` r
sharesSumm<-trainData %>% 
  summarize("Min"=min(shares),
            "1st Quartile"=quantile(shares,0.25),
            "Median"=quantile(shares,0.5),
            "3rd Quartile"=quantile(shares,0.75),
            "Max"=max(shares)
            )
knitr ::kable(sharesSumm)
```

| Min | 1st Quartile | Median | 3rd Quartile |    Max |
|----:|-------------:|-------:|-------------:|-------:|
|  78 |         1100 |   1700 |         3225 | 208300 |

``` r
library(ggplot2)
trainData<- trainData %>% 
  mutate(sharecategory = ifelse(shares <1400, "few",
                      ifelse(shares %in% 1400:3800, "some",
                             "many")))
testData <- testData %>% mutate(sharecategory = ifelse(shares <1400, "few",ifelse(shares %in% 1400:3800, "some",
                             "many")))
knitr::kable(table(trainData$sharecategory), caption = paste0("contingency table for sharecategory"))
```

| Var1 | Freq |
|:-----|-----:|
| few  |  573 |
| many |  307 |
| some |  592 |

contingency table for sharecategory

``` r
g<-ggplot(data=trainData,aes(x=num_imgs,fill=sharecategory))
g+geom_bar(position="dodge")+
  labs(x="Number of Images", title = "Number of images based on category of shares")+
  theme(legend.title=element_blank(),
        axis.text.x=element_text(angle=45))+
  facet_wrap(~sharecategory)+
  theme(legend.position = "None")
```

![](LIFEST~2/figure-gfm/unnamed-chunk-4-1.png)<!-- -->

``` r
knitr::kable(table(trainData$sharecategory,trainData$num_imgs),caption = paste("contingency table for number of images based on share category"))
```

|      |   0 |   1 |   2 |   3 |   4 |   5 |   6 |   7 |   8 |   9 |  10 |  11 |  12 |  13 |  14 |  15 |  16 |  17 |  18 |  19 |  20 |  21 |  22 |  23 |  24 |  25 |  26 |  27 |  28 |  29 |  30 |  31 |  33 |  35 |  36 |  37 |  44 |  45 |  47 |  51 |  55 | 101 | 108 | 111 |
|:-----|----:|----:|----:|----:|----:|----:|----:|----:|----:|----:|----:|----:|----:|----:|----:|----:|----:|----:|----:|----:|----:|----:|----:|----:|----:|----:|----:|----:|----:|----:|----:|----:|----:|----:|----:|----:|----:|----:|----:|----:|----:|----:|----:|----:|
| few  |  84 | 276 |  37 |  19 |  16 |   9 |  15 |   6 |  13 |  11 |  15 |  36 |   6 |   4 |   4 |   3 |   3 |   4 |   2 |   2 |   4 |   1 |   1 |   0 |   0 |   1 |   0 |   0 |   0 |   0 |   0 |   0 |   0 |   0 |   0 |   1 |   0 |   0 |   0 |   0 |   0 |   0 |   0 |   0 |
| many |  36 | 123 |  16 |   5 |   8 |   4 |   6 |   3 |   4 |   4 |  10 |  18 |   9 |   2 |   3 |   9 |   8 |   2 |   1 |   3 |   3 |   5 |   5 |   0 |   2 |   4 |   3 |   3 |   1 |   1 |   1 |   1 |   0 |   0 |   0 |   1 |   0 |   0 |   0 |   0 |   1 |   1 |   0 |   1 |
| some |  80 | 252 |  37 |  10 |   7 |  10 |  19 |  11 |  17 |  17 |  30 |  32 |  12 |   9 |   3 |   6 |   9 |   1 |   1 |   1 |   8 |   3 |   1 |   2 |   1 |   4 |   0 |   0 |   0 |   0 |   0 |   0 |   1 |   1 |   1 |   0 |   1 |   1 |   1 |   1 |   0 |   0 |   1 |   1 |

contingency table for number of images based on share category

``` r
g<-ggplot(data=trainData,aes(x=num_videos, fill=sharecategory))
g+geom_bar(position="dodge")+
  labs(x="Number of Videos", title = "Number of videos based on category of shares")+
  theme(legend.title=element_blank(),
        axis.text.x=element_text(angle=45))+
  facet_wrap(~sharecategory)+
  theme(legend.position = "None")
```

![](LIFEST~2/figure-gfm/unnamed-chunk-4-2.png)<!-- -->

We can inspect the trend of number of images and videos and how it
affects number of shares. If the tallest and most concentrated chunk of
bars is in a different spot for each share category, then we can
conclude that the number of images videos (depending on the graph you
are looking at) is related to the number of shares. If each of the three
graphs looks the same, then we would conclude that images and videos do
not necessarily impact the number of shares.

I hypothesize that the shorter the average word length, the more popular
a media item will be. So we will analyze word length next. First, let’s
get the mean and standard deviation of word length in all of the media
items. Next, we can look at how word length differs based on share
category.

``` r
wordSumm<-trainData %>% 
  summarize("Mean"=mean(average_token_length),
            "Standard Deviation"=sd(average_token_length))
knitr ::kable(wordSumm, caption = "Mean and Standard deviation of average word length")
```

|     Mean | Standard Deviation |
|---------:|-------------------:|
| 4.588392 |          0.5650087 |

Mean and Standard deviation of average word length

``` r
wordSumm2<-trainData %>% group_by(sharecategory) %>%
  summarize("Mean"=mean(average_token_length),
            "Standard Deviation"=sd(average_token_length))
knitr ::kable(wordSumm2, caption = "Mean and Standard deviation of average word length by share category")
```

| sharecategory |     Mean | Standard Deviation |
|:--------------|---------:|-------------------:|
| few           | 4.619062 |          0.3326114 |
| many          | 4.480556 |          0.8653504 |
| some          | 4.614627 |          0.5405005 |

Mean and Standard deviation of average word length by share category

Finally, we’ll explore the title polarity vs. the share category.

``` r
g<-ggplot(data=trainData,aes(title_sentiment_polarity,color=title_sentiment_polarity))
g+geom_histogram(aes(fill=title_sentiment_polarity),position="dodge")+labs(x="Title Polarity", title = "Histogram of Title Polarity based on sharecategory")+theme(legend.title=element_blank(), axis.text.x=element_text(angle=45))+scale_y_continuous(limits= c(0,1000))+facet_wrap(~sharecategory)
```

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

![](LIFEST~2/figure-gfm/unnamed-chunk-6-1.png)<!-- -->

# Model fitting

## Linear Models

The main idea of linear regression models is that we look at a dependent
variable (our response variable) as a function of a set of independent
variables of our choosing (our predictor variables) which ends up being
linear. The two types are OLS which stands for ordinary least squares
along with GLM which is abbreviated as generalized linear model. OLS is
used when we are looking at errors that follow a normal distribution.
GLM is used when we are looking at errors that do not follow a normal
distribution.

### Model 1: (OLS)

The first model we will be fitting is a linear model (OLS) with shares
as the response and has all interaction model options from our
predictors (number of videos, average word length, and title polarity).

``` r
train1<-train(shares~num_imgs
                  *num_videos
                  *average_token_length
                  *title_sentiment_polarity,
              data=trainData,
              method="lm",
              preProcess=c("center","scale"),
              trControl=trainControl(method="cv",number=10))
```

### Model 2: Poisson Regression model (GLM)

The second model we will be creating is a poisson regression model which
is a generalized linear model. In this model, poisson is applied to all
the previous variables for all the interaction terms. We can use poisson
here since these come from all news articles coming from a 2 year period
which is a fixed amount of time.

Our model for the tech channel is (lambda(shares)\~num\_imgs
*num\_videos *average\_token\_length \*title\_sentiment\_polarity)

We will call this model our **Poisson Model**

``` r
train2 <- train(shares~num_imgs
                  *num_videos
                  *average_token_length
                  *title_sentiment_polarity,
              data=trainData,
              method="glm",
              family = "poisson",
              preProcess=c("center","scale"),
              trControl=trainControl(method="cv",number=10))
```

# Ensemble Methods

Tree-based ensemble methods are very important in that they allow us to
look at multiple areas of a non-linear model at once and map them out to
be able for us to easily interpret our predictive models. The two ones
that we will be using here and the ones that are the most popular are
boosted trees and random forests. We will be doing a grid search in our
repeated k-fold cross validations to tune our models for the best
hyper-parameters

## Boosted Tree Model

Boosting is a process where models are trained sequentially. The focus
is put on situations where the model fails to predict correctly a
statistically significant amount of the time. These models are given
more weight so that they are more likely to appear in any given sample.
Therefore, the tree will focus on getting these correct and therefore
improve the overall prediction. As this process continues, the model
gets stronger and stronger.Boosting is particularly robust against
overfitting.

Here I will do boosting with 5 fold repeated cross validation (3 times).
I will then print the confusion matrix on the test set.

``` r
ctrl <- trainControl(method="repeatedcv",number=5, repeats = 3)
boostFit <- train(shares~num_imgs
                  +num_videos
                  +average_token_length
                  +title_sentiment_polarity
                  ,data = trainData, 
                method = "gbm", trControl = ctrl, metric = "RMSE",
                preProcess = c("center","scale"), verbose = FALSE, 
                tuneGrid =  expand.grid(n.trees = 25, 
                              shrinkage = 0.1,
                              interaction.depth = c(1:4),
                              n.minobsinnode = 10))
```

## Random Forest

The idea of Random Forests is that they generate `B` bootstrapped
samples and fit trees to each of the samples. They use a random subset
of predictors for each bootstrap sample/tree fit. Then the trees grow
independently of one another while also being averaged. Averaging the
trees along with letting the trees grow independently can lower our
variance we are using for prediction.

``` r
library(doParallel)
cl <- makePSOCKcluster(5)
registerDoParallel(cl)
fitrf <- train(shares~num_imgs*num_videos*average_token_length*title_sentiment_polarity,method = "rf",data = trainData,
             trControl = ctrl, 
             metric = "RMSE",
             tuneGrid = data.frame(mtry = 1:3))
stopCluster(cl)
```

# Model Comparison

With the repeated k-folds CV completed and our models fit, we can
evaluate their performance in CV and on the withheld testing set.

Let’s evaluate their repeated k-folds CV performance first

``` r
modelList <- list(train1, train2, boostFit)
modelNames <- c("OLS", "Poisson Regression", "Boosted Trees")
```

``` r
cvRMSE <- unlist(
  sapply(
    sapply(
      sapply(modelList, FUN="[", "results"), 
      FUN=filter, RMSE==min(RMSE)), 
    FUN="[", "RMSE"
    )
)

cvMAE <- unlist(
  sapply(
    sapply(
      sapply(modelList, FUN="[", "results"), 
      FUN=filter, MAE==min(MAE)), 
    FUN="[", "MAE"
    )
  )

cvRsquared <- unlist(
  sapply(
    sapply(
      sapply(modelList, FUN="[", "results"), 
      FUN=filter, Rsquared==max(Rsquared)), 
    FUN="[", "Rsquared"
    )
  )

# Create a data.frame of model performances.
cvPerformance <- data.frame(
  Model=modelNames,
  RMSE=cvRMSE,
  Rsq=cvRsquared,
  MAE=cvMAE
)

# Extract the best model's name and RMSE.
bestModelCV <- cvPerformance %>%
  mutate(Model = modelNames) %>%
  filter(RMSE == min(RMSE)) %>%
  select(Model, RMSE)
# Save the model name and RMSE to 2 decimal places as vairables.
bestModelNameCV <- bestModelCV$Model
bestRMSECV <- round(bestModelCV$RMSE, 2)

# Display the table in a neat format.
knitr::kable(
  cvPerformance,
  digits=2,
  caption="Table 3: Repeated k-folds CV Performance Summary",
  col.names=c("", "RMSE", "Rsquared", "MAE")
)
```

|                    |     RMSE | Rsquared |     MAE |
|:-------------------|---------:|---------:|--------:|
| OLS                |  6872.63 |     0.02 | 3208.25 |
| Poisson Regression | 12005.06 |     0.02 | 3633.75 |
| Boosted Trees      |  7164.12 |     0.02 | 3154.19 |

Table 3: Repeated k-folds CV Performance Summary

The best performing model in repeated k-folds CV is the OLS with an RMSE
of 6872.63. Usually, we would pick the best performing model here to
test on the testing data, but we will compare them all this time.

Now let’s look at their test set performance.

``` r
evaluatePeformance <- function(model, dataEval, target){
  ###
  # This function takes in a fit model, testing data (tibble), and a target
  # variable (string) and returns the performance.
  ###
  preds <- predict(model, newdata=dataEval)
  return(postResample(preds, pull(dataEval, target)))
}
# Get the test set performances.
testPerformances <- sapply(
  modelList, FUN=evaluatePeformance, dataEval=testData, target="shares"
  )
# Rename the columns with the model names.
colnames(testPerformances) <- modelNames
# Convert the table to data.frame.
testPerformances <- as.data.frame(t(testPerformances))
# Extract the best model's name and RMSE.
bestModel <- testPerformances %>%
  mutate(Model = modelNames) %>%
  filter(RMSE == min(RMSE)) %>%
  select(Model, RMSE)
# Save the model name and RMSE to 2 decimal places as variables.
bestModelName <- bestModel$Model
bestRMSE <- round(bestModel$RMSE, 2)
# Display the table in a neat format.
knitr::kable(
  testPerformances,
  digits=2,
  caption="Table 4: Test Set Performance Summary",)
```

|                    |     RMSE | Rsquared |     MAE |
|:-------------------|---------:|---------:|--------:|
| OLS                | 11046.26 |        0 | 3482.93 |
| Poisson Regression | 55980.74 |        0 | 5611.65 |
| Boosted Trees      | 10933.39 |        0 | 3496.46 |

Table 4: Test Set Performance Summary

The best performing model on the testing set is the Boosted Trees with
an RMSE of 1.093339^{4}.
