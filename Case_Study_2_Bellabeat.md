Bellabeat
================
Valesca Bravo
2022-12-21

``` r
#install.packages("anytime")
#install.packages("sqldf")
#install.packages("data.table")
#install.packages("magrittr")

library(ggvenn)         
```

    ## Loading required package: dplyr

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

    ## Loading required package: grid

    ## Loading required package: ggplot2

``` r
library(magrittr)
library(tidyverse)
```

    ## ── Attaching packages
    ## ───────────────────────────────────────
    ## tidyverse 1.3.2 ──

    ## ✔ tibble  3.1.8     ✔ purrr   0.3.5
    ## ✔ tidyr   1.2.1     ✔ stringr 1.4.1
    ## ✔ readr   2.1.3     ✔ forcats 0.5.2
    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ tidyr::extract()   masks magrittr::extract()
    ## ✖ dplyr::filter()    masks stats::filter()
    ## ✖ dplyr::lag()       masks stats::lag()
    ## ✖ purrr::set_names() masks magrittr::set_names()

``` r
library(sqldf)
```

    ## Loading required package: gsubfn
    ## Loading required package: proto
    ## Loading required package: RSQLite

``` r
library(ggplot2)# plot 
library(dplyr)
library(lubridate)
```

    ## Loading required package: timechange
    ## 
    ## Attaching package: 'lubridate'
    ## 
    ## The following objects are masked from 'package:base':
    ## 
    ##     date, intersect, setdiff, union

``` r
library(anytime)
library(corrplot)
```

    ## corrplot 0.92 loaded

``` r
library(ggplot2) 
library(data.table)
```

    ## 
    ## Attaching package: 'data.table'
    ## 
    ## The following objects are masked from 'package:lubridate':
    ## 
    ##     hour, isoweek, mday, minute, month, quarter, second, wday, week,
    ##     yday, year
    ## 
    ## The following object is masked from 'package:purrr':
    ## 
    ##     transpose
    ## 
    ## The following objects are masked from 'package:dplyr':
    ## 
    ##     between, first, last

``` r
library(grid)
library(futile.logger)
```

## Introduction

The present study will investigate Bellabeat, a high-tech manufacturer
of health-focused products for women. I will work with the 6 steps of
the data analysis process: ask, prepare, process, analyze, share, and
act, in order to get inside the dataset that have been proposed by
Google in its Data Analyst certification.

I am trying to resolve, with the daily data, how people are already
using their smart devices,in order to gain insights.

``` r
#general
weight <- read_csv("~/Google/Capstone/datasets/weightLogInfo_merged.csv")
```

    ## Rows: 67 Columns: 8
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr (1): Date
    ## dbl (6): Id, WeightKg, WeightPounds, Fat, BMI, LogId
    ## lgl (1): IsManualReport
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
sleepDay <- read_csv("~/Google/Capstone/datasets/sleepDay_merged.csv")
```

    ## Rows: 413 Columns: 5
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr (1): SleepDay
    ## dbl (4): Id, TotalSleepRecords, TotalMinutesAsleep, TotalTimeInBed
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
heartrate_seconds <- read_csv("~/Google/Capstone/datasets/heartrate_seconds_merged.csv")
```

    ## Rows: 2483658 Columns: 3
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr (1): Time
    ## dbl (2): Id, Value
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
#minutes
#hourlyCalories <- read_csv("~/Google/Capstone/datasets/hourlyCalories_merged.csv")
#minuteIntensities <- read_csv("~/Google/Capstone/datasets/minuteIntensitiesNarrow_merged.csv")
#minuteMETs <- read_csv("~/Google/Capstone/datasets/minuteMETsNarrow_merged.csv")
#minuteSteps <- read_csv("~/Google/Capstone/datasets/minuteStepsNarrow_merged.csv")

#Hourly
#hourlyCalories <- read_csv("~/Google/Capstone/datasets/hourlyCalories_merged.csv")
#hourlyIntensities <- read_csv("~/Google/Capstone/datasets/hourlyIntensities_merged.csv")
#hourlySteps <- read_csv("~/Google/Capstone/datasets/hourlySteps_merged.csv")

#daily
dailySteps<- read_csv("~/Google/Capstone/datasets/dailySteps_merged.csv")
```

    ## Rows: 940 Columns: 3
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr (1): ActivityDay
    ## dbl (2): Id, StepTotal
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
dailyIntensities <- read_csv("~/Google/Capstone/datasets/dailyIntensities_merged.csv")
```

    ## Rows: 940 Columns: 10
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr (1): ActivityDay
    ## dbl (9): Id, SedentaryMinutes, LightlyActiveMinutes, FairlyActiveMinutes, Ve...
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
dailyCalories <- read_csv("~/Google/Capstone/datasets/dailyCalories_merged.csv")
```

    ## Rows: 940 Columns: 3
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr (1): ActivityDay
    ## dbl (2): Id, Calories
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
dailyActivity <- read_csv("~/Google/Capstone/datasets/dailyActivity_merged.csv")
```

    ## Rows: 940 Columns: 15
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr  (1): ActivityDate
    ## dbl (14): Id, TotalSteps, TotalDistance, TrackerDistance, LoggedActivitiesDi...
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
print("---------------------------------------------weight---------------------------------")
```

    ## [1] "---------------------------------------------weight---------------------------------"

``` r
glimpse(weight)
```

    ## Rows: 67
    ## Columns: 8
    ## $ Id             <dbl> 1503960366, 1503960366, 1927972279, 2873212765, 2873212…
    ## $ Date           <chr> "5/2/2016 11:59:59 PM", "5/3/2016 11:59:59 PM", "4/13/2…
    ## $ WeightKg       <dbl> 52.6, 52.6, 133.5, 56.7, 57.3, 72.4, 72.3, 69.7, 70.3, …
    ## $ WeightPounds   <dbl> 115.9631, 115.9631, 294.3171, 125.0021, 126.3249, 159.6…
    ## $ Fat            <dbl> 22, NA, NA, NA, NA, 25, NA, NA, NA, NA, NA, NA, NA, NA,…
    ## $ BMI            <dbl> 22.65, 22.65, 47.54, 21.45, 21.69, 27.45, 27.38, 27.25,…
    ## $ IsManualReport <lgl> TRUE, TRUE, FALSE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, …
    ## $ LogId          <dbl> 1.462234e+12, 1.462320e+12, 1.460510e+12, 1.461283e+12,…

``` r
print("---------------------------------------------sleepDay------------------------------")
```

    ## [1] "---------------------------------------------sleepDay------------------------------"

``` r
glimpse(sleepDay)
```

    ## Rows: 413
    ## Columns: 5
    ## $ Id                 <dbl> 1503960366, 1503960366, 1503960366, 1503960366, 150…
    ## $ SleepDay           <chr> "4/12/2016 12:00:00 AM", "4/13/2016 12:00:00 AM", "…
    ## $ TotalSleepRecords  <dbl> 1, 2, 1, 2, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, …
    ## $ TotalMinutesAsleep <dbl> 327, 384, 412, 340, 700, 304, 360, 325, 361, 430, 2…
    ## $ TotalTimeInBed     <dbl> 346, 407, 442, 367, 712, 320, 377, 364, 384, 449, 3…

``` r
print("---------------------------------------------heartrate_seconds----------------------")
```

    ## [1] "---------------------------------------------heartrate_seconds----------------------"

``` r
glimpse(heartrate_seconds)
```

    ## Rows: 2,483,658
    ## Columns: 3
    ## $ Id    <dbl> 2022484408, 2022484408, 2022484408, 2022484408, 2022484408, 2022…
    ## $ Time  <chr> "4/12/2016 7:21:00 AM", "4/12/2016 7:21:05 AM", "4/12/2016 7:21:…
    ## $ Value <dbl> 97, 102, 105, 103, 101, 95, 91, 93, 94, 93, 92, 89, 83, 61, 60, …

``` r
print("---------------------------------------------dailySteps----------------------------")
```

    ## [1] "---------------------------------------------dailySteps----------------------------"

``` r
glimpse(dailySteps)
```

    ## Rows: 940
    ## Columns: 3
    ## $ Id          <dbl> 1503960366, 1503960366, 1503960366, 1503960366, 1503960366…
    ## $ ActivityDay <chr> "4/12/2016", "4/13/2016", "4/14/2016", "4/15/2016", "4/16/…
    ## $ StepTotal   <dbl> 13162, 10735, 10460, 9762, 12669, 9705, 13019, 15506, 1054…

``` r
print("---------------------------------------------dailyCalories-------------------------")
```

    ## [1] "---------------------------------------------dailyCalories-------------------------"

``` r
glimpse(dailyCalories)
```

    ## Rows: 940
    ## Columns: 3
    ## $ Id          <dbl> 1503960366, 1503960366, 1503960366, 1503960366, 1503960366…
    ## $ ActivityDay <chr> "4/12/2016", "4/13/2016", "4/14/2016", "4/15/2016", "4/16/…
    ## $ Calories    <dbl> 1985, 1797, 1776, 1745, 1863, 1728, 1921, 2035, 1786, 1775…

``` r
print("---------------------------------------------dailyIntensities----------------------")
```

    ## [1] "---------------------------------------------dailyIntensities----------------------"

``` r
glimpse(dailyIntensities)
```

    ## Rows: 940
    ## Columns: 10
    ## $ Id                       <dbl> 1503960366, 1503960366, 1503960366, 150396036…
    ## $ ActivityDay              <chr> "4/12/2016", "4/13/2016", "4/14/2016", "4/15/…
    ## $ SedentaryMinutes         <dbl> 728, 776, 1218, 726, 773, 539, 1149, 775, 818…
    ## $ LightlyActiveMinutes     <dbl> 328, 217, 181, 209, 221, 164, 233, 264, 205, …
    ## $ FairlyActiveMinutes      <dbl> 13, 19, 11, 34, 10, 20, 16, 31, 12, 8, 27, 21…
    ## $ VeryActiveMinutes        <dbl> 25, 21, 30, 29, 36, 38, 42, 50, 28, 19, 66, 4…
    ## $ SedentaryActiveDistance  <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, …
    ## $ LightActiveDistance      <dbl> 6.06, 4.71, 3.91, 2.83, 5.04, 2.51, 4.71, 5.0…
    ## $ ModeratelyActiveDistance <dbl> 0.55, 0.69, 0.40, 1.26, 0.41, 0.78, 0.64, 1.3…
    ## $ VeryActiveDistance       <dbl> 1.88, 1.57, 2.44, 2.14, 2.71, 3.19, 3.25, 3.5…

``` r
print("---------------------------------------------dailyActivity ------------------------")
```

    ## [1] "---------------------------------------------dailyActivity ------------------------"

``` r
glimpse(dailyActivity)
```

    ## Rows: 940
    ## Columns: 15
    ## $ Id                       <dbl> 1503960366, 1503960366, 1503960366, 150396036…
    ## $ ActivityDate             <chr> "4/12/2016", "4/13/2016", "4/14/2016", "4/15/…
    ## $ TotalSteps               <dbl> 13162, 10735, 10460, 9762, 12669, 9705, 13019…
    ## $ TotalDistance            <dbl> 8.50, 6.97, 6.74, 6.28, 8.16, 6.48, 8.59, 9.8…
    ## $ TrackerDistance          <dbl> 8.50, 6.97, 6.74, 6.28, 8.16, 6.48, 8.59, 9.8…
    ## $ LoggedActivitiesDistance <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, …
    ## $ VeryActiveDistance       <dbl> 1.88, 1.57, 2.44, 2.14, 2.71, 3.19, 3.25, 3.5…
    ## $ ModeratelyActiveDistance <dbl> 0.55, 0.69, 0.40, 1.26, 0.41, 0.78, 0.64, 1.3…
    ## $ LightActiveDistance      <dbl> 6.06, 4.71, 3.91, 2.83, 5.04, 2.51, 4.71, 5.0…
    ## $ SedentaryActiveDistance  <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, …
    ## $ VeryActiveMinutes        <dbl> 25, 21, 30, 29, 36, 38, 42, 50, 28, 19, 66, 4…
    ## $ FairlyActiveMinutes      <dbl> 13, 19, 11, 34, 10, 20, 16, 31, 12, 8, 27, 21…
    ## $ LightlyActiveMinutes     <dbl> 328, 217, 181, 209, 221, 164, 233, 264, 205, …
    ## $ SedentaryMinutes         <dbl> 728, 776, 1218, 726, 773, 539, 1149, 775, 818…
    ## $ Calories                 <dbl> 1985, 1797, 1776, 1745, 1863, 1728, 1921, 203…

We can see that all the dataframes that are called daily contains 940
row and have similarities columns in between. Firstly, I will fix the
data name for Date and then I will verify if the dataframe dailyActivity
contains the same information as dailySteps, dailyCalories and
dailyIntensities.

In order to do this I will make a new dataframe with the columns to be
compared from dailyActivity. And, helping myself with SQL, I will check
if there is a difference between this new temporary dataframe and the
other three dataframes. This should give me 0 as a result of the
information is the same.

``` r
Activity <- dailyActivity# copy to used later.
heartrate <- heartrate_seconds# copy to used later.
sleep <-  sleepDay # copy to used later.


# fix columns Date dailyActivity
colnames(dailyActivity)[2]  <- "Date"

colnames(dailySteps)[2]  <- "Date"

colnames(dailyCalories)[2]  <- "Date" 

colnames(dailyIntensities)[2]  <- "Date" 


# fix columns Date dailysleepDay
colnames(sleepDay)[2]  <- "Date"
```

``` r
# fix columns TotalSteps
colnames(dailySteps)[3]  <- "TotalSteps" 


#temporary dataframe with selected columns to check
Activity_step <- dailyActivity%>%
   select(Id, Date, TotalSteps)

#comparison dailyActivity vs dailySteps
steps <- sqldf("SELECT * FROM Activity_step
              EXCEPT
              SELECT * FROM dailySteps")

head(steps)
```

    ## [1] Id         Date       TotalSteps
    ## <0 rows> (or 0-length row.names)

``` r
colnames(dailyCalories)[2]  <- "Date" 

#temporary dataframe with selected columns to check
 Activity_Cal <- dailyActivity%>%
  select(Id, Date, Calories)

 #comparison dailyActivity vs dailyCalories
Cal <- sqldf("SELECT * FROM Activity_Cal
              EXCEPT
              SELECT * FROM dailyCalories")

head(Cal)
```

    ## [1] Id       Date     Calories
    ## <0 rows> (or 0-length row.names)

``` r
#temporary dataframe with selected columns to check

Activity_Intensities <- dailyActivity%>%
   select(Id, Date, SedentaryMinutes,LightlyActiveMinutes, FairlyActiveMinutes,VeryActiveMinutes, SedentaryActiveDistance,LightActiveDistance,ModeratelyActiveDistance,VeryActiveDistance)

#comparison dailyActivity vs dailyIntensities
Intensities <- sqldf("SELECT * FROM Activity_Intensities
              EXCEPT
              SELECT * FROM dailyIntensities")

head(Intensities)
```

    ##  [1] Id                       Date                     SedentaryMinutes        
    ##  [4] LightlyActiveMinutes     FairlyActiveMinutes      VeryActiveMinutes       
    ##  [7] SedentaryActiveDistance  LightActiveDistance      ModeratelyActiveDistance
    ## [10] VeryActiveDistance      
    ## <0 rows> (or 0-length row.names)

Now on, we know that dailyActivity is the dataset summary that contains
dailyIntensities, dailyCalories and dailySteps. The next problem is
verified how many users are using which product and check if there are
users using more than one product.

``` r
#checking ID unique values of each dataset

#dailyActivity

print("dailyActivity")
```

    ## [1] "dailyActivity"

``` r
a1 <- as.data.frame(table(dailyActivity$Id))
colnames(a1)[1]  <- "ID" 
dim(a1)
```

    ## [1] 33  2

``` r
#weight
print("weight")
```

    ## [1] "weight"

``` r
a2 <- as.data.frame(table(weight$Id))
colnames(a2)[1]  <- "ID" 
dim(a2)
```

    ## [1] 8 2

``` r
#sleepDay
print("sleepDay")
```

    ## [1] "sleepDay"

``` r
a3 <- as.data.frame(table(sleepDay$Id))
colnames(a3)[1]  <- "ID" 
dim(a3)
```

    ## [1] 24  2

``` r
#heartrate_seconds
print("heartrate")
```

    ## [1] "heartrate"

``` r
a4 <- as.data.frame(table(heartrate_seconds$Id))
colnames(a4)[1]  <- "ID" 
dim(a4)
```

    ## [1] 14  2

``` r
#dailyIntensities
print("dailyIntensities")
```

    ## [1] "dailyIntensities"

``` r
a5 <- as.data.frame(table(dailyIntensities$Id))
colnames(a5)[1]  <- "ID" 

dim(a5)
```

    ## [1] 33  2

``` r
#dailyCalories
print("dailyCalories")
```

    ## [1] "dailyCalories"

``` r
a6 <- as.data.frame(table(dailyCalories$Id))
colnames(a6)[1]  <- "ID" 

dim(a6)
```

    ## [1] 33  2

``` r
#dailySteps
print("dailySteps")
```

    ## [1] "dailySteps"

``` r
a7 <- as.data.frame(table(dailySteps$Id))
colnames(a7)[1]  <- "ID" 
dim(a7)
```

    ## [1] 33  2

``` r
users1 <-list('Intensities'=a5$ID,'Calories'=a6$ID,'Steps'=a7$ID)
# display all the sets
ggvenn(users1,fill_alpha= 0.1,
       fill_color = c("yellow","green", "blue"),
       text_size= 2,
       set_name_size=3)
```

![](Case_Study_2_Bellabeat_files/figure-gfm/User%20venn%20diagram%20plot-1.png)<!-- -->

``` r
users2 <-list('Activity'=a1$ID,'weight'=a2$ID,'sleepDay'=a3$ID,'heartrate'=a4$ID)
# display all the sets
ggvenn(users2,fill_alpha= 0.1,
       fill_color = c("yellow","green", "blue", "red"),
       text_size= 2,
       set_name_size=3)
```

![](Case_Study_2_Bellabeat_files/figure-gfm/User%20venn%20diagram%20plot-2.png)<!-- -->
The dataset summary of daily Intensities, daily Calories and daily Steps
is daily Activity, it have 33 users, how we can see in the first venn
Diagram, the users are equaly using this 3 products weight 8, Sleepday
24 and finally heartrate 24. Following the business task I crossed the
ID information in a venn diagram to check if there are clients using the
same products. After checking this venn diagram we can realized that the
universe of users of Bellabeat company are 33 people.

Let see aour findings:

- We can see that the totality of user in SleepDay(24), are all of them
  in dailyActivity. First insight: Users that are monitoring their
  activities, 50%(12/24) of them also are doing it with their heart rate
  and 25%(6/24) are checking at their weight.

- User that are monitoring their heart rate(14), also all of them are in
  the main dataset, daily activity. Second insight: Users that are
  monitoring their heart rate, 86%(12/14) of them also are doing it with
  their sleep and 28%(4/14) are checking at their weight.

- User that are monitoring their weight(8), also all of them are in the
  main dataset, daily activity. Third insight: Users that are monitoring
  their weight, 75%(6/8) of them also are doing it with their sleep and
  50%(4/4) are checking at their heart rate.

Anyways there are 18%(6/33) user that are in the dailyActivity, but they
are no monitoring heart rate,weight or sleep.

### EDA

``` r
# are there NaNs?
colSums(Activity=="")
```

    ##                       Id             ActivityDate               TotalSteps 
    ##                        0                        0                        0 
    ##            TotalDistance          TrackerDistance LoggedActivitiesDistance 
    ##                        0                        0                        0 
    ##       VeryActiveDistance ModeratelyActiveDistance      LightActiveDistance 
    ##                        0                        0                        0 
    ##  SedentaryActiveDistance        VeryActiveMinutes      FairlyActiveMinutes 
    ##                        0                        0                        0 
    ##     LightlyActiveMinutes         SedentaryMinutes                 Calories 
    ##                        0                        0                        0

``` r
colSums(is.na(Activity))
```

    ##                       Id             ActivityDate               TotalSteps 
    ##                        0                        0                        0 
    ##            TotalDistance          TrackerDistance LoggedActivitiesDistance 
    ##                        0                        0                        0 
    ##       VeryActiveDistance ModeratelyActiveDistance      LightActiveDistance 
    ##                        0                        0                        0 
    ##  SedentaryActiveDistance        VeryActiveMinutes      FairlyActiveMinutes 
    ##                        0                        0                        0 
    ##     LightlyActiveMinutes         SedentaryMinutes                 Calories 
    ##                        0                        0                        0

``` r
#Checking the unique 
apply(Activity,2, function(x) length(unique(x)))
```

    ##                       Id             ActivityDate               TotalSteps 
    ##                       33                       31                      842 
    ##            TotalDistance          TrackerDistance LoggedActivitiesDistance 
    ##                      615                      613                       19 
    ##       VeryActiveDistance ModeratelyActiveDistance      LightActiveDistance 
    ##                      333                      211                      491 
    ##  SedentaryActiveDistance        VeryActiveMinutes      FairlyActiveMinutes 
    ##                        9                      122                       81 
    ##     LightlyActiveMinutes         SedentaryMinutes                 Calories 
    ##                      335                      549                      734

``` r
#apply(X, MARGIN, FUN)Margin1=row/margin2=Columns
```

``` r
#Calculating Correlation Activity

N1<- round(cor(Activity[, unlist(lapply(Activity, is.numeric))]),1)

testRes = cor.mtest(N1, conf.level = 0.95)
corrplot(N1, p.mat = testRes$p, method = 'square', type = 'lower', insig='blank', tl.cex = 0.5,
         addCoef.col ='black', number.cex = 0.5, order = 'AOE', diag=FALSE,col = COL2('PRGn'))
```

![](Case_Study_2_Bellabeat_files/figure-gfm/unnamed-chunk-3-1.png)<!-- -->
We can see that there is a perfect correlation between Total Distance,
Tracker Distance, and Total Steps. Another higher correlation with 0.9
of the coefficient is Fairly Active Minutes and Moderately Active
Distance and light Active Distance and Light Active minutes. Although if
we analyzed the variable of Very Active Distance has positive
relationships with Very Active Minutes, Total Distance, Tracker
Distance, and Total Steps. We can see Calories having relationships with
a coefficient of 0.6 with Very Active Minutes, and Total Steps 0,7 with
Very Active Minutes, Very Active Distance, and Also light Active
Distance. Finally, we can see the negative relationship between
Sedentary Minutes and some columns having a significative coefficient
number of -0,4 with Light Active Minutes and Light Active Distance.

``` r
#weekday

dailyActivity$date<- mdy(dailyActivity$Date)


dailyActivity$weekday <- weekdays.POSIXt(dailyActivity$date) # Add a weekday columns
#day
dailyActivity$day<- as.numeric(format(as.Date(dailyActivity$date),"%d"))
#month
dailyActivity$month<- as.numeric(format(as.Date(dailyActivity$date),"%m"))

#day_of_week
#dailyActivity$weekday<- weekdays(as.Date(dailyActivity$date))

#Format day_of_week  

head(dailyActivity)
```

    ## # A tibble: 6 × 19
    ##         Id Date  Total…¹ Total…² Track…³ Logge…⁴ VeryA…⁵ Moder…⁶ Light…⁷ Seden…⁸
    ##      <dbl> <chr>   <dbl>   <dbl>   <dbl>   <dbl>   <dbl>   <dbl>   <dbl>   <dbl>
    ## 1   1.50e9 4/12…   13162    8.5     8.5        0    1.88   0.550    6.06       0
    ## 2   1.50e9 4/13…   10735    6.97    6.97       0    1.57   0.690    4.71       0
    ## 3   1.50e9 4/14…   10460    6.74    6.74       0    2.44   0.400    3.91       0
    ## 4   1.50e9 4/15…    9762    6.28    6.28       0    2.14   1.26     2.83       0
    ## 5   1.50e9 4/16…   12669    8.16    8.16       0    2.71   0.410    5.04       0
    ## 6   1.50e9 4/17…    9705    6.48    6.48       0    3.19   0.780    2.51       0
    ## # … with 9 more variables: VeryActiveMinutes <dbl>, FairlyActiveMinutes <dbl>,
    ## #   LightlyActiveMinutes <dbl>, SedentaryMinutes <dbl>, Calories <dbl>,
    ## #   date <date>, weekday <chr>, day <dbl>, month <dbl>, and abbreviated
    ## #   variable names ¹​TotalSteps, ²​TotalDistance, ³​TrackerDistance,
    ## #   ⁴​LoggedActivitiesDistance, ⁵​VeryActiveDistance, ⁶​ModeratelyActiveDistance,
    ## #   ⁷​LightActiveDistance, ⁸​SedentaryActiveDistance

``` r
summary(dailyActivity)
```

    ##        Id                Date             TotalSteps    TotalDistance   
    ##  Min.   :1.504e+09   Length:940         Min.   :    0   Min.   : 0.000  
    ##  1st Qu.:2.320e+09   Class :character   1st Qu.: 3790   1st Qu.: 2.620  
    ##  Median :4.445e+09   Mode  :character   Median : 7406   Median : 5.245  
    ##  Mean   :4.855e+09                      Mean   : 7638   Mean   : 5.490  
    ##  3rd Qu.:6.962e+09                      3rd Qu.:10727   3rd Qu.: 7.713  
    ##  Max.   :8.878e+09                      Max.   :36019   Max.   :28.030  
    ##  TrackerDistance  LoggedActivitiesDistance VeryActiveDistance
    ##  Min.   : 0.000   Min.   :0.0000           Min.   : 0.000    
    ##  1st Qu.: 2.620   1st Qu.:0.0000           1st Qu.: 0.000    
    ##  Median : 5.245   Median :0.0000           Median : 0.210    
    ##  Mean   : 5.475   Mean   :0.1082           Mean   : 1.503    
    ##  3rd Qu.: 7.710   3rd Qu.:0.0000           3rd Qu.: 2.053    
    ##  Max.   :28.030   Max.   :4.9421           Max.   :21.920    
    ##  ModeratelyActiveDistance LightActiveDistance SedentaryActiveDistance
    ##  Min.   :0.0000           Min.   : 0.000      Min.   :0.000000       
    ##  1st Qu.:0.0000           1st Qu.: 1.945      1st Qu.:0.000000       
    ##  Median :0.2400           Median : 3.365      Median :0.000000       
    ##  Mean   :0.5675           Mean   : 3.341      Mean   :0.001606       
    ##  3rd Qu.:0.8000           3rd Qu.: 4.782      3rd Qu.:0.000000       
    ##  Max.   :6.4800           Max.   :10.710      Max.   :0.110000       
    ##  VeryActiveMinutes FairlyActiveMinutes LightlyActiveMinutes SedentaryMinutes
    ##  Min.   :  0.00    Min.   :  0.00      Min.   :  0.0        Min.   :   0.0  
    ##  1st Qu.:  0.00    1st Qu.:  0.00      1st Qu.:127.0        1st Qu.: 729.8  
    ##  Median :  4.00    Median :  6.00      Median :199.0        Median :1057.5  
    ##  Mean   : 21.16    Mean   : 13.56      Mean   :192.8        Mean   : 991.2  
    ##  3rd Qu.: 32.00    3rd Qu.: 19.00      3rd Qu.:264.0        3rd Qu.:1229.5  
    ##  Max.   :210.00    Max.   :143.00      Max.   :518.0        Max.   :1440.0  
    ##     Calories         date              weekday               day       
    ##  Min.   :   0   Min.   :2016-04-12   Length:940         Min.   : 1.00  
    ##  1st Qu.:1828   1st Qu.:2016-04-19   Class :character   1st Qu.: 9.00  
    ##  Median :2134   Median :2016-04-26   Mode  :character   Median :16.00  
    ##  Mean   :2304   Mean   :2016-04-26                      Mean   :15.79  
    ##  3rd Qu.:2793   3rd Qu.:2016-05-04                      3rd Qu.:23.00  
    ##  Max.   :4900   Max.   :2016-05-12                      Max.   :30.00  
    ##      month     
    ##  Min.   :4.00  
    ##  1st Qu.:4.00  
    ##  Median :4.00  
    ##  Mean   :4.35  
    ##  3rd Qu.:5.00  
    ##  Max.   :5.00

We can see that the data start on the 12th of April, 2016, and finish on
the 12th of May 2016. The minimum of Calories taken by the users is zero
and the maximum is 4.900.

- VeryActiveMinutes have a mean of 21.16 minutes and a maximum of 210
  minutes (3.5 hours). \_ VeryActiveDistance has a mean of 1.503
  kilometers and a maximum of 21.9k.

- FairlyActiveMinutes have a mean of 13.56 minutes and a maximum of 143
  minutes(2.38 hours).

- ModeratelyActiveDistance has a mean of 0.5675 kilometers and a maximum
  of 6.4k.

- LightlyActiveMinutes have a mean of 192 minutes(3.2 hours) and a
  maximum of 518 minutes(8.63 hours). -LightActiveDistance has a mean of
  3.341 kilometers and a maximum of 10.710k

-SedentaryMinutes have a mean of 991.2 minutes(16 hours) and a maximum
of 1440.0 minutes(24 hours)

-TotalSteps has a mean of 7638 steps and a maximum of 36019 steps
accumulated in a month. -TotalDistance has a mean of 5.245 Kilometers
and a maximum of 28 kilometers accumulated in a month.

``` r
ggplot(data = dailyActivity) + geom_bar(mapping = aes(x =weekday, fill=weekday)) +
  labs(title = "Day performance ",  caption = "Bellabeat Products")
```

![](Case_Study_2_Bellabeat_files/figure-gfm/Day%20performance-1.png)<!-- -->
We can observate that is Tuesday and then Wednesday when the user are
using the products the most and and is Monday which gets the less
interaction.

``` r
# perfect correlationship TotalSteps and TotalDistance
ggplot(data=dailyActivity, aes(x=TotalSteps, y=TotalDistance, color=weekday)) +geom_point() +stat_smooth(method=lm) +facet_wrap(~weekday) + labs(title = "TotalDistance-TotalSteps Weekdays Performance")
```

    ## `geom_smooth()` using formula = 'y ~ x'

![](Case_Study_2_Bellabeat_files/figure-gfm/higher%20coefficient%20in%20heatmap%20review-1.png)<!-- -->

``` r
#"FairlyMinutes-Moderately Distance Weekday Performance"
ggplot(data=dailyActivity, aes(x=FairlyActiveMinutes, y=ModeratelyActiveDistance, color=weekday)) +geom_point() +stat_smooth(method=lm) +facet_wrap(~weekday) + labs(title = "FairlyMinutes-Moderately Distance Weekdays Performance")
```

    ## `geom_smooth()` using formula = 'y ~ x'

![](Case_Study_2_Bellabeat_files/figure-gfm/higher%20coefficient%20in%20heatmap%20review-2.png)<!-- -->

``` r
#Very Active -Very Active  Distance Weekday Performance
ggplot(data=dailyActivity, aes(x=VeryActiveMinutes, y=VeryActiveDistance, color=weekday)) +geom_point() +stat_smooth(method=lm) +facet_wrap(~weekday) + labs(title = "Very Active-Very Active  Distance Weekdays Performance")
```

    ## `geom_smooth()` using formula = 'y ~ x'

![](Case_Study_2_Bellabeat_files/figure-gfm/higher%20coefficient%20in%20heatmap%20review-3.png)<!-- -->

``` r
ggplot(data=dailyActivity, aes(x=LightlyActiveMinutes, y=LightActiveDistance, color=weekday)) +geom_point() +stat_smooth(method=lm) +facet_wrap(~weekday) + labs(title = "Light Active-Light Active  Distance Weekdays Performance")
```

    ## `geom_smooth()` using formula = 'y ~ x'

![](Case_Study_2_Bellabeat_files/figure-gfm/higher%20coefficient%20in%20heatmap%20review-4.png)<!-- -->

``` r
#SedentaryMinutes -Light Active Distance Weekdays Performance
ggplot(data=dailyActivity, aes(x=SedentaryMinutes, y=LightActiveDistance, color=weekday)) +geom_point() +stat_smooth(method=lm) +facet_wrap(~weekday) + labs(title = "SedentaryMinutes -Light Active Distance Weekdays Performance")
```

    ## `geom_smooth()` using formula = 'y ~ x'

![](Case_Study_2_Bellabeat_files/figure-gfm/higher%20coefficient%20in%20heatmap%20review-5.png)<!-- -->
Total Distance-Total Steps Weekday Performance, we can see that weekends
has better accomplishment here, - Fairly Minutes-Moderately Distance
Weekday Performance, it is Monday which has the highest yield, followed
by Sunday, it goes in between 4K and 100 minutes daily, being the worst
Friday - Very Active -Very Active Distance Weekday Performance, just as
the first plot Weekends people are doing 200 minutes and almost reaching
15K -Light Active-Light Active Distance Weekday Performance, It has a
more smooth performance that is around 7k daily with also regular
400–500 minutes daily. -Sedentary Minutes -Light Active Distance
Weekdays Performance we can see that here we have a negative correlation
because when more Sedentary Minutes less Light Active Distance reaching
3k daily. Let’s see another relationship.

``` r
ggplot(data=dailyActivity, aes(x=TotalSteps, y=Calories, color=weekday)) +geom_point() +stat_smooth(method=lm) +facet_wrap(~weekday) + labs(title = "Calories-TotalSteps Weekday Performance")
```

    ## `geom_smooth()` using formula = 'y ~ x'

![](Case_Study_2_Bellabeat_files/figure-gfm/plots%20Calories%20-%20TotalSteps-1.png)<!-- -->

``` r
ggplot(data=dailyActivity, aes(x=TotalSteps, y=Calories, color=weekday)) +geom_point() +stat_smooth(method=lm) +facet_wrap(~month)+ labs(title = "Calories-TotalSteps montly Performance")
```

    ## `geom_smooth()` using formula = 'y ~ x'

![](Case_Study_2_Bellabeat_files/figure-gfm/plots%20Calories%20-%20TotalSteps-2.png)<!-- -->

``` r
ggplot(data=dailyActivity, aes(x=TotalDistance, y=Calories, color=weekday)) +geom_point() +stat_smooth(method=lm) +facet_wrap(~weekday) + labs(title = "Calories-TotalDistance Weekday Performance")
```

    ## `geom_smooth()` using formula = 'y ~ x'

![](Case_Study_2_Bellabeat_files/figure-gfm/plots%20Calories%20-%20TotalSteps-3.png)<!-- -->

``` r
ggplot(data=dailyActivity, aes(x=TotalDistance, y=Calories, color=weekday)) +geom_point() +stat_smooth(method=lm) +facet_wrap(~month) + labs(title = "Calories-TotalSteps montly Performance")
```

    ## `geom_smooth()` using formula = 'y ~ x'

![](Case_Study_2_Bellabeat_files/figure-gfm/plots%20Calories%20-%20TotalSteps-4.png)<!-- -->

``` r
ggplot(data=dailyActivity, aes(x=VeryActiveMinutes, y=Calories, color=weekday)) +geom_point() +stat_smooth(method=lm) +facet_wrap(~weekday) + labs(title = "Calories-VeryActiveMinutes Weekday Performance")
```

    ## `geom_smooth()` using formula = 'y ~ x'

![](Case_Study_2_Bellabeat_files/figure-gfm/plots%20Calories%20-%20TotalSteps-5.png)<!-- -->

``` r
ggplot(data=dailyActivity, aes(x=VeryActiveMinutes, y=Calories, color=weekday)) +geom_point() +stat_smooth(method=lm) +facet_wrap(~month) + labs(title = "Calories-VeryActiveMinutes montly Performance")
```

    ## `geom_smooth()` using formula = 'y ~ x'

![](Case_Study_2_Bellabeat_files/figure-gfm/plots%20Calories%20-%20TotalSteps-6.png)<!-- -->
We see that while more steps more calories are consumed by the user
being weekend where it reached 30000 steps in total with 5000 Calories.
If we search by month April was Saturdays and in May Sundays, where even
surpassed the 30000 steps but had a less steeped rise and this
phenomenon is transferred to Calories -Distances and
Calories-VeryActiveMinutes. That suggest the hight intake of calories
make reference to weekends, especially Sundays.

## Refences

- Reference: Calbimonte D. (2021-10-21).”Ways to compare and find
  differences for SQL Server tables and data”. Recovered el 23 December
  of 2022
  <https://www.mssqltips.com/sqlservertip/2779/ways-to-compare-and-find-differences-for-sql-server-tables-and-data>
