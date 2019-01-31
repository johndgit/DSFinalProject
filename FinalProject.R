if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(ada)) install.packages("ada", repos = "http://cran.us.r-project.org")

library(lubridate)

library("rpart")
library("ada")

# San Diego weather dataset . 1.59M rows. 13 features:

dl <- tempfile()
download.file("https://www.dropbox.com/s/l7s5jcddy2ve6wu/minute_weather.csv?dl=1", dl)

WeatherReadings <- read_csv(dl)
#head(WeatherReadings)
# If automatic download does not work, 
# downloan manually from Kaggle or Dropbox with links above and use code line below.

# Read in file that was manually downloaed to local disk.
WeatherReadings <- read_csv("C:/WeatherData/minute_weather.csv")
#Count number of rows
WeatherReadings %>% summarize(count = n())
#Remove rows with NULLS
#WeatherReadings <- WeatherReadings[complete.cases(WeatherReadings),]
WeatherReadings <- na.omit(WeatherReadings)
#Count number of rows after removing rows with NULLS
WeatherReadings %>% summarize(count = n())

#Extract date, hour and minute from datatime field and add as new fields.
WeatherReadings <- WeatherReadings %>% mutate(ReadingDate = date(hpwren_timestamp))
WeatherReadings <- WeatherReadings %>% mutate(ReadingHour = hour(hpwren_timestamp))
WeatherReadings <- WeatherReadings %>% mutate(ReadingMinute = minute(hpwren_timestamp))

# Create new dataframe with only observations at 9AM.
WeatherReadings9A <- WeatherReadings %>% filter(ReadingHour == 9,ReadingMinute == 0) %>%
  select(ReadingDate,air_pressure9am=air_pressure, air_temp9am = air_temp, avg_wind_direction9am = avg_wind_direction, avg_wind_speed9am = avg_wind_speed,max_wind_direction9am = max_wind_direction, max_wind_speed9am = max_wind_speed,min_wind_direction9am = min_wind_direction, min_wind_speed9am = min_wind_speed, rain_accumulation9am = rain_accumulation,rain_duration9am = rain_duration, relative_humidity9am = relative_humidity)

#head(WeatherReadings9A)
# Create new dataframe with only date, time, and  relative humidity observations at 3PM.
WeatherReadings3P <- WeatherReadings %>% filter(ReadingHour == 15,ReadingMinute == 0) %>%
  select(ReadingDate, relative_humidity3pm = relative_humidity)
#head(WeatherReadings3P)

#only want data were we have both a 9AM ann 3PM readings. Combine using inner join.
WeatherReadingsClean <- inner_join(WeatherReadings9A, WeatherReadings3P, by = "ReadingDate")

# Add new file which will contain -1 for low humidity and 1 for high humidity at 3PM.
# High humidy is defined as relative humidity greater than or equal to 24.99% 
WeatherReadingsClean <- WeatherReadingsClean %>% mutate(HighHumidity3pm = case_when(relative_humidity3pm >= 24.99 ~1,TRUE ~ -1) )
#Remove un-needed field ReadingDate.
WeatherReadingsClean <- WeatherReadingsClean[,-1]
#head(WeatherReadingsClean)

#Create training and test sets
# Test set will be 10% of data

set.seed(1)
test_index <- createDataPartition(y = WeatherReadingsClean$HighHumidity3pm, times = 1, p = 0.1, list = FALSE)
train_set <- WeatherReadingsClean[-test_index,]
test_set <- WeatherReadingsClean[test_index,]

WeatherReadingsClean %>% summarize(count = n())
train_set %>% summarize(count = n())
test_set %>% summarize(count = n())

# Train models using default of 50 iterations.
set.seed(1)
adamodelD50 <- ada(HighHumidity3pm ~ air_pressure9am + air_temp9am + avg_wind_direction9am +
                   avg_wind_speed9am + max_wind_direction9am + max_wind_speed9am + 
                   min_wind_direction9am + min_wind_speed9am + rain_accumulation9am +
                   + rain_duration9am + relative_humidity9am
                ,data = train_set, iter=50, type = "discrete")
set.seed(1)
adamodelR50 <- ada(HighHumidity3pm ~ air_pressure9am + air_temp9am + avg_wind_direction9am +
                  avg_wind_speed9am + max_wind_direction9am + max_wind_speed9am + 
                  min_wind_direction9am + min_wind_speed9am + rain_accumulation9am +
                  + rain_duration9am + relative_humidity9am
                ,data = train_set, iter=50, type = "real")
set.seed(1)
adamodelG50 <- ada(HighHumidity3pm ~ air_pressure9am + air_temp9am + avg_wind_direction9am +
                  avg_wind_speed9am + max_wind_direction9am + max_wind_speed9am + 
                  min_wind_direction9am + min_wind_speed9am + rain_accumulation9am +
                  + rain_duration9am + relative_humidity9am
                ,data = train_set, iter=50, type = "gentle")
#show results
summary(adamodelD50)
summary(adamodelR50)
summary(adamodelG50)

#Gentle appears to be the most accurate for this data.
#Train models with different iterations.

set.seed(1)
adamodelG20 <- ada(HighHumidity3pm ~ air_pressure9am + air_temp9am + avg_wind_direction9am +
                   avg_wind_speed9am + max_wind_direction9am + max_wind_speed9am + 
                   min_wind_direction9am + min_wind_speed9am + rain_accumulation9am +
                   + rain_duration9am + relative_humidity9am
                 ,data = train_set, iter=20, type = "gentle")
# Use model to do predictioins on Test data.
predictionsG20 <- predict(adamodelG20,test_set )
#add test dataset
adamodelG20=addtest(adamodelG20,test_set,predictionsG20)

set.seed(1)
adamodelG100 <- ada(HighHumidity3pm ~ air_pressure9am + air_temp9am + avg_wind_direction9am +
                     avg_wind_speed9am + max_wind_direction9am + max_wind_speed9am + 
                     min_wind_direction9am + min_wind_speed9am + rain_accumulation9am +
                     + rain_duration9am + relative_humidity9am
                   ,data = train_set, iter=100, type = "gentle")
# Use model to do predictioins on Test data.
predictionsG100 <- predict(adamodelG100,test_set )
#add test dataset
adamodelG100=addtest(adamodelG100,test_set,predictionsG100)

set.seed(1)
adamodelG200 <- ada(HighHumidity3pm ~ air_pressure9am + air_temp9am + avg_wind_direction9am +
                      avg_wind_speed9am + max_wind_direction9am + max_wind_speed9am + 
                      min_wind_direction9am + min_wind_speed9am + rain_accumulation9am +
                      + rain_duration9am + relative_humidity9am
                    ,data = train_set, iter=200, type = "gentle")
# Use model to do predictioins on Test data.
predictionsG200 <- predict(adamodelG200,test_set )
#add test dataset
adamodelG200=addtest(adamodelG200,test_set,predictionsG200)


# Use model to do predictioins on Test data.
predictionsG50 <- predict(adamodelG50,test_set )
#add test dataset
adamodelG50=addtest(adamodelG50,test_set,predictionsG50)

#show results
summary(adamodelG20)
plot(adamodelG20, kappa = TRUE, test=TRUE
     ,cols= rainbow(dim(adamodelG20$model$errs)[2]+1)
     ,tflag=TRUE,mfrow=c(2,1),mar=c(2, 2.5, 1.5, .5))
summary(adamodelG50)
plot(adamodelG50, kappa = TRUE, test=TRUE
     ,cols= rainbow(dim(adamodelG50$model$errs)[2]+1)
     ,tflag=TRUE,mfrow=c(2,1),mar=c(2, 2.5, 1.5, .5))
summary(adamodelG100)
plot(adamodelG100, kappa = TRUE, test=TRUE
     ,cols= rainbow(dim(adamodelG100$model$errs)[2]+1)
     ,tflag=TRUE,mfrow=c(2,1),mar=c(2, 2.5, 1.5, .5))
summary(adamodelG200)
plot(adamodelG200, kappa = TRUE, test=TRUE
     ,cols= rainbow(dim(adamodelG200$model$errs)[2]+1)
     ,tflag=TRUE,mfrow=c(2,1),mar=c(2, 2.5, 1.5, .5))

# The more iteratioins we go though in traing the higher the accuracy.  
# However as we set iteratins higher the improvement in accuracy grows smaller.
# As we approach and go beyond 100 the incremental improvment greatly diminishes so we will use 100.

#set.seed(1)
#adamodelG125 <- ada(HighHumidity3pm ~ air_pressure9am + air_temp9am + avg_wind_direction9am +
 #                     avg_wind_speed9am + max_wind_direction9am + max_wind_speed9am + 
 #                     min_wind_direction9am + min_wind_speed9am + rain_accumulation9am +
 #                     + rain_duration9am + relative_humidity9am
 #                   ,data = train_set, iter=125, type = "gentle")
# Use model to do predictioins on Test data.
#predictionsG125 <- predict(adamodelG125,test_set )
#add test dataset
#adamodelG125=addtest(adamodelG125,test_set,predictionsG100)
#summary(adamodelG125)
#plot(adamodelG125, kappa = TRUE, test=TRUE
#     ,cols= rainbow(dim(adamodelG125$model$errs)[2]+1)
#     ,tflag=TRUE,mfrow=c(2,1),mar=c(2, 2.5, 1.5, .5))

#Create plot showing importance of each variable in predictions.
varplot(adamodelG100,plot.it = TRUE)
#Create plots showing kappa and errors across iterations for traing and testing.

#Show final results
#summary(adamodelG100)
#plot(adamodelG100,TRUE,TRUE)
#adamodelG100$confusion

#head(adamodel)
#pairs(adamodelG100,train_set[,-13],maxvar=2)
#varplot(adamodelG100,plot.it = TRUE,type = "scores")
#print(adamodelG100)
#Cleanup
rm(dl, WeatherReadings, WeatherReadings9A,WeatherReadings3P, test_index)
