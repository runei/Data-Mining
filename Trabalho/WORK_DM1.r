### Practical Assigment Data Minning
#### The folloing work is devided into 3 maisn parts:
# 1. Data Cleaning and transformation
# 2. Data Sumarization and analisys
# 3. Prediction


## Part 1

#####1.1- Reading the xls file, with "-" as non an Value.
##### Initialy it tries to guess the columns types. 
library(readxl)
library(dplyr)
library(lubridate)

myData <- read_excel("crime.xls", sheet=1, col_names = TRUE, col_types = NULL, na = "-")
head(myData)


#####1.2- Check if variable is a data frame format.
(is.data.frame(myData))


#####1.3- Verify if the columns type are in the correct format.
sapply(myData, class)
#Or using extra goddies such as levels
str(myData)


#####1.4- Date and Hour are in the incorrect column type. Convert column type.
myData$Date <- as.Date( myData$Date, '%Y/%m/%d') #Convert as Date
myData$Hour <- as.numeric(myData$Hour) #Converto to numeric 


#####1.5- The column names contain messy caracther that dificult use. Replacing messy columns name with something more simplier.
colnames(myData)

names(myData) <-gsub("'", "", names(myData), fixed=TRUE)
names(myData) <-gsub("#", "n", names(myData), fixed=TRUE)
names(myData) <-gsub(" ", "_", names(myData), fixed=TRUE)

colnames(myData)


#####1.6- Let?s analise the missing cases and witch colums they appear.
colSums(is.na(myData)) #Using vectorization
apply(myData, 2, function(x) length(which(is.na(x)))) #Using apply


#####1.7- Quick analisys into the number of cases.
(nrow(myData))


#####1.8- Checking the window time frame of the dataset.
firstOcc <- min(myData$Date)
lastOcc <- max(myData$Date)
(timeframe <- lastOcc-firstOcc)




## PART 2 - Sumarization

#####2.1- Whats the average offences per hour in descent order?  
group_by(myData, Hour) %>% 
  summarize(meanOff= mean(n_offenses, na.rm=TRUE)) %>%
  arrange(desc(meanOff)) 



#####2.2- Witch  time of the day (hour) occurs more offences?
group_by(myData, Hour) %>%
  summarize(counts= sum(n_offenses, na.rm=TRUE)) %>%
  arrange(desc(counts)) %>%
  slice(1) #Get the peak


#####2.3- In witch areas(Beats) occur more offences? Order descent
group_by(myData, Beat) %>% 
  summarise(counts= sum(n_offenses, na.rm=TRUE)) %>%
  arrange(desc(counts))


#####2.4- In Wicht areas(Beats) occurs more offenses?
group_by(myData, Beat) %>%
  summarise(counts= sum(n_offenses, na.rm=TRUE)) %>%
  arrange(desc(counts)) %>% 
  slice(1)


#####2.5- What genre of offences are more frequent in the city? 
library(ggplot2)

ggplot(myData, aes(x= myData$Offense_Type)) +
  geom_bar()+
  ggtitle("Number of offences per type")+
  ylab("# occurences")+
  xlab("Beat Code")


#####2.5- What genre of offences occur more often in the city?
group_by(myData, Offense_Type) %>%
  summarise(counts= sum(n_offenses, na.rm=TRUE)) %>%
  arrange(desc(counts))



#####2.6(TODO)- What genre of offenses occur more often per beat?
ggplot(myData, aes(x= myData$Beat, weights=n_offenses)) +
  geom_bar()+
  ggtitle("Number of offences per beat")


#####2.6(TODO)- What genre of offenses occur more often per beat? (As table)
myData %>%
  group_by(Offense_Type,Beat)%>%
  summarise(counts=sum(n_offenses)) %>%
  arrange(desc(counts))


#####2.7- What Are the more common promises?
group_by(myData, Premise) %>%
  summarise(counts= sum(n_offenses, na.rm=TRUE)) %>%
  arrange(desc(counts))



#####2.8- Show weekdays were occcur more offenses.
myDataDummy <-myData #Make local copy
myDataDummy$Week_Day <- wday(myDataDummy$Date)# Get Weekday

group_by(myDataDummy, Week_Day) %>%
  summarise(counts= sum(n_offenses, na.rm=TRUE)) %>%
  arrange(desc(counts))
#Or
myDataWeek <-myData # dummy copy
myDataWeek$day_of_week <- format(myDataWeek$Date, "%A")

group_by(myDataWeek, day_of_week) %>%
  summarise(counts= sum(n_offenses, na.rm=TRUE)) %>%
  arrange(desc(counts))



#####2.9- Show the Week day were occurs more offenses.
group_by(myDataWeek, day_of_week) %>%
  summarise(counts= sum(n_offenses, na.rm=TRUE)) %>%
  arrange(desc(counts)) %>%
  slice(1)



#####2.10- Show weekdays were occcur more offenses. (Histogram)
ggplot(myDataWeek,aes(x=day_of_week,weights=n_offenses))+
  geom_bar() +
  ggtitle('Histogram of offences by Week Day')

## Using daycode instead of day name
myDataDummy <-myData #Make local copy
myDataDummy$Week_Day <- wday(myDataDummy$Date)# Get Weekday

ggplot(myDataDummy,aes(x=Week_Day,weights=n_offenses))+
  geom_bar() +
  ggtitle('Histogram of offences by Week Day')




#####2.11- What genre of offenses occur more often per day period.
byPeriod <- myData
byPeriod$Day_Period[byPeriod$Hour>=8 &
                      byPeriod$Hour <12]<- "Morning"
byPeriod$Day_Period[byPeriod$Hour>=12 &
                      byPeriod$Hour <19]<- "Afternoon"
byPeriod$Day_Period[(byPeriod$Hour>=19 &
                       byPeriod$Hour <=23) |
                      (byPeriod$Hour>=0 &
                         byPeriod$Hour <8)]<- "Night"

group_by(byPeriod, Day_Period) %>%
  summarise(counts= sum(n_offenses, na.rm=TRUE)) %>%
  arrange(desc(counts))



#####2.11- What genre of offenses occur more often per day period(Histogram).
ggplot(byPeriod,aes(x=Day_Period,weights=n_offenses))+
  geom_bar() +
  ggtitle('Histogram of offences by Day Period')



## MOre Asks to do
##### witch genre of offences occur more offen per premises

##### Show evolution of offences per year

##### show per yaer beatcode
##### witch year occurs more more offenses
##### with month occurs more offences


## Analise
# Witch month occurs more offences
short.date = strftime(myData$Date, "%Y/%m")
aggr.stat = aggregate(myData$n_offenses ~ short.date, FUN = sum)
# Note: only one month basicaly, january 2015 and december

# Witch days of the week occurs more offences
myDataWeek <-myData # dummy copy
myDataWeek$day_of_week <- format(myDataWeek$Date, "%A")

group_by(myDataWeek, day_of_week) %>%
  summarise(counts= sum(n_offenses, na.rm=TRUE)) %>%
  arrange(desc(counts))

# Get the max
group_by(myDataWeek, day_of_week) %>%
  summarise(counts= sum(n_offenses, na.rm=TRUE)) %>%
  arrange(desc(counts)) %>%
  slice(1)

# As graph
ggplot(myDataWeek, aes(x= myDataWeek$day_of_week)) +
  geom_bar()+
  ggtitle("Number of offences per week day")+
  ylab("# occurences")+
  xlab("Week day")
  
# HERE
# genre of offences more common per weekday
m <- aggregate(n_offenses ~ day_of_week +
            Offense_Type, data=myDataWeek, sum)

# m %>%
#   group_by(day_of_week, Offense_Type, n_offenses) %>%
#   arrange(desc(n_offenses)) %>%
  

# TODO Get the common offence pr weekday day
m[order(m$n_offenses, decreasing=TRUE),] # ordeno

# Get the more comon offence for each weekday
m %>%
  group_by(day_of_week, Offense_Type) %>%
  summarise(maxOffenses=sum(n_offenses))


  
# Genre of offences more common per weekday per Beat
m <- aggregate(myDataWeek$n_offenses ~ + Beat+ day_of_week +
                 Offense_Type , data=myDataWeek, sum)


 
# PART 3

## Format data Column(If neeeded)
library(lubridate)
myData <- transform(myData, Date = ymd(Date))
myDataCopy <- myData # dummy copy

## Create colum with day period
myDataCopy$Day_Period[myDataCopy$Hour>=8 & myDataCopy$Hour <12]<- "Morning"
myDataCopy$Day_Period[myDataCopy$Hour>=12 & myDataCopy$Hour <19]<- "Afternoon"
myDataCopy$Day_Period[(myDataCopy$Hour>=19 & myDataCopy$Hour <=23) |
                        (myDataCopy$Hour>=0 & myDataCopy$Hour <8)]<- "Night"


## CHECK Using sapply
getDayPeriod <- function(x){
  if(x >=8 & x <12) y <-"Morning"
  if(x >=12 & x <19) y <-"Afternoon"
  if((x >=19 & x<=23)|(x>=0 & x <8)) y <-"Night"
  return (y)
}

#myDataCopy$Day_Period <- sapply(myDataCopy$Hour, getDayPeriod)


## Create colum with day week
myDataCopy$Week_Day <- wday(myDataCopy$Date)

# There is an 1.0000 messing around
is.na(myDataCopy$Offense_Type) <- myDataCopy$Offense_Type == "1.000000"
myDataCopy$Offense_Type <- factor(myDataCopy$Offense_Type)

# Remove the line were the NA as placed
myDataCopy<-myDataCopy[!is.na(myDataCopy$Offense_Type),]

## Get Beat Codes vector
beatCodesVector <- unique(myDataCopy$Beat)
weekDaysVector <- unique(myDataCopy$Week_Day)
dayPeriodsVector <- unique(myDataCopy$Day_Period)

# Select only the columns that are relevant for training
setTrainData <-subset(myDataCopy, select=c("Date", "Hour",
                                           "Offense_Type",
                                           "Beat",
                                           "n_offenses",
                                           "Day_Period",
                                           "Week_Day"))
# ## Dummy copy
# d<-setTrainData
# ## Convert to factor
# d$Beat <- as.factor(d$Beat)
# d$Day_Period <- as.factor(d$Day_Period)
# d$Offense_Type <-as.factor(d$Offense_Type)
# 
# 
# ## Convert data to format enabling to apply algorithms
# l=unique(c(as.character(d$Beat),
#            as.character(d$Day_Period),
#            as.character(d$Premise)))

# ## Generate new dataframe
# setTrainData <- data.frame(N_Beat=as.numeric(factor(d$Beat, levels=l)),
#                  N_Day_Period=as.numeric(factor(d$Day_Period, levels=l)),
#                  N_Premise=as.numeric(factor(d$Premise, levels=l)),
#                  N_offenses=d$n_offenses,
#                  N_Hour=d$Hour,
#                  N_Week_day = d$Week_Day,
#                  Beat=d$Beat,
#                  Offenses_Type=d$Offense_Type,
#                  Premise = d$Premise
#                  )

## Establish relations between codes and names (lookup table)
# levelsBeat = unique(setTrainData$N_Beat)
# levelsDayPeriod = unique(setTrainData$N_Day_Period)
# levelsPremise = unique(setTrainData$N_Premise)
# levelsWeekDay = unique(setTrainData$N_Week_day)





# Split interval for data
splitVal <- 0.7

# Extract samples
set.seed(1234) # for reproduction
idx.tr <-sample(1:nrow(setTrainData),as.integer(splitVal*nrow(setTrainData)))
tr <- setTrainData[idx.tr, ] #Split into two sets
ts <- setTrainData[-idx.tr,] 




# Training stage
######## 1- Using Trees ##############
library(DMwR)
library(rpart.plot)
model_AC <- rpartXse(n_offenses ~ .,tr)
preds <- predict(model_AC,ts)

### Error Analysis ###
## Mean Square error
(mse <- mean((preds -ts$n_offenses)^2))

## Mean Absolute error
(mae <- mean(abs( preds - tr$n_offenses)))

## Normalized Mean Square error
nmse <- sum((preds - ts$n_offenses)^2) /
  sum((ts$n_offenses-mean(ts$n_offenses))^2)

## Normalized Mean Absolute Error
nmae <- sum(abs(preds - ts$n_offenses)) /
  sum(abs(ts$n_offenses-mean(ts$n_offenses)))

## Mean Average Percentage Error
mape <- mean(abs(preds-ts$n_offenses)/ts$n_offenses)

# Correlation
corr <- cor(ts$n_offenses,preds)
######### END - Trees ############




######## 2- Using Naive Bayes ###########
library(e1071)
model_NB <- naiveBayes(n_offenses ~ ., tr)
preds <- predict(model_NB, ts)

### Error Analysis ###
## Mean Square error
(mse <- mean((preds -ts$n_offenses)^2))

## Mean Absolute error
(mae <- mean(abs( preds - tr$n_offenses)))

## Normalized Mean Square error
nmse <- sum((preds - ts$n_offenses)^2) /
  sum((ts$n_offenses-mean(ts$n_offenses))^2)

## Normalized Mean Absolute Error
nmae <- sum(abs(preds - ts$n_offenses)) /
  sum(abs(ts$n_offenses-mean(ts$n_offenses)))

## Mean Average Percentage Error
mape <- mean(abs(preds-ts$n_offenses)/ts$n_offenses)

# Correlation
corr <- cor(ts$n_offenses,preds)
######## END - Naive Bayes ###########


######## 3- Using kNN ############
library(class)
library(DMwR)
model_KNN <-kNN(n_offenses ~ .,tr,ts,k=3,norm=TRUE)

### Error Analysis ###
## Mean Square error
(mse <- mean((preds -ts$n_offenses)^2))

## Mean Absolute error
(mae <- mean(abs( preds - tr$n_offenses)))

## Normalized Mean Square error
nmse <- sum((preds - ts$n_offenses)^2) /
  sum((ts$n_offenses-mean(ts$n_offenses))^2)

## Normalized Mean Absolute Error
nmae <- sum(abs(preds - ts$n_offenses)) /
  sum(abs(ts$n_offenses-mean(ts$n_offenses)))

## Mean Average Percentage Error
mape <- mean(abs(preds-ts$n_offenses)/ts$n_offenses)

# Correlation
corr <- cor(ts$n_offenses,preds)
######## END - kNN ###########



######## 4- Using SVM ############
library(e1071)
model_SVM <- svm(n_offenses ~., tr, cost =10, epsilon=0.02)
preds <- predict(model_SVM,ts)

### Error Analysis ###
## Mean Square error
(mse <- mean((preds -ts$n_offenses)^2))

## Mean Absolute error
(mae <- mean(abs( preds - tr$n_offenses)))

## Normalized Mean Square error
nmse <- sum((preds - ts$n_offenses)^2) /
  sum((ts$n_offenses-mean(ts$n_offenses))^2)

## Normalized Mean Absolute Error
nmae <- sum(abs(preds - ts$n_offenses)) /
  sum(abs(ts$n_offenses-mean(ts$n_offenses)))

## Mean Average Percentage Error
mape <- mean(abs(preds-ts$n_offenses)/ts$n_offenses)

# Correlation
corr <- cor(ts$n_offenses,preds)
######## END - SVM ###########



######## 5- Using NN ############
library(nnet)
model_NN <- nnet(n_offenses ~., tr, size=5, decay=0.1, maxit=1000)
preds <- predict(model_NN,ts, type='class')

### Error Analysis ###
## Mean Square error
(mse <- mean((preds -ts$n_offenses)^2))

## Mean Absolute error
(mae <- mean(abs( preds - tr$n_offenses)))

## Normalized Mean Square error
nmse <- sum((preds - ts$n_offenses)^2) /
  sum((ts$n_offenses-mean(ts$n_offenses))^2)

## Normalized Mean Absolute Error
nmae <- sum(abs(preds - ts$n_offenses)) /
  sum(abs(ts$n_offenses-mean(ts$n_offenses)))

## Mean Average Percentage Error
mape <- mean(abs(preds-ts$n_offenses)/ts$n_offenses)

# Correlation
corr <- cor(ts$n_offenses,preds)
######## END - NN ###########



######## 6- Using MARS ############
model_MARS <- earth(n_offenses ~., tr)
preds <- predict(model_MARS, ts)

### Error Analysis ###
## Mean Square error
(mse <- mean((preds -ts$n_offenses)^2))

## Mean Absolute error
(mae <- mean(abs( preds - tr$n_offenses)))

## Normalized Mean Square error
nmse <- sum((preds - ts$n_offenses)^2) /
  sum((ts$n_offenses-mean(ts$n_offenses))^2)

## Normalized Mean Absolute Error
nmae <- sum(abs(preds - ts$n_offenses)) /
  sum(abs(ts$n_offenses-mean(ts$n_offenses)))

## Mean Average Percentage Error
mape <- mean(abs(preds-ts$n_offenses)/ts$n_offenses)

# Correlation
corr <- cor(ts$n_offenses,preds)
######## END - MARS ###########



######## 7- Using Simple bagging ############
simpleBagging <- function(form,data,model='rpartXse',nModels=100,...) {
  ms <- list()
  n <- nrow(data)
  for(i in 1:nModels) {
    tr <- sample(n,n,replace=T)
    ms[[i]] <- do.call(model,c(list(form,data[tr,]),...))
  }
  ms
}

predict.simpleBagging <- function(models,test) {
  ps <- sapply(models,function(m) predict(m,test))
  apply(ps,1,mean)
}

library(DMwR)
model_BAG <- simpleBagging(n_offenses ~., tr, nModels=300, se=0.5)
preds <- predict.simpleBagging(model_BAG, ts)

### Error Analysis ###
## Mean Square error
(mse <- mean((preds -ts$n_offenses)^2))

## Mean Absolute error
(mae <- mean(abs( preds - tr$n_offenses)))

## Normalized Mean Square error
nmse <- sum((preds - ts$n_offenses)^2) /
  sum((ts$n_offenses-mean(ts$n_offenses))^2)

## Normalized Mean Absolute Error
nmae <- sum(abs(preds - ts$n_offenses)) /
  sum(abs(ts$n_offenses-mean(ts$n_offenses)))

## Mean Average Percentage Error
mape <- mean(abs(preds-ts$n_offenses)/ts$n_offenses)

# Correlation
corr <- cor(ts$n_offenses,preds)
######## END - Simple Bagging ###########



######## 8- Using Advance bagging ############
library(DMwR)
model_ADBAG <- bagging(n_offenses ~ ., tr,mfinal=50)
preds <- predict(model_ADBAG, ts)

### Error Analysis ###
## Mean Square error
(mse <- mean((preds -ts$n_offenses)^2))

## Mean Absolute error
(mae <- mean(abs( preds - tr$n_offenses)))

## Normalized Mean Square error
nmse <- sum((preds - ts$n_offenses)^2) /
  sum((ts$n_offenses-mean(ts$n_offenses))^2)

## Normalized Mean Absolute Error
nmae <- sum(abs(preds - ts$n_offenses)) /
  sum(abs(ts$n_offenses-mean(ts$n_offenses)))

## Mean Average Percentage Error
mape <- mean(abs(preds-ts$n_offenses)/ts$n_offenses)

# Correlation
corr <- cor(ts$n_offenses,preds)
######## END - Advance Bagging ###########


######## 9- Using Random forest ############
library(randomForest)

model_RF <- randomForest(medv ~ ., tr)
preds <- predict(model_RF,ts)

### Error Analysis ###
## Mean Square error
(mse <- mean((preds -ts$n_offenses)^2))

## Mean Absolute error
(mae <- mean(abs( preds - tr$n_offenses)))

## Normalized Mean Square error
nmse <- sum((preds - ts$n_offenses)^2) /
  sum((ts$n_offenses-mean(ts$n_offenses))^2)

## Normalized Mean Absolute Error
nmae <- sum(abs(preds - ts$n_offenses)) /
  sum(abs(ts$n_offenses-mean(ts$n_offenses)))

## Mean Average Percentage Error
mape <- mean(abs(preds-ts$n_offenses)/ts$n_offenses)

# Correlation
corr <- cor(ts$n_offenses,preds)
######## END - Random forest ###########


######## 10- Using Boosting ############
model_BOOST <- boosting(n_offenses ~ ., tr)
preds <- predict(model_BOOST, ts)

### Error Analysis ###
## Mean Square error
(mse <- mean((preds -ts$n_offenses)^2))

## Mean Absolute error
(mae <- mean(abs( preds - tr$n_offenses)))

## Normalized Mean Square error
nmse <- sum((preds - ts$n_offenses)^2) /
  sum((ts$n_offenses-mean(ts$n_offenses))^2)

## Normalized Mean Absolute Error
nmae <- sum(abs(preds - ts$n_offenses)) /
  sum(abs(ts$n_offenses-mean(ts$n_offenses)))

## Mean Average Percentage Error
mape <- mean(abs(preds-ts$n_offenses)/ts$n_offenses)

# Correlation
corr <- cor(ts$n_offenses,preds)
######## END - Using Boosting ###########



######## 11- Using AdaBoostRegression ############
model_GBM <- gbm(n_offenses ~ .,distribution='gaussian',data=tr,
                 n.trees=20000,verbose=F)
preds <- predict(model_GBM, ts, type='response', n.trees=20000)

### Error Analysis ###
## Mean Square error
(mse <- mean((preds -ts$n_offenses)^2))

## Mean Absolute error
(mae <- mean(abs( preds - tr$n_offenses)))

## Normalized Mean Square error
nmse <- sum((preds - ts$n_offenses)^2) /
  sum((ts$n_offenses-mean(ts$n_offenses))^2)

## Normalized Mean Absolute Error
nmae <- sum(abs(preds - ts$n_offenses)) /
  sum(abs(ts$n_offenses-mean(ts$n_offenses)))

## Mean Average Percentage Error
mape <- mean(abs(preds-ts$n_offenses)/ts$n_offenses)

# Correlation
corr <- cor(ts$n_offenses,preds)
######## END - AdaBoostRegression ###########

### Predict for the several#####

####### Make predictions using vectors #####
for(beatCodeIDX in beatCodesVector)
  for(weekDayIDX in weekDaysVector)
    for(dayPeriodIDX in dayPeriodsVector){
      ### Lest precict usnog the vector and each model
      
      #test <- df
      
    }



#### OLD stuff

for (beatCodeIDX in beatCodesVector){
  # Extract the data
  setData <- myDataCopy[myDataCopy$Beat ==beatCodeIDX,]
  #Or using filter
  setData <- filter(myDataCopy, Beat  == beatCode)
  
  # Extract sample
  idx.tr <-sample(1:nrow(setData),as.integer(splitVal*nrow(setData)))
  tr <- setData[idx.tr, ] #Split into two sets
  ts <- setData[-idx.tr,] 
  # Lets Train, CHECK invalid cases
  mdl <-lm(n_offenses ~ setData$Hour +
             setData$Offense_Type, tr$Beat==beatCodeIDX , tr)
  model <- append(mdl) 
  
  
}

# For each beat code, day and weekday make preds


for (beatCodeIDX in beatCodesVector){
  for(weekDayIDX in weekDaysVector ){
    for( dayPeriodIDX in dayPeriodsVector){
      # Extract the data
      (beatCodeIDX)
      (weekDayIDX)
      (dayPeriodIDX)
      setData <- myDataCopy[(myDataCopy$Beat ==beatCodeIDX &
                               myDataCopy$Week_Day == weekDayIDX &
                               myDataCopy$Day_Period==dayPeriodIDX),]
      #Or using filter
      setData <- filter(myDataCopy, Beat  == beatCode,
                        Week_Day == weekDay,
                        Day_Period==dayPeriod)
      
      # Extract sample
      idx.tr <-sample(1:nrow(setData),as.integer(splitVal*nrow(setData)))
      tr <- setData[idx.tr, ] #Split into two sets
      ts <- setData[-idx.tr,] 
      # Lets Train
      #l <-lm(n_offenses ~., tr)
      
      
    }
  }
}

## Version 2

## Format data Column(If neeeded)
library(lubridate)
myData <- transform(myData, Date = ymd(Date))

## Subset into there day intervals categories
morning <- subset(myData, Hour >=8 & Hour <12)
afternoon <- subset(myData, Hour >=12 & Hour<19)
night <- subset(myData, (Hour >=19 & Hour <= 23)| (Hour>=0 & Hour <8)) 

## Create now weekday
morning$WeekDay <- wday(morning$Date)
afternoon$WeekDay <- wday(afternoon$Date)
night$WeekDay <- wday(night$Date)

## Subset per weekday // Ok we can select those
morningSun <- subset(morning, WeekDay==1)
morningMon <- subset(morning, WeekDay==2)
morningTue <- subset(morning, WeekDay==3)
morningWed <- subset(morning, WeekDay==4)
morningThu <- subset(morning, WeekDay==5)
morningFry <- subset(morning, WeekDay==6)
morningSat <- subset(morning, WeekDay==7)
## Afternoon
afternoonSun <- subset(afternoon, WeekDay==1)
afternoonMon <- subset(afternoon, WeekDay==2)
afternoonTue <- subset(afternoon, WeekDay==3)
afternoonWed <- subset(afternoon, WeekDay==4)
afternoonThu <- subset(afternoon, WeekDay==5)
afternoonFry <- subset(afternoon, WeekDay==6)
afternoonSat <- subset(afternoon, WeekDay==7)
## night
nightSun <- subset(night, WeekDay==1)
nightMon <- subset(night, WeekDay==2)
nightTue <- subset(night, WeekDay==3)
nightWed <- subset(night, WeekDay==4)
nightThu <- subset(night, WeekDay==5)
nightFry <- subset(night, WeekDay==6)
nightSat <- subset(night, WeekDay==7)

## Split by beat
## Get the beatcodes vector
beatCodes <- unique(myData$Beat)




myDataByMonth<-split(myData, format(myData$Date, "%Y-%m"))

# Create Subsets by weekdays
myDataByMon <- subset(dat, wday(Date, label = T) == 'Mon')
myDataByTue <- subset(dat, wday(Date, label = T) == 'Tue')#Check
myDataByWed <- subset(dat, wday(Date, label = T) == 'Wed')
myDataByThu <- subset(dat, wday(Date, label = T) == 'Thu')#Check
myDataByThu <- subset(dat, wday(Date, label = T) == 'Fry')

myDataBySat <- subset(dat, wday(Date, label = T) == 'Sat')
myDataBySun <- subset(dat, wday(Date, label = T) == 'Sun')

# Or just create an new collum with weekday id
dat$Week_Day <- wday(dat$Date)

# Now factor hour by id (morning, afternon, night)
library(Hmisc)
# TODO need to join data
# ChECK REF: http://stackoverflow.com/questions/21848211/converting-r-time-column-to-specific-strings

# Split data per morning, affternoon, night
myDataByDayPart <- myData
myDataByDayPart$DayPart <- cut(myDataByDayPart$Hour, breaks = c(8,12, 19, 7),include.lowest=TRUE, labels=c("Morning", "Afternoon","Night"))



