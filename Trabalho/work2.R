## Part 1

#Libraries:
library(readxl)
library(dplyr)
library(ggplot2)
library(lubridate)
library(DMwR)

# Reading the file
# "File: crime.xls, number of sheets: 1, column names are given: TRUE, na : "-"
policeData <- read_excel("crime.xls", sheet=1, col_names = TRUE, col_types = NULL, na = "-")

# To be able to work with the data, we have to check, if they are in the right format. For example
# we need for date a date type

sapply(policeData, class)

#Important columns are:  Date, Hour, Offense Type, Beat, #offenses
# We have to convert: Date-> date, Hour-> numeric, 

policeData$Date<-ymd(policeData$Date)
policeData$Hour<-as.numeric(policeData$Hour)

# Remove characters like white Space:


names(policeData) <-gsub("'", "", names(policeData), fixed=TRUE)
names(policeData) <-gsub("#", "n", names(policeData), fixed=TRUE)
names(policeData) <-gsub(" ", "_", names(policeData), fixed=TRUE)


#Getting Values represented in offense type
table(policeData$Offense_Type)
# Since the value is not an offense type, we have to replace the value "1" with "NA"
policeData$Offense_Type[policeData$Offense_Type=="1.000000"]<-NA


# Creating informations about the shifts

shift1<-policeData[policeData$Hour>=8&policeData$Hour<12,]
shift2<-policeData[policeData$Hour>=12&policeData$Hour<19,]
shift3<-rbind(policeData[policeData$Hour>19,],policeData[policeData$Hour<8,])

#adding the week day as an additional information

policeDataM<-mutate(policeData,day=wday(Date, label=TRUE))

shift1M<-mutate(shift1,day=wday(Date, label=TRUE))
shift2M<-mutate(shift2,day=wday(Date, label=TRUE))
shift3M<-mutate(shift3,day=wday(Date,label=TRUE))


#adding the day of the month as an additional information

policeDataM2 <-mutate(policeData, day=day(Date))
shift1M2<-mutate(shift1,day=day(Date))
shift2M2<-mutate(shift2,day=day(Date))
shift3M2<-mutate(shift3,day=day(Date))

## To get an overview, we will try to find out how many data we have:
# Data:
nrow(policeData)
nrow(shift1)
nrow(shift2)
nrow(shift3)
# number of Beats
nrow(count(policeData,Beat))
#number of different offense Types
nrow(count(policeData,Offense_Type))
# From Which year to which year are data given:
min(year(policeData$Date))
max(year(policeData$Date))
#From which month:
table(month(policeData$Date, label=TRUE))
#From which days:
table(wday(policeData$Date,label=TRUE))




### Part 2   Illustrating the data:


 
#Time-> number of offenses sorted
head(group_by(policeData,Hour)%>%summarise(no=sum(n_offenses))%>%arrange(desc(no)))


#
#Week Day-> number of offenses sorted and unsorted
group_by(policeData,wday(Date,label=TRUE))%>%summarise(sum=sum(n_offenses, na.rm=TRUE))%>%arrange(desc(sum))
group_by(policeData,wday(Date,label=TRUE))%>%summarise(sum=sum(n_offenses, na.rm=TRUE))

#day
group_by(policeData,day(Date))%>%summarise(sum=sum(n_offenses, na.rm=TRUE))%>%arrange(desc(sum))
  #- District ->amount 	+
  
#- Type (maximum and minimum district) -
maxBeat <-group_by(policeData,Beat)%>%summarise(num=sum(n_offenses, na.rm=TRUE))%>%arrange(desc(num)) 
head(maxBeat)
tail(maxBeat)

# Whats the average offences per hour in descent order?  
group_by(policeData, Hour) %>% 
  summarise(meanOff= mean(n_offenses, na.rm=TRUE)) %>%
  arrange(desc(meanOff)) 

#on which of the three shift are occuring how many offenses:

sum(shift1$n_offenses)
sum(shift2$n_offenses)
sum(shift3$n_offenses)



#Analysis of Offense Type

group_by(policeData[!is.na(policeData$Offense_Type),], Offense_Type) %>%
  summarise(counts= sum(n_offenses, na.rm=TRUE)) %>%
  arrange(desc(counts))

group_by(shift1[!is.na(shift1$Offense_Type),], Offense_Type) %>%
  summarise(counts= sum(n_offenses, na.rm=TRUE)) %>%
  arrange(desc(counts))

group_by(shift2[!is.na(shift2$Offense_Type),], Offense_Type) %>%
  summarise(counts= sum(n_offenses, na.rm=TRUE)) %>%
  arrange(desc(counts))

group_by(shift3[!is.na(shift3$Offense_Type),], Offense_Type) %>%
  summarise(counts= sum(n_offenses, na.rm=TRUE)) %>%
  arrange(desc(counts))



#####Information about premises
group_by(policeData, Premise) %>%
  summarise(counts= sum(n_offenses, na.rm=TRUE)) %>%
  arrange(desc(counts))
#divided by shifts:
group_by(shift1, Premise) %>%
  summarise(counts= sum(n_offenses, na.rm=TRUE)) %>%
  arrange(desc(counts))
group_by(shift2, Premise) %>%
  summarise(counts= sum(n_offenses, na.rm=TRUE)) %>%
  arrange(desc(counts))
group_by(shift3, Premise) %>%
  summarise(counts= sum(n_offenses, na.rm=TRUE)) %>%
  arrange(desc(counts))


#############plotting stuff:
# Number of offences per day of month


ggplot(policeDataM2,aes(x=day,weights=n_offenses))+geom_bar() +  ggtitle('Histogram of offences by Week Day')

ggplot(shift1M2,aes(x=day,weights=n_offenses))+geom_bar() +  ggtitle('Histogram of offences by Week Day')
ggplot(shift2M2,aes(x=day,weights=n_offenses))+geom_bar() +  ggtitle('Histogram of offences by Week Day')
ggplot(shift3M2,aes(x=day,weights=n_offenses))+geom_bar() +  ggtitle('Histogram of offences by Week Day')

#Number of offences per type
ggplot(policeData, aes(x= policeData$Offense_Type)) +
  geom_bar()+
  ggtitle("Number of offences per type")+
  ylab("number occurences")+
  xlab("Beat Code")


#divided by shift
ggplot(shift1, aes(x= shift1$Offense_Type)) +
  geom_bar()+
  ggtitle("Number of offences per type")+
  ylab("number occurences")+
  xlab("Beat Code")

#divided by shift
ggplot(shift2, aes(x= shift2$Offense_Type)) +
  geom_bar()+
  ggtitle("Number of offences per type")+
  ylab("number occurences")+
  xlab("Beat Code")


#divided by shift
ggplot(shift3, aes(x= shift3$Offense_Type)) +
  geom_bar()+
  ggtitle("Number of offences per type")+
  ylab("number occurences")+
  xlab("Beat Code")


##offense type
#General
ggplot(policeData, aes(x= policeData$Offense_Type)) +
  geom_bar()+
  ggtitle("Number of offences per type")+
  ylab("number occurences")+
  xlab("Beat Code")

# divided by shifts

ggplot(shift1, aes(x= shift1$Offense_Type)) +
  geom_bar()+
  ggtitle("Number of offences per type")+
  ylab("number occurences")+
  xlab("Beat Code")

ggplot(shift2, aes(x= shift2$Offense_Type)) +
  geom_bar()+
  ggtitle("Number of offences per type")+
  ylab("number occurences")+
  xlab("Beat Code")

ggplot(shift3, aes(x= shift3$Offense_Type)) +
  geom_bar()+
  ggtitle("Number of offences per type")+
  ylab("number occurences")+
  xlab("Beat Code")

##divided by days
#general
ggplot(policeDataM, aes(x= policeDataM$Offense_Type)) +geom_bar()+ggtitle("Number of offences per type")+ ylab("number occurences")+ xlab("Beat Code")+facet_wrap(~day)
#shift 1
ggplot(shift1M, aes(x= shift1M$Offense_Type)) +geom_bar()+ggtitle("Number of offences per type")+ ylab("number occurences")+ xlab("Beat Code")+facet_wrap(~day)
#shift 2
ggplot(shift2M, aes(x= shift2M$Offense_Type)) +geom_bar()+ggtitle("Number of offences per type")+ ylab("number occurences")+ xlab("Beat Code")+facet_wrap(~day)
#shift 3
ggplot(shift3M, aes(x= shift3M$Offense_Type)) +geom_bar()+ggtitle("Number of offences per type")+ ylab("number occurences")+ xlab("Beat Code")+facet_wrap(~day)



# Give information about the weekdays
group_by(policeDataM,day)%>%summarise(counts=sum(n_offenses))%>%arrange(desc(counts))


group_by(shift1M,day)%>%summarise(counts=sum(n_offenses))%>%arrange(desc(counts))

group_by(shift2M,day)%>%summarise(counts=sum(n_offenses))%>%arrange(desc(counts))

group_by(shift3M,day)%>%summarise(counts=sum(n_offenses))%>%arrange(desc(counts))


ggplot(policeDataM,aes(x=day,weights=n_offenses))+geom_bar() +  ggtitle('Histogram of offences by Week Day')

ggplot(shift1M,aes(x=day,weights=n_offenses))+geom_bar() +  ggtitle('Histogram of offences by Week Day')

ggplot(shift2M,aes(x=day,weights=n_offenses))+geom_bar() +  ggtitle('Histogram of offences by Week Day')

ggplot(shift3M,aes(x=day,weights=n_offenses))+geom_bar() +  ggtitle('Histogram of offences by Week Day')


## General information about the given data

#which days are represented
table(wday(policeData$Date,label=TRUE))


#which months are represented

table(month(policeData$Date,label=TRUE))

#which years are represented

table(year(policeData$Date))

## 3rd Part- Prediction

# Our main aim is, given a list with informations:
# Beat, day and hour, how many offenses will be occuring at this hour
# Therefore we have to create training data basing on this three aspects.
# Having made the predictions, we can use this information to give this information 
# about the shifts

policeData<-mutate(policeData,day=day(Date),wday=wday(Date,label=TRUE))

#Cutting the unuseful informations:

policeDataTest<-group_by(policeData, Beat, wday, Hour)%>%summarise(n_offenses=sum(n_offenses))

policeDataTest$shift<-NA
policeDataTest$shift[policeDataTest$Hour>=8 & policeDataTest$Hour <12]<-"shift1"
policeDataTest$shift[policeDataTest$Hour>=12 & policeDataTest$Hour <19]<-"shift2"
policeDataTest$shift[policeDataTest$Hour>=19 & policeDataTest$Hour <24]<-"shift3"
policeDataTest$shift[policeDataTest$Hour>=0 & policeDataTest$Hour <8]<-"shift3"


# Swapping the last two columns and cancel the information about the Hour
policeDataTest<-policeDataTest[c("Beat","wday", "shift", "n_offenses")]
policeDataTest$Beat<-factor(policeDataTest$Beat)
policeDataTest$shift<-factor(policeDataTest$shift)
policeDataTest<-group_by(policeDataTest, Beat, wday, shift)%>%summarise(n_offenses=sum(n_offenses))


#Setting the data for prediction and testing
splitVal <- 0.7

# Extract samples
set.seed(1234) # for reproduction
idx.tr <-sample(1:nrow(policeDataTest),as.integer(splitVal*nrow(policeDataTest)))
tr <- policeDataTest[idx.tr, ] #Split into two sets
ts <- policeDataTest[-idx.tr,]
#create prediction Table 

##Prediction Parts:

#Tree Model

model_AC <- rpartXse(n_offenses ~ .,tr)
predsTree <- predict(model_AC,ts)

# Naive Bayes


model_NB <- naiveBayes(n_offenses ~ ., tr)
predsNB <- predict(model_NB, ts)

#k neirest neighbors

model_KNN <-kNN(n_offenses ~ .,tr,ts,k=3,norm=TRUE)

#

model_SVM <- svm(n_offenses ~., tr, cost =10, epsilon=0.02)
preds <- predict(model_SVM,ts)


#

timeTablePolice<-matrix(nrow=116*7*3, ncol=4)
beats=unique(policeDataTest$Beat)
days=unique(policeDataTest$wday)
shift=unique(policeDataTest$shift)
predi<-ts[1,]
predicted=0
c=0

for(i in 1:2436){
  
  timeTablePolice[i,1]= as.character(beats[ceiling(i/21)])
  c=ceiling(i/3)%%7
  if(c==0){timeTablePolice[i,2]=as.character(days[7])}
  else{
    timeTablePolice[i,2]=as.character(days[c])#ceiling(i/7)%%7#as.character(days[ceiling(i%%7])}
  }
  if(i%%3==0){
    timeTablePolice[i,3]=as.character(shift[3])
  }else{
    timeTablePolice[i,3]=as.character(shift[i%%3])
  }
}


