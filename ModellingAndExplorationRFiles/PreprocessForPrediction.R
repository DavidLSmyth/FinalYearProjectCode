library(rpart)
library(rattle)
library(rpart.plot)
library(glmnet)
library(leaps)
library(ggplot2)
library(rattle)
library(rpart.plot)
library(scales)
library(lubridate)
library(graphics)

set.seed(1)
anonymisedData=read.csv('/home/user15/Downloads/GPSData.csv')
str(anonymisedData)
#50 different athletes
table(anonymisedData$Athlete)
#  1   2   3   4   5   6   7   8   9  10  11  12  13  14  15  16  17  18  19  20  21  22  23  24  25  26  27  28  29  30  31  32  33  34  35  36  37  38  39  40  41  42 
#117 413 209 120 322 219   1  59 261  63  25 374   1 239  31 269 111  31 447  16  62  43 237  15  87  11 371  38 141 208 368  27 423  18 342 227 314  38  58 384  21  31 
# 43  44  45  46  47  48  49  50 
#307  55 413  15  16 237  93 100 

#--------------------------------Remove columns that are all 0 -------------------------------#

#Remove all of these
summary(anonymisedData$HR.Training.Effect)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#0       0       0       0       0       0 
summary(anonymisedData$Session.RPE)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#0       0       0       0       0       0 
summary(anonymisedData$Sprint.Max.Acceleration)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#0       0       0       0       0       0 

#Setting to NULL removes these columns from the data
anonymisedData$HR.Training.Effect=NULL
anonymisedData$Session.RPE=NULL
anonymisedData$Sprint.Max.Acceleration=NULL

sum(anonymisedData$Work.Rate.Interval.Count==anonymisedData$Work.Rate.Interval.Duration)
#two columns are exactly equal,just keep interval.count
anonymisedData$Work.Rate.Interval.Duration=NULL

#--------------------------------Remove columns that are all 0-------------------------------#


#Have found that zone variables are useful in predicting new.bodyload, keep them for now.
grep('Zone',names(anonymisedData))
names(anonymisedData)[grep('Zone',names(anonymisedData))]
#10 13 14 15 16 17 18 24 25 26 27 28 29 32 33 34 35 36 37 39 40 41 42 43 44 50 51 52 53 54 59 60 61 62 63
#dataMinusZone=anonymisedData[,-grep('Zone',names(anonymisedData))]
#dataMinusZone  8028 obs. of 34 variables

#Fix times and dates, don't want these to be factor
anonymisedData$Start.Time=dmy_hm(anonymisedData$Start.Time)
anonymisedData$Date=as.Date(anonymisedData$Date,format='%d/%m/%Y')
anonymisedData$Athlete=as.factor(anonymisedData$Athlete)

#----------------------------Clean up new.bodyload variable before predictions-------------------------------#

summary(anonymisedData$New.Bodyload)
#Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
#0.00    18.39    33.71    64.54    70.89 14170.00 
plot(anonymisedData$New.Bodyload,cex=0.1)
hist(as.Date(anonymisedData$Date,format='%d/%m/%Y'),breaks='years',col='grey',main='Training frequency by year')
hist(as.Date(anonymisedData$Date,format='%d/%m/%Y'),breaks='months',col='grey',main='Training frequency by month',las=2,xlab='')
par(mar=c(6,4,2,1))
#Plot training frequency by day
barplot(table(factor(weekdays(as.Date(anonymisedData$Date,format='%d/%m/%Y')),c('Monday','Tuesday','Wednesday','Thursday','Friday','Saturday','Sunday'),ordered=TRUE))/sum(table(weekdays(as.Date(anonymisedData$Date,format='%d/%m/%Y')))),col='grey',main='Training frequency by day',las=2,ylab='Density')
##Plot training frequency by day 
barplot(table(weekdays(as.Date(anonymisedData$Date,format='%d/%m/%Y'))),col='grey',main='Training frequency by month',las=2,xlab='')
#Plot training frequency by hour
anonymisedData$DrillTime=as.POSIXct(paste(hour(anonymisedData$Start.Time),minute(anonymisedData$Start.Time),'00',sep=':'),format='%H:%M:%S')
barplot(table(hour(anonymisedData$DrillTime)-4)/sum(table(hour(anonymisedData$DrillTime))-4),main='Training frequency by Hour',ylab='Density')



x=hist(as.Date(anonymisedData$Date,format='%d/%m/%Y'),breaks='weeks',col='grey',main='Training frequency by week',las=2)
x
plot(x$mids, x$counts,type='b',cex=0.2)
#Date density plot shows how often team trained throughout the year
#Include training density plot in report
ggplot(anonymisedData, aes(x = as.Date(anonymisedData$Date,format='%d/%m/%Y'))) + 
  geom_density()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  ggtitle('Density plot of training frequency')
#There is clearly a higher frequency of training on some dates than others

#-------------------Deak With High values first--------------#
#there are some obvious outliers
#plot histogram for a better sense of distribution of new.bodyload
x=hist(anonymisedData$New.Bodyload,breaks=30)
#$breaks
#[1]     0   500  1000  1500  2000  2500  3000  3500  4000  4500  5000  5500  6000  6500  7000  7500  8000  8500  9000  9500 10000 10500 11000 11500
#[25] 12000 12500 13000 13500 14000 14500
#$mids
#[1]   250   750  1250  1750  2250  2750  3250  3750  4250  4750  5250  5750  6250  6750  7250  7750  8250  8750  9250  9750 10250 10750 11250 11750
#[25] 12250 12750 13250 13750 14250
#$counts
#[1] 7991   26    4    4    1    1    0    0    0    0    0    0    0    0    0    0    0    0    0    0    0    0    0    0    0    0    0    0    1
# histogram data is conclusive that there are extreme values present
anonymisedData$Date=as.Date(anonymisedData$Date,format='%Y-%m-%d')
ggplot(anonymisedData, aes('New.Bodyload', y=New.Bodyload))+geom_boxplot()
ggplot(anonymisedData,aes('Duration',y=Duration))+geom_boxplot(color='Red')


max(anonymisedData$New.Bodyload)
mean(anonymisedData$New.Bodyload)
sd(anonymisedData$New.Bodyload)
#View(anonymisedData[which(anonymisedData$New.Bodyload==max(anonymisedData$New.Bodyload)),])
#remove top valued New.Bodyload to start with
anonymisedData=anonymisedData[-c(which(anonymisedData$New.Bodyload==max(anonymisedData$New.Bodyload))),]
x=hist(anonymisedData$New.Bodyload,breaks=30)
x
plot(density(anonymisedData$New.Bodyload))
#$breaks
#[1]    0  100  200  300  400  500  600  700  800  900 1000 1100 1200 1300 1400 1500 1600 1700 1800 1900 2000 2100 2200 2300 2400 2500 2600 2700 2800 2900 3000
#
#$counts
#[1] 6458 1150  308   61   14   14    8    0    1    3    1    2    0    0    1    1    2    0    1    0    0    0    0    1    0    0    0    0    0    1

#look again at box-and-whisker
ggplot(anonymisedData, aes('New.Bodyload', y=New.Bodyload))+stat_boxplot(geom ='errorbar')+geom_boxplot()
ggplot(anonymisedData, aes('New.Bodyload', y=New.Bodyload))+geom_boxplot(outlier.colour='red',outlier.size=0.5)+ggtitle('Boxplot of New.Bodyload values')+theme(plot.title = element_text(hjust = 0.5))+stat_boxplot(geom ='errorbar')
ggplot(anonymisedData, aes('New.Bodyload', y=New.Bodyload))+stat_boxplot(geom ='errorbar')
#-1.5 * IQR<=whisker<=1.5 * IQR
#Still appears to be outliers
#See if there is any easy way to predict whether the value is outside whisker region or not
anonymisedData$Outlierindicator=ifelse(anonymisedData$New.Bodyload>=200|anonymisedData$New.Bodyload<=1,1,0)
table(anonymisedData$Outlierindicator)/nrow(anonymisedData)
#        0         1 
#0.8887505 0.1112495 
#View(anonymisedData[anonymisedData$New.Bodyload>700,])

treeModel=rpart('Outlierindicator~.',anonymisedData[,setdiff(cbind(names(anonymisedData)[sapply(anonymisedData, is.numeric)],'Outlierindicator'),"New.Bodyload")],control = rpart.control(maxdepth=6,cp = 0.01))
summary(treeModel)
fancyRpartPlot(treeModel)	

#Impacts.Zone.2 seems to play an important role, look at distribution
plot(density(anonymisedData$Impacts.Zone.2),main='Density plot of Impacts.Zone.2')
plot(density(anonymisedData$New.Bodyload))
plot(density(anonymisedData$New.Bodyload/anonymisedData$Duration))
plot(density(anonymisedData[anonymisedData$New.Bodyload<250,'New.Bodyload']),main='New.Bodyload<250',xlab='New.Bodyload Values')


summary(anonymisedData$Impacts.Zone.2)
sd(anonymisedData$Impacts.Zone.2)
qplot(anonymisedData$Impacts.Zone.2)
ggplot(anonymisedData, aes('Zone2', y=anonymisedData$Impacts.Zone.2))+stat_boxplot(geom ='errorbar')+geom_boxplot()
#How does impacts.zone.1 interact with impacts.zone.2
plot(anonymisedData$Impacts.Zone.1,anonymisedData$Impacts.Zone.2,cex=0.1)

#Does New.Bodyload vary much by position?
ggplot(data = anonymisedData, mapping = aes(x = New.Bodyload, y = ..density..))+
  geom_freqpoly(mapping = aes(colour = as.factor(anonymisedData$Position)), binwidth = 10)+
  scale_color_discrete(guide = guide_legend(title = NULL))

par(mar=c(4.5,4.5,4.5,4.5))
ggplot(data = anonymisedData[anonymisedData$New.Bodyload<500,], mapping = aes(x = New.Bodyload, y = ..density..))+
  geom_freqpoly(mapping = aes(colour = as.factor(anonymisedData[anonymisedData$New.Bodyload<500,'Position'])), binwidth = 10)+
  ggtitle('New.Bodyload density by position')+
  scale_color_discrete(guide = guide_legend(title = NULL))
  

ggplot(anonymisedData, aes(x = as.Date(anonymisedData$Date,format='%d/%m/%Y'),y=anonymisedData$New.Bodyload)) + 
  geom_point(size=0.2)+geom_smooth()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  ggtitle('Density plot of training frequency')


ggplot(anonymisedData[anonymisedData$New.Bodyload<400,], aes(x = as.Date(anonymisedData[anonymisedData$New.Bodyload<400,'Date'],format='%d/%m/%Y'),y=anonymisedData[anonymisedData$New.Bodyload<400,'New.Bodyload'])) + 
  geom_point(size=0.2)+geom_smooth()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  ggtitle('Density plot of training frequency')

#-------------------Deal With High values first--------------#


#Impacts plot function - plots how many impacts person had in any give zone
makePlot=function(variable,player,aggFunction=mean){
  seasonalData=aggregate(anonymisedData[anonymisedData$Athlete==player,variable],by=list(anonymisedData[anonymisedData$Athlete==player,'Date']),FUN=aggFunction)
  seasonalDataEveryone=aggregate(anonymisedData[,variable],by=list(anonymisedData[,'Date']),FUN=aggFunction)
  
  seasonalData$Group.1=as.Date(seasonalData$Group.1,format='%d/%m/%Y')
  seasonalDataEveryone$Group.1=as.Date(seasonalDataEveryone$Group.1,format='%d/%m/%Y')
  
  ordering=order(seasonalData$Group.1)
  everyoneOrdering=order(seasonalDataEveryone$Group.1)
  
  seasonalData=seasonalData[ordering,]
  seasonalDataEveryone=seasonalDataEveryone[everyoneOrdering,]
  
  names(seasonalData)=c('Date','variable')
  names(seasonalDataEveryone)=c('Date','variable')
  
  seasonStart=as.Date('16/09/2015',format='%d/%m/%Y')
  seasonEnd=as.Date('28/04/2015',format='%d/%m/%Y')
  preSeasonStart=as.Date("10/07/2015",format='%d/%m/%Y')
  seasonPlot=ggplot(seasonalData,aes(x=Date,y=variable))
  seasonPlot+geom_line(aes(color='Player'))+
    geom_line(data = seasonalDataEveryone, aes(x = Date, y = variable, color = "Everyone"),alpha=0.4) +
    
    annotate('rect',xmin=seasonEnd, xmax=seasonStart,
             ymin=0, ymax=Inf,alpha=0.4,fill='white')+
    
    annotate('rect',xmin=min(seasonalData$Date), xmax=max(seasonalData$Date),
             ymin=0, ymax=Inf,alpha=0.15,fill='blue')+
    
    annotate('rect',xmin=preSeasonStart, xmax=seasonStart,
             ymin=0, ymax=Inf,alpha=0.15,fill='red')+ geom_smooth()+
    
    annotate('rect',xmin=seasonEnd, xmax=preSeasonStart,
             ymin=0, ymax=Inf,alpha=0.15,fill='green')+
    
    scale_colour_manual("", values = c("Everyone"='darkgreen','Player'='red'))+
    annotate("text", x = median(c(seasonEnd,preSeasonStart)), y = mean(seasonalDataEveryone$variable), label = "Training break",angle = 90,color='red',size=6)+
    annotate("text", x = preSeasonStart+days(30), y = mean(seasonalDataEveryone$variable), label = "PreSeason 2016",angle = 90,color='red',size=6)+
    scale_x_date(breaks = seq.Date(min(seasonalData$Date), max(seasonalData$Date), by = 'month'),labels = date_format("%b"))
}
makePlot('New.Bodyload','10',sum)

#-------------------------------Deal with very low values----------------------------#
#According to GPSports website, new.bodyload starts at 0 and then accumulates once acceleration exceeds 0.25g or so.
#Therefore if it finishes up 0 then the player has experienced no high intensity accelerations. Need to check if zeros in 
#the data make sense as website says that there should be many low intensity impacts in any drill.
#View(anonymisedData[anonymisedData$New.Bodyload<=1,])

#website reports that accelerations of 2-6g pretty common values for low intensity impacts, therefore
#getting very low values of new.bodyload seems unlikely
plot(density(anonymisedData[anonymisedData$New.Bodyload<20,'New.Bodyload']))
par(mar=c(3,3,3,3))
par(mai=c(1.02,0.82,0.82,0.42))
hist(anonymisedData[anonymisedData$New.Bodyload<20,'New.Bodyload'],breaks=20, main='New.Bodyload<20',xlab="New.Bodyload Bin Value")
#no values less than zero but definitely a funny looking spike at 0-1, seems like another possible miscalibration
summary(anonymisedData[anonymisedData$New.Bodyload<20,'New.Bodyload'])
#View(anonymisedData[anonymisedData$New.Bodyload<1,])
#All of these have zero collisions
summary(anonymisedData$Collisions)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#0.000   2.000   5.000   8.678  12.000  88.000 
#View(anonymisedData[anonymisedData$New.Bodyload>1&anonymisedData$New.Bodyload<2,])
#All have waay more impacts. 

#Mabye all low values occur around the same dates.

length(unique(anonymisedData[anonymisedData$New.Bodyload<1,'Date']))
length(unique(anonymisedData[anonymisedData$New.Bodyload<1,'Athlete']))
#No dependence on date for zero values. 
#would seem like zero values are outliers


#-------------------------------Deal with very low values----------------------------#
#Outliers have been flagged and outlier vs. non-outlier can be predicted separately.

#---------------------Physically Duration and Distance cannot be very low for any drill---------------#
#View(anonymisedData[anonymisedData$Distance<=10,])
#Low new.bodyload true for almost all of these, indicates that if distance appears in model, it can predict
#low values of new.bodyload well
#View(anonymisedData[anonymisedData$Duration<=1,])
#few of these and they all look like valid records

#----------------create a culumative impacts variable and a cumulative acc. variable------------------#
#Since these columns area  linear combination of others, cannot be included in regresssion
#anonymisedData$CumulativeImpacts=anonymisedData$Impacts.Zone.1+
#                                       anonymisedData$Impacts.Zone.2+
#                                       anonymisedData$Impacts.Zone.3+
#                                       anonymisedData$Impacts.Zone.4+
#                                       anonymisedData$Impacts.Zone.5+
#                                       anonymisedData$Impacts.Zone.6
#anonymisedData$Accelerations=anonymisedData$Accelerations.Zone.1+
#                                        anonymisedData$Accelerations.Zone.2+
#                                        anonymisedData$Accelerations.Zone.3
#anonymisedData$Deccelerations=anonymisedData$Decelerations.Zone.1+
#                                        anonymisedData$Decelerations.Zone.2+
#                                        anonymisedData$Decelerations.Zone.3

#summary(anonymisedData$CumulativeImpacts)
#summary(anonymisedData$Accelerations)
#summary(anonymisedData$Deccelerations)

#----------------create a culumative impacts variable and a cumulative acc. variable------------------#
#-------------------Remove distance zone variables since a linear combination of them gives distance (Simplifies analysis)---------#
#Removing both Distance.Zone. and Metabolic.Load.Distance.Zone.
names(anonymisedData)[grep('Distance.Zone',names(anonymisedData))]
anonymisedData=anonymisedData[,-grep('Distance.Zone',names(anonymisedData))]

#set aside numeric columns
numericCols=names(anonymisedData)[sapply(anonymisedData, is.numeric)]
#keep athlete as numeric for the moment, can show that some variables are 'individual'

#----------------------------Clean up new.bodyload variable before predictions-------------------------------#

#-----------------------------Look for duration outliers-------------------------------#

ggplot(anonymisedData, aes('Duration', y=Duration))+stat_boxplot(geom ='errorbar')+geom_boxplot()
ggplot(anonymisedData, aes('Duration', y=Duration))+geom_boxplot(outlier.colour='red',outlier.size=0.5)+ggtitle('Boxplot of Duration values')+theme(plot.title = element_text(hjust = 0.5))+stat_boxplot(geom ='errorbar')
ggplot(anonymisedData, aes('Duration', y=Duration))+stat_boxplot(geom ='errorbar')

DurOutliers=sort(anonymisedData$Duration,decreasing = TRUE)[1:4]
View(anonymisedData[which(anonymisedData$Duration%in%DurOutliers),])
anonymisedData=anonymisedData[-which(anonymisedData$Duration%in%DurOutliers),]

#-----------------------------Look for duration outliers-------------------------------#

#---------------------------------Remove redundant columns that are a constant multiplied by another varaible---------------------#
plot(anonymisedData$Athlete,(anonymisedData$Equivalent.Distance/anonymisedData$Metabolic.Load..relative.),type='p',cex=0.1)
plot(anonymisedData$Equivalent.Distance,anonymisedData$Metabolic.Load..relative.*215,type='p',cex=0.1)
#Equivalent.Distance=Metabolic.Load..relative.*215


#add the relative variable metabolicLoad and remove metabolic load relative- reason is that the relative variable is correlated
#with same variables as absolute so is redundant in the model
#Metabolic load relative can be recovered by PlayerWeight*Metabolic.Load..absolute.
anonymisedData$PlayerWeight=anonymisedData$Metabolic.Load..absolute./anonymisedData$Metabolic.Load..relative.
anonymisedData[is.na(anonymisedData$PlayerWeight)|!is.finite(anonymisedData$PlayerWeight),'PlayerWeight']=mean(anonymisedData[!is.na(anonymisedData$PlayerWeight)&is.finite(anonymisedData$PlayerWeight),'PlayerWeight'])
summary(anonymisedData$PlayerWeight)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#49.00   65.14   70.88   73.28   80.48  133.00 
anonymisedData$Metabolic.Load..relative.=NULL

#this variable is related to distance also
plot(anonymisedData$Metabolic.Load..absolute.,anonymisedData$Distance*anonymisedData$PlayerWeight,type='p',cex=0.1)

#Adjust numeric cols accordingly
numericCols=setdiff(numericCols,c('Metabolic.Load..relative.'))
numericCols=append(numericCols,'PlayerWeight')

#---------------------------------Remove redundant columns that are a constant multiplied by another varaible---------------------#




#----------------------Remove variables that are very highly correlated with other variables------------------#
treeModel=rpart('Equivalent.Distance~.',anonymisedData,control = rpart.control(cp = 0.01),method = "anova")
summary(treeModel)
fancyRpartPlot(treeModel)	
forwardSelection=regsubsets(Equivalent.Distance~.,data = anonymisedData[,numericCols], nvmax = 4,method='forward')
x=summary(forwardSelection)
x$which
#R^2 very high means that equivalent distance is a linear combination of these and so doesn't need to be included
modela=lm('Equivalent.Distance~Distance',anonymisedData[,numericCols])
summary(modela)

anonymisedData$Equivalent.Distance=NULL
numericCols=setdiff(numericCols,c('Equivalent.Distance'))


plot(anonymisedData$Max.HR,anonymisedData$Avg.HR,type='p',cex=0.1)
forwardSelection=regsubsets(Max.HR~.,data = anonymisedData[,numericCols], nvmax = 4,method='forward')
x=summary(forwardSelection)
x$which
modela=lm('Max.HR~Avg.HR',anonymisedData[,numericCols])
summary(modela)
#R^2 very high means that equivalent distance is a linear combination of these and so doesn't need to be included
anonymisedData$Max.HR=NULL
numericCols=setdiff(numericCols,c('Max.HR'))

plot(anonymisedData$Metabolic.Power.Average,anonymisedData$Avg.Speed,type='p',cex=0.1)
forwardSelection=regsubsets(Metabolic.Power.Average~.,data = anonymisedData[,numericCols], nvmax = 4,method='forward')
x=summary(forwardSelection)
x$which
modela=lm('Metabolic.Power.Average~Avg.Speed',anonymisedData[,numericCols])
summary(modela)
#Keep this for now to compare players
#anonymisedData$Metabolic.Power.Average=NULL
#numericCols=setdiff(numericCols,c('Metabolic.Power.Average'))

plot(anonymisedData$Sprint.Count,anonymisedData$Repeat.Sprints,type='p',cex=0.1)
forwardSelection=regsubsets(Sprint.Count~.,data = anonymisedData[,numericCols], nvmax = 4,method='forward')
x=summary(forwardSelection)
x$which
modela=lm('Sprint.Count~Repeat.Sprints',anonymisedData[,numericCols])
summary(modela)
anonymisedData$Sprint.Count=NULL
numericCols=setdiff(numericCols,c('Sprint.Count'))


plot(anonymisedData$Work.Rate.Interval.Count,anonymisedData$Work.Rate.Interval.Distance,type='p',cex=0.1)
forwardSelection=regsubsets(Work.Rate.Interval.Count~.,data = anonymisedData[,numericCols], nvmax = 4,method='forward')
x=summary(forwardSelection)
x$which
modela=lm('Work.Rate.Interval.Count~Work.Rate.Interval.Distance',anonymisedData[,numericCols])
summary(modela)
anonymisedData$Work.Rate.Interval.Count=NULL
numericCols=setdiff(numericCols,c('Work.Rate.Interval.Count'))

plot(anonymisedData$Metabolic.Load..absolute.,anonymisedData$Distance*anonymisedData$PlayerWeight,type='p',cex=0.1)
forwardSelection=regsubsets(Metabolic.Load..absolute.~.,data = anonymisedData[,numericCols], nvmax = 4,method='forward')
x=summary(forwardSelection)
x$which
modela=lm('Metabolic.Load..absolute.~Distance',anonymisedData[,numericCols])
summary(modela)
anonymisedData$Metabolic.Load..absolute.=NULL
numericCols=setdiff(numericCols,c('Metabolic.Load..absolute.'))

#----------------------Remove variables that are very highly correlated with other variables------------------#

#------------------------------Split into train set and test set--------------------------------------#

#Split data into train and test sets for prediction
#Make the assumption that all datapoints are independent
set.seed(1)
#80%-20% split train-test
trainSetIndices=sample(c(1:nrow(anonymisedData)),0.8*nrow(anonymisedData),replace=FALSE)
length(trainSetIndices)
#6340
trainSet=anonymisedData[trainSetIndices,]
testSet=anonymisedData[!c(1:nrow(anonymisedData))%in%trainSetIndices,]
sum(duplicated(rbind(trainSet,testSet)))
#0 dupliacted, data set successfully partitioned into train and test

#------------------------------Split into train set and test set--------------------------------------#

#---------------------------Look into creating a hours&minutes variable---------------------------#

#Al Ain are 4 hours ahead (so minus 4 hours)
trainSet$TimeHM=unlist(lapply(trainSet$Start.Time,function(x) as.POSIXct(substr(x,12,16), format="%H:%M")))
testSet$TimeHM=unlist(lapply(testSet$Start.Time,function(x) as.POSIXct(substr(x,12,16), format="%H:%M")))

#ggplot(trainSet,aes(trainSet$TimeHM,trainSet$New.Bodyload))+geom_point(size=0.2)+geom_smooth(method='loess',span=0.8)
#plot(density(trainSet$TimeHM),cex=0.1)
#summary(as.POSIXct(trainSet$TimeHM,origin="1960-10-01"))


####################################################################################################
#######################################Accumulated Variables########################################
####################################################################################################

#Acc variables, i.e. variables that can only increase with time.
#Distance?, Distance.Zone, Max.speed, Time.in.HR.Zone, Sprint.Count, Accelerations.Zone, Decelerations.Zone, 
#New.Bodyload, Impacts. Zone, Collisions, Metabolic.Load.Time.Zone, Sprint.Total.Distance

#Not
#pairs(trainSet[,numericCols])














