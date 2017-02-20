source('~/Dropbox/FinalYearProject/PreprocessForPrediction.R', echo=TRUE)
indices=which(trainSet$Metabolic.Power.Average%in% sort(trainSet$Metabolic.Power.Average,decreasing=TRUE)[1:20])
trainSet[indices,'Drill']




#look for a global seasonal trend in new.bodyload
library(ggplot2)
library(scales)
seasonalData=aggregate(trainSet$New.Bodyload,by=list(trainSet$Date),FUN=mean)
seasonalData$Group.1=as.Date(seasonalData$Group.1,format='%d/%m/%Y')
ordering=order(seasonalData$Group.1)
seasonalData=seasonalData[ordering,]
names(seasonalData)=c('Date','New.Bodyload')
seasonStart=as.Date('16/09/2015',format='%d/%m/%Y')
seasonEnd=as.Date('28/04/2015',format='%d/%m/%Y')
preSeasonStart=as.Date("10/07/2015",format='%d/%m/%Y')
seasonPlot=ggplot(seasonalData,aes(x=Date,y=New.Bodyload))
seasonPlot+geom_point()+geom_line()+
  annotate('rect',xmin=seasonEnd, xmax=seasonStart,
           ymin=0, ymax=Inf,alpha=0.4,fill='white')+
  
  annotate('rect',xmin=min(seasonalData$Date), xmax=max(seasonalData$Date),
           ymin=0, ymax=Inf,alpha=0.15,fill='blue')+
  
  annotate('rect',xmin=preSeasonStart, xmax=seasonStart,
           ymin=0, ymax=Inf,alpha=0.15,fill='red')+ geom_smooth()+
  
  annotate('rect',xmin=seasonEnd, xmax=preSeasonStart,
           ymin=0, ymax=Inf,alpha=0.15,fill='green')+
  
  annotate("text", x = median(c(seasonEnd,preSeasonStart)), y = 150, label = "Training break",angle = 90,color='red',size=7)+
  annotate("text", x = median(seasonalData$Date), y = 150, label = "PreSeason 2016",angle = 90,color='red',size=7)+
  scale_x_date(breaks = seq.Date(min(seasonalData$Date), max(seasonalData$Date), by = 'month'),labels = date_format("%b"))

#Plot of training Frequency
seasonStart=as.Date('16/09/2015',format='%d/%m/%Y')
seasonEnd=as.Date('18/05/2015',format='%d/%m/%Y')
preSeasonStart=as.Date("10/07/2015",format='%d/%m/%Y')
preSeasonEnd=as.Date("4/09/2015",format='%d/%m/%Y')
ggplot(anonymisedData, aes(Date))+geom_density(alpha=0.1,fill='blue',adjust=0.25)+
  ggtitle('Plot of training frequency')+theme(plot.title = element_text(hjust = 0.5),axis.text.x = element_text(angle = 90, hjust = 1))+
  annotate('rect',xmin=preSeasonStart, xmax=seasonStart,
           ymin=0, ymax=Inf,alpha=0.15,fill='red')+
  
  annotate('rect',xmin=seasonEnd, xmax=preSeasonStart,
           ymin=0, ymax=Inf,alpha=0.15,fill='green')+
  scale_x_date(date_breaks = '4 weeks')+
  annotate("text", x = median(c(seasonEnd,preSeasonStart)), y = 0.005, label = "Training break",angle = 90,color='red',size=7)+
  annotate("text", x = median(c(preSeasonStart,preSeasonEnd)), y = 0.005, label = "PreSeason 2016",angle = 90,color='red',size=7)

makePlot=function(variable,Func=mean){
  seasonalData=aggregate(anonymisedData[,variable],by=list(anonymisedData$Date),FUN=Func)
  seasonalData$Group.1=as.Date(seasonalData$Group.1,format='%d/%m/%Y')
  ordering=order(seasonalData$Group.1)
  seasonalData=seasonalData[ordering,]
  names(seasonalData)=c('Date',variable)
  print(seasonalData)
  seasonStart=as.Date('16/09/2015',format='%d/%m/%Y')
  seasonEnd=as.Date('18/05/2015',format='%d/%m/%Y')
  preSeasonStart=as.Date("10/07/2015",format='%d/%m/%Y')
  seasonPlot=ggplot(seasonalData,aes(x=Date,y=seasonalData[,variable]))
  seasonPlot+geom_point()+geom_line()+geom_smooth(span = 0.3)+
    
    annotate('rect',xmin=seasonEnd, xmax=seasonStart,
             ymin=0, ymax=Inf,alpha=0.4,fill='white')+
    
    annotate('rect',xmin=min(seasonalData$Date), xmax=max(seasonalData$Date),
             ymin=0, ymax=Inf,alpha=0.15,fill='blue')+
    
    annotate('rect',xmin=preSeasonStart, xmax=seasonStart,
             ymin=0, ymax=Inf,alpha=0.15,fill='red')+ 
    
    annotate('rect',xmin=seasonEnd, xmax=preSeasonStart,
             ymin=0, ymax=Inf,alpha=0.15,fill='green')+ 

    annotate("text", x = median(c(seasonEnd,preSeasonStart)), y = (max(seasonalData[,variable])-min(seasonalData[,variable]))/2, label = "Training break",angle = 90,color='red',size=7)+
    annotate("text", x = median(seasonalData$Date), y = (max(seasonalData[,variable])-min(seasonalData[,variable]))/2, label = "PreSeason 2016",angle = 90,color='red',size=7)+
    scale_x_date(date_breaks = 'month')+    
    labs(title=paste('Plot of ',variable,' as a function of time'))+
    ylab(variable)+
    theme(plot.title = element_text(hjust = 0.5),axis.text.x = element_text(angle = 90, hjust = 1))
}
makePlot('Duration',sum)
makePlot('New.Bodyload',mean)
makePlot('New.Bodyload/Duration',mean)

trainSet$NBLDivDur=trainSet$New.Bodyload/trainSet$Duration
aggregatedDataAthleteLong=aggregate(trainSet,by=list(trainSet$Athlete),FUN=list)
aggregatedDataAthleteLong$Position=lapply(aggregatedDataAthleteLong$Position,function(x) names(sort(table(x),decreasing=TRUE))[1])
aggregatedDataAthleteLong$Position=unlist(aggregatedDataAthleteLong$Position)
aggregatedDataAthleteLong$Athlete=lapply(aggregatedDataAthleteLong$Athlete,function(x) names(sort(table(x),decreasing=TRUE))[1])
aggregatedDataAthleteLong$Athlete=unlist(aggregatedDataAthleteLong$Athlete)
aggregatedDataAthleteLong$Drill=lapply(aggregatedDataAthleteLong$Drill,function(x) names(sort(table(x),decreasing=TRUE))[2])
aggregatedDataAthleteLong$Drill=unlist(aggregatedDataAthleteLong$Drill)
aggregatedDataAthleteLong$New.Bodyload=lapply(aggregatedDataAthleteLong$New.Bodyload,mean)
aggregatedDataAthleteLong$New.Bodyload=unlist(aggregatedDataAthleteLong$New.Bodyload)
aggregatedDataAthleteLong$NBLDivDur=lapply(aggregatedDataAthleteLong$NBLDivDur,mean)
aggregatedDataAthleteLong$NBLDivDur=unlist(aggregatedDataAthleteLong$NBLDivDur)
unique(anonymisedData$Drill)
aggregatedDataAthleteLong[aggregatedDataAthleteLong$Position=='Centre Defender','NBLDivDur']

aggregatedDataDrillLong=aggregate(trainSet,by=list(trainSet$Drill),FUN=list)
aggregatedDataDrillLong$New.Bodyload=lapply(aggregatedDataDrillLong$New.Bodyload,mean)
aggregatedDataDrillLong$New.Bodyload=unlist(aggregatedDataDrillLong$New.Bodyload)




orderedPositions=c('Goalkeeper','Centre Back','Centre Defender','Left Back','Right Back','Right Midfield','Left Midfield','Centre Midfield','Forward')
#Dotplot of new.bodyload by position
ggplot(aggregatedDataAthleteLong,aes(x=factor(Position,orderedPositions,ordered = TRUE),y=New.Bodyload,fill=Position))+geom_smooth()+
  geom_dotplot(binaxis = "y",dotsize=0.5,stackdir='up')+ggtitle('Dotplot of New.Bodyload by Athlete Position')+
  theme(plot.title = element_text(hjust = 0.5),axis.text.x = element_text(angle = 90, hjust = 1))+xlab('')+
  guides(fill=FALSE)


#Scale by duration
ggplot(aggregatedDataAthleteLong,aes(x=factor(Position,orderedPositions,ordered = TRUE),y=NBLDivDur,fill=Position))+geom_smooth()+
  geom_dotplot(binaxis = "y",dotsize=0.5,stackdir='up')+ggtitle('Dotplot of New.Bodyload by Athlete Position')+
  theme(plot.title = element_text(hjust = 0.5),axis.text.x = element_text(angle = 90, hjust = 1))+xlab('')+ylab('Mean New.Bodyload/Duration')+
  guides(fill=FALSE)

#New.Bodyload frequency plot by position
par(mar=c(4.5,4.5,4.5,4.5))
ggplot(trainSet, mapping = aes(x = New.Bodyload, y = ..density..))+
  geom_freqpoly(mapping = aes(colour = factor(Position)), binwidth = 5)+
  ggtitle('New.Bodyload density by position')+
  scale_color_discrete(guide = guide_legend(title = NULL))+
  theme(plot.title = element_text(hjust = 0.5))
ggplot(trainSet, mapping = aes(x = New.Bodyload, y = ..density..))+
  geom_freqpoly(mapping = aes(colour = factor(Athlete)), binwidth = 5)+
  ggtitle('New.Bodyload density by position')+
  scale_color_discrete(guide = guide_legend(title = NULL))+
  theme(plot.title = element_text(hjust = 0.5))

#frequency of training on any given day - no real difference between days, more frequent earlier in week
barplot(table(weekdays(as.Date(trainSet$Date,format='%d/%m/%Y'))),las=2)






modela=lm('Metabolic.Power.Average~Drill',trainSet)
summary(modela)
#Find drills that record a high met.pow.avg
sort(modela$coefficients,decreasing = TRUE)[1:20]

trainSet$Metabolic.Power.Average
metPowRating=aggregate(trainSet[,c('Metabolic.Power.Average','Athlete')],by=list(trainSet$Athlete),FUN=mean)
metPowOrdering=order(metPowRating$Metabolic.Power.Average,decreasing=TRUE)
metPowRating=metPowRating[metPowOrdering,]
metPowRating$Position=unlist(lapply(metPowRating$Athlete,FUN = function(x) trainSet[trainSet$Athlete==x,'Position'][1]))



summary(dpois(1000, 7, 3))
?dpois
set.seed(1)
x=rpois(100, lambda = 15)
set.seed(1)
plot(density(rpois(100, lambda = 15)))
set.seed(1)
x=x/rnorm(100,1.6,0.2)
ind=function(x){
  if((x-9.8)>0.25*9.8){
    return (x-9.8)
  }
  else{
    return(0)
  }
}
par(mar=c(4,4,2,4))
y=unlist(lapply(x,ind))
plot(cumsum(y),cex=0.1,type='b',xlab='Time',ylab='Simulated Cumultive value of New.Bodyload',main='Simulated Generation of New.Bodyload value')
x
x[which(max(x)==x)]=x[which(max(x)==x)]*3
y=unlist(lapply(x,ind))
y
plot(cumsum(y),cex=0.1,type='b',xlab='Time',ylab='Simulated Cumultive value of New.Bodyload',main='Simulated Generation of New.Bodyload value')
mean(anonymisedData$New.Bodyload)

plot(density(unlist(lapply(y,function(a) a+a^3))))
plot(density(anonymisedData[anonymisedData$Athlete=='6',"New.Bodyload"]))
hist(unlist(lapply(y,function(a) a+a^3)),breaks=30)
hist(anonymisedData[anonymisedData$Athlete=='6',"New.Bodyload"],breaks=30)

plot(density(anonymisedData[anonymisedData$New.Bodyload<150,]$New.Bodyload))








#--------------------------------------------------------------------------------------------------------------#

TRUE%in%duplicated(anonymisedData)
naPresent=lapply(names(anonymisedData),function(x)TRUE%in%is.na(anonymisedData[,x]))
which(unlist(naPresent))

nonNumericCols=names(anonymisedData)[!names(anonymisedData)%in%numericCols]
missingVals=lapply(nonNumericCols,function(x)TRUE%in%(''%in%anonymisedData[,x]))
missingVals=nonNumericCols[which(unlist(missingVals))]
missingVals
countMissingVals=lapply(missingVals,function(x) sum(''==anonymisedData[,x]))
rbind(missingVals,round(unlist(countMissingVals)/nrow(anonymisedData)*100,3))

#training frequency by hour
ggplot(anonymisedData,aes(x=hour(ymd_hms(DrillTime))))+geom_bar()+
  theme(plot.title = element_text(hjust = 0.5))+
  ggtitle('Barplot of hourly drill times')+labs(x = "Hour of training")

ggplot(anonymisedData,aes(x=factor(weekdays(as.Date(anonymisedData$Date,format='%d/%m/%Y')),weekd,ordered = TRUE)))+geom_bar()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1),plot.title = element_text(hjust = 0.5))+
  ggtitle('Barplot of daily drill times')+labs(x = "Day of training time")

ggplot(anonymisedData, aes(x = factor(months(anonymisedData$Date), mo, ordered=TRUE))) + 
  geom_bar()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1),plot.title = element_text(hjust = 0.5))+
  ggtitle('Barplot of monthly drill times')+labs(x = "Month of training time")


anonymisedData$DrillTime=as.POSIXct(paste(hour(anonymisedData$Start.Time),minute(anonymisedData$Start.Time),'00',sep=':'),format='%H:%M:%S')
barplot(table(hour(round(anonymisedData$DrillTime,units='hours')))/sum(table(hour(round(anonymisedData$DrillTime,units='hours')))))

reorder(months(anonymisedData$Date),months(anonymisedData$Date),
        function(x) which(x%in%mo))
which('January'%in%mo)
factor(months(anonymisedData$Date), mo, ordered=TRUE)
mo=c('January',
  'February',
  'March',
  'April',
  'May',
  'June',
  'July',
  'August',
  'September',
  'October',
  'November',
  'December')
weekd=c('Monday','Tuesday','Wednesday','Thursday','Friday','Saturday','Sunday')

hour(ymd_hms(anonymisedData$DrillTime))

#How does Duration interact with New.Bodyload
par(mfrow=c(1,1))
plot(anonymisedData[anonymisedData$Outlierindicator==0,]$Duration,anonymisedData[anonymisedData$Outlierindicator==0,]$New.Bodyload,cex=0.1)
ggplot(anonymisedData[anonymisedData$Outlierindicator==0,],aes(Impacts.Zone.2, New.Bodyload))+
  geom_point(size=0.1)+
  geom_smooth()+
  ggtitle('New Bodyload values vs. Duration')+
  theme(plot.title = element_text(hjust = 0.5))

quintile <- function(var,p){ 
  cut(var, breaks= unique(quantile(var,probs=seq(0,1,by=p), na.rm=T)), include.lowest=T, ordered=T)
}
anonymisedData$cuts=quintile(anonymisedData$Duration, 0.05)
ggplot(anonymisedData[anonymisedData$Outlierindicator==0,],aes(cuts, New.Bodyload))+
  geom_boxplot()+
  xlab('Duration cuts')+ggtitle('New Bodyload values across cuts of Duration')+
  theme(plot.title = element_text(hjust = 0.5),axis.text.x = element_text(angle = 90, hjust = 1))
plot(anonymisedData$cuts,las=2)

anonymisedData$cuts=quintile(anonymisedData$Impacts.Zone.2, 0.05)
ggplot(anonymisedData[anonymisedData$Outlierindicator==0,],aes(cuts, New.Bodyload))+
  geom_boxplot()+
  xlab('Duration cuts')+ggtitle('New Bodyload values across cuts of Duration')+
  theme(plot.title = element_text(hjust = 0.5),axis.text.x = element_text(angle = 90, hjust = 1))
plot(anonymisedData$cuts,las=2)

plotGGBoxQuantile=function(variable){
  anonymisedData$cuts=quintile(anonymisedData[,variable], 0.05)
  ggplot(anonymisedData[anonymisedData$Outlierindicator==0,],aes(cuts, New.Bodyload))+
    geom_boxplot()+
    xlab('Duration cuts')+ggtitle('New Bodyload values across cuts of Duration')+
    theme(plot.title = element_text(hjust = 0.5),axis.text.x = element_text(angle = 90, hjust = 1))
}
plotGGScatter=function(variable1,variable2){
  ggplot(anonymisedData[anonymisedData$Outlierindicator==0,],aes(anonymisedData[anonymisedData$Outlierindicator==0,variable1], anonymisedData[anonymisedData$Outlierindicator==0,variable2]))+
    geom_point(size=0.1)+
    geom_smooth(span = 0.3)+
    ggtitle(paste(variable1,' values vs. ',variable2))+
    xlab(variable1)+
    ylab(variable2)+
    geom_hline(yintercept=120)+
    theme(plot.title = element_text(hjust = 0.5))
}
plotGGScatter('Collisions',"New.Bodyload")
plotGGScatter('Duration',"New.Bodyload")
plotGGScatter('Distance',"New.Bodyload")
plotGGScatter('DrillTime',"New.Bodyload")
plotGGScatter('Impacts.Zone.1',"New.Bodyload")
plotGGScatter('Impacts.Zone.2',"Impacts.Zone.1")
plotGGScatter('Impacts.Zone.3',"New.Bodyload")
plotGGScatter('Impacts.Zone.4',"New.Bodyload")
plotGGScatter('Impacts.Zone.5',"Collisions")

plotGGScatter('Max.Speed',"New.Bodyload")
plotGGScatter('Avg.Speed',"New.Bodyload")
plotGGScatter('Metabolic.Power.Average',"New.Bodyload")

#plotGGBoxQuantile('Duration')

library(corrplot)
correlations=cor(anonymisedData[,setdiff(numericCols,'TimeHM')])
sort(sapply(c(1:42), function(x) sum(correlations[x,])))
counter=1
for (i in sapply(c(1:42), function(x) sum(correlations[x,]))){
  print(paste('Correlation total of',setdiff(numericCols,'TimeHM')[counter],'is ',i))
  counter=counter+1
}
barplot()
class(correlations)




ggplot(trainSet, aes('New.Bodyload', y=New.Bodyload))+geom_boxplot(outlier.colour='red',outlier.size=0.5)+ggtitle('Boxplot of New.Bodyload values Train set')+theme(plot.title = element_text(hjust = 0.5))+stat_boxplot(geom ='errorbar')
ggplot(testSet, aes('New.Bodyload', y=New.Bodyload))+geom_boxplot(outlier.colour='red',outlier.size=0.5)+ggtitle('Boxplot of New.Bodyload values Test set')+theme(plot.title = element_text(hjust = 0.5))+stat_boxplot(geom ='errorbar')

#Plot to check whether high values of NBL occur close together
ggplot(data=trainSet,aes(Start.Time,New.Bodyload))+geom_point(size=0.3,aes(colour=linearModel$residuals))
#colour=aes(trainSet[which(linearModel$residuals>200),]))





