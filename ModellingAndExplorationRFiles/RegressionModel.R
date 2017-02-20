#Ideally want to have a model that strongly favours interpretability over predicive power. Predictive power doesn't really give any new info
#because new.bodyload can be easily recorded. 
#So with this in mind, following techniques need to be explored: 

#1).Dimension Reduction Techinques - There are 63 variables in the data frame that seem to be worth looking at (preprocessing file has taken care of 
#a few varaibles from simple graphs and models)
#PCA should help reduve dimensionality. y
#Look into sufficiency. 
#Variable creation would also be a nice thing to explore.
#No experience in clustering but if I have time could be worthwhile looking at.
#Factor analysis seems suitable for this dataset but it would take a while to understand the maths

#2).A model that is interpretable. This rules out the likes of random forests etc. Ideally will look at: 
#Linear models - start with simple least squares and move into polynomial least squares with interaction terms, Ridge, Lasso, Elastic Net y
#Principal components regression sounds promising y
#Partial least squares could also be worth looking at y
#KNN - sometimes the simlest models perform the best!
#Decision trees - allows for non-linear effects and is very interpretable but prone to overfit.
#Ideally this model would be some kind of lm-if the model was a global one, then it would suggest that new.bodyload is somewhat independent
#of the individual player, the time of day etc.

#3).Evaluation metrics. How will the model be evaluated? Ideally the model will be able to predict on unseen data, but how well?
#MSE 
#RMSE
#Mean Absolute Error  sum(abs(y-y_pred)) / length(y)
#Root Mean Squared Logarithmic Error ϵ= sqrt(1/n ∑(log(p_i+1)−log(a_i+1))2)  - p_i predicted, a_i actual 
#Note RMSLE penalizes an under-predicted estimate greater than an over-predicted estimate

#4).Data currently might be too granular - could take groupings and then look at logistic regression to predict 
#what group a data point belongs to (e.g. 0-50, 50-100, 100-200, 200+). Then train a separate regression model on each group

#5).There is an obvious dependency on time - try and consider building a model that somehow takes time component into account.
#Here there could be a lot to explore - time of day could effect (e.g. might train much harder in the morning), players might have unique bodyloads
#and it might not make any sense to have a global model for new.bodyload

#NOTES
#Does it make sense to predict new.bodyload by itself? Perhaps it would make more sense to predict a transform of this variable.
#If it's not already time dependent, then scaling it by time could be beneficial. Taking a log transform could also yield good results.

#some variables seem to intuitively make sense to use in the model - 
#Drill-if bodyload is some indicator of how hard someone has trained, then it should be highly correlated with high intensity drills
#Athlete-it makes sense that playing in some positions is more demanding than playing in other positions

#On gpssports website, they state the following: Due to individual movement characteristics, it is recommended New Body Load (and other
#accelerometer outputs) be used for individual, rather than group analysis.
#It makes more sense to try and interpret new.bodyload for individuals, which suggests that this will be a hard problem to solve.
#At the end go back and see if can build individual models based off the global model

#Need to run analysis twice, once with outliers and once without outliers, check if results make sense

library(leaps)
library(bnlearn)
library(glmnet)
library(ggplot2)
library(rpart)
library(rattle)
library(rpart.plot)
library(lubridate)
library(splines)
library(gam)
library(gbm)
library(cplm)
library(segmented)
library(perturb)
library(MASS)
library(gvlma)
library(car)
library(caret)
library(FNN)

#Functions which are useful for getting a quick sense of how a variable interacts with new Bodyload
#Quintile used to generate boxQuantile, partitions variable into chunks of size p
quintile <- function(var,p){ 
  cut(var, breaks= unique(quantile(var,probs=seq(0,1,by=p), na.rm=T)), include.lowest=T, ordered=T)
}
#plotGGBoxQuantile partitions the variable into groups showing less granular changes
plotGGBoxQuantile=function(variable1,variable2,data){
  data[,'cuts']=quintile(data[,variable1], 0.05)
  print(ggplot(data,aes(cuts, New.Bodyload))+
    geom_boxplot()+
    xlab('Duration cuts')+ggtitle('New Bodyload values across cuts of Duration')+
    theme(plot.title = element_text(hjust = 0.5),axis.text.x = element_text(angle = 90, hjust = 1)))
  data[,'cuts']=NULL
}

smooth.spline2 <- function(formula, data, ...) {
  mat <- model.frame(formula, data)
  smooth.spline(mat[, 2], mat[, 1],spar=0.88)
}

predictdf.smooth.spline <- function(model, xseq, se, level) {
  pred <- predict(model, xseq)
  data.frame(x = xseq, y = pred$y)
}
#https://groups.google.com/forum/#!topic/ggplot2/FJ36CJH-ODo
#plotGGScatter is a scatterPlot with a smoothing spline fit to the data
plotGGScatter=function(variable1,variable2,data){
  ggplot(data,aes(data[,variable1], data[,variable2]))+
    geom_point(size=0.1)+
    geom_smooth(method="smooth.spline2",se=T)+
    geom_smooth(method = "lm", formula = y ~ x, se = TRUE,colour='red')+
    #geom_line(data=data.frame(spline(data[,variable1],data[,variable1])))+
    ggtitle(paste(variable1,' values vs. ',variable2))+
    xlab(variable1)+
    ylab(variable2)+
    geom_hline(yintercept=120)+
    theme(plot.title = element_text(hjust = 0.5))
}

plotGGScatterCluster=function(variable1,variable2,data){
  ggplot(data,aes(data[,variable1], data[,variable2]))+
    geom_point(aes(colour=data[,'clusters']),size=0.1)+
    geom_smooth(method="smooth.spline2",se=T)+
    geom_smooth(method = "lm", formula = y ~ x, se = TRUE,colour='red')+
    #geom_line(data=data.frame(spline(data[,variable1],data[,variable1])))+
    ggtitle(paste(variable1,' values vs. ',variable2))+
    xlab(variable1)+
    ylab(variable2)+
    theme(plot.title = element_text(hjust = 0.5))
}

#runDiagnostics shows the frequency of predictions as histograms and a plot of residuals
runDiagnostics=function(model,data){
  par(mfrow=c(1,2))
  upper=max(predict(model,data))+5
  lower=min(predict(model,data))-5
  h=hist(data[,'New.Bodyload'],breaks=seq(lower,upper,10),col=rgb(1,0,0,0.8),xlab='',main='')
  h1=hist(predict(model,data),breaks=seq(lower,upper,10),col=rgb(1,0,0,0.8),xlab='',main='')
  lim=c(0,max(max(h1$counts),max(h$counts)))
  hist(data[,'New.Bodyload'],breaks=seq(lower,upper,10),col=rgb(0,0,1,0.8),xlab='',main='',ylim=lim)
  par(new=TRUE)
  hist(predict(model,data),breaks=seq(lower,upper,10),col=rgb(1,0,0,0.3),ylim=lim,xlab='Bin',main='Histogram of Actual vs. Predicted values of New.Bodyload')
  legend('topright',legend=c("Predicted NBL", "Actual NBL"),col=c("red", "blue"),cex=0.8,pch=0:0)
  plot(density(predict(model,data)),main='Density of Actual/Predicted values of New.Bodyload',col='blue')
  lines(density(data[,'New.Bodyload']),col='red')
  legend('topright',legend=c("Predicted NBL", "Actual NBL"),col=c("red", "blue"),cex=0.8,lty=1:1)
  par(mfrow=c(1,1))
  if(class(model)[1]!='gam' & class(model)[1]!='mvr'){
  qqnorm(rstandard(model), 
         ylab="Standardized Residuals", 
         xlab="Normal Scores", 
         main="New.Bodyload Normal Quantile Plot",cex=0.4) 
  qqline(rstandard(model))  
  }
  print(ggplot(data,aes(New.Bodyload, resid(model)))+geom_point(size=0.1)+geom_smooth(method="smooth.spline2",se=T)+ggtitle('Residual Plot of Model')+theme(plot.title = element_text(hjust = 0.5))+geom_hline(yintercept = 0,colour='red',linetype="dashed"))
  print(ggplot(data,aes(New.Bodyload,predict(model)))+geom_point(size=0.1)+geom_smooth(method="smooth.spline2",se=T)+ggtitle('Plot of actual vs. predicted values')+xlab('Actual New.Bodyload')+ylab('Predicted New.Bodyload')+geom_abline(intercept = 0,slope = 1,colour='red',linetype="dashed")+theme(plot.title = element_text(hjust = 0.5)))
}

#For models other than those found with lm, Effrons r square can be used to approximate fit of model
effronRSquare=function(model, data){
  return( 1 - (sum((data[,'New.Bodyload']-predict(model,data))^2)/sum((data[,'New.Bodyload']-mean(data[,'New.Bodyload']))^2)))
}

adjustedRSquare=function(model,data,noPredictors){
  return(1-(((1-effronRSquare(model,data))*(nrow(data)-1))/(nrow(data)-noPredictors-1)))
}

plotPredictedUnivariable=function(model,data,variable){
  data[,'predicted']=predict(model,data)
  print(ggplot(data, aes(data[,variable],data[,'New.Bodyload']))+geom_point(size=0.1)+geom_smooth(method="smooth.spline2",se=T)+ggtitle(paste('Actual (blue) vs. Predicted (red) Interpolated values of',variable,'vs New.Bodyload'))+theme(plot.title = element_text(hjust = 0.5))+geom_smooth(data=data,aes(data[,variable],data[,'predicted']),colour='red',linetype='dashed'))
  data$predicted=NULL
}
MSE = function(model){
  mean(model$residuals^2)
}
TestMSE = function(model){
  mean((predict(model,testSet)-testSet$New.Bodyload)^2)
}

#CAR package can be used to check linear model assumptions, this code was taken from site below
diagnoseLM=function(fit){
  #http://www.statmethods.net/stats/rdiagnostics.html
  print('--------Outlier Test---------')
  outlierTest(fit) # Bonferonni p-value for most extreme obs
  print('--------Outlier Test---------')
  qqPlot(fit, main="QQ Plot") #qq plot for studentized resid 
  leveragePlots(fit)
  
  print('---Influential observations---')
  # Influential Observations
  # added variable plots 
  av.Plots(fit)
  # Cook's D plot
  # identify D values > 4/(n-k-1) 
  cutoff <- 4/((nrow(mtcars)-length(fit$coefficients)-2)) 
  plot(fit, which=4, cook.levels=cutoff)
  # Influence Plot 
  influencePlot(fit,	id.method="identify", main="Influence Plot", sub="Circle size is proportial to Cook's Distance" )
  
  print('---Residual Normality---')
  # Normality of Residuals
  # qq plot for studentized resid
  qqPlot(fit, main="QQ Plot")
  # distribution of studentized residuals
  sresid <- studres(fit) 
  hist(sresid, freq=FALSE, 
       main="Distribution of Studentized Residuals")
  xfit<-seq(min(sresid),max(sresid),length=40) 
  yfit<-dnorm(xfit) 
  lines(xfit, yfit)
  
  print('---Non-constant Error Variance---')
  # Evaluate homoscedasticity
  # non-constant error variance test
  ncvTest(fit)
  # plot studentized residuals vs. fitted values 
  spreadLevelPlot(fit)
  
  print('--- Multi-collinearity---')
  # Evaluate Collinearity
  vif(fit) # variance inflation factors 
  sqrt(vif(fit)) > 2 # problem?
  
  print('---NonLinearity---')
  # Evaluate Nonlinearity
  # component + residual plot 
  crPlots(fit)
  # Ceres plots 
  ceresPlots(fit)
  
  print('---Non-independence of Errors---')
  # Test for Autocorrelated Errors
  durbinWatsonTest(fit)
  
  print('---Additional Diagnostics---')
  # Global test of model assumptions
  gvmodel <- gvlma(fit) 
  summary(gvmodel)
}



#For reproducible results
set.seed(1)
#source the preprocessing file to load in data
source('~/Dropbox/FinalYearProject/ImportantModellingAndExplorationRFiles/PreprocessForPrediction.R', echo=FALSE)
#trainSet 6418
#testSet 1605

#how many suspected outliers are there in train and test
sum(trainSet$Outlierindicator==1)/nrow(trainSet)*100
#6.793394
sum(testSet$Outlierindicator==1)/nrow(testSet)*100
#6.542056

#Get rid of outliers for first analysis
trainSet=trainSet[trainSet$Outlierindicator==0,]
testSet=testSet[testSet$Outlierindicator==0,]

#Order train set by date in order to detect possible seasonal trends in residuals
trainSet=trainSet[order(trainSet$Date),]
testSet=testSet[order(testSet$Date),]

#Look for seasonal trends
ggplot(trainSet[order(as.Date(trainSet$Date,format='%d/%m/%Y')),],aes(c(1:nrow(trainSet)) ,trainSet$New.Bodyload))+geom_point(size=0.1)+geom_smooth()
nrow(trainSet[trainSet$New.Bodyload>=100,])/nrow(trainSet)*100
#~85% of New.Bodyload values under 100, (not including outliers)

#look at histogram to see how variable is distributed
x=hist(trainSet$New.Bodyload,breaks=30)
#Distribution looks like a variation of poisson, or perhaps truncated normal
#There's a lot of granularity in new.bodyload, it might be more consistent to try and first predict what bracket new.bodyload falls into.
#One the bracket has been predicted, then predict the exact value. This can be done piecewise or using a classifier and then regressor

#Look at finer grained plot to see if there are clearer brackets
plot(density(trainSet$New.Bodyload),main='Density plot of New.Bodyload')
#Density is quite smooth up to 100, no clear groupings

#It might be easier to predict some transformation of new.bodyload. This could help cure case of heteroscedistaticity Come back to this later.
plot(density(log(trainSet$New.Bodyload)),main='Density plot of New.Bodyload')

#negatives due to values less than 1
trainSet[which(log(trainSet$New.Bodyload)<1),'New.Bodyload']

#Look at plot of New.Bodyload/Duration, this variable could be identically distributed for all observations
plot(density(trainSet$New.Bodyload/trainSet$Duration),main='Density plot of New.Bodyload/Duration')


#Create new new.bodyload/duration variable that might be better to use for predictions
#Gpsports website states that new.bodyload/duration is a useful variable to look at.
#New Body Load Per Minute
#Arguably, the most useful variable from the NBL analysis. Apply this variable to
#drill split data to observe a relationship with work intensity.
#trainSet$New.Bodyload.Duration=trainSet$New.Bodyload/trainSet$Duration
#testSet$New.Bodyload.Duration=testSet$New.Bodyload/testSet$Duration

#Set aside numeric and non-numeric columns
numericCols=names(trainSet)[sapply(trainSet, is.numeric)]
nonNumericCols=names(trainSet)[!sapply(trainSet, is.numeric)]
#Outlier Indicator cannot be used as a predictor!
numericCols=setdiff(numericCols,'Outlierindicator')

#trainSet 5982
#testSet 1500
#---------------------------Look at univariate plots to give an idea of how everything is related to new.bodyload--------------#

#setwd("/home/user15/Downloads/David/SimulatedDataShiny")
#pdf('Univariates.pdf')
#for(name in names(trainSet[numericColsPosition])){
#  print(name)
#  plot(trainSet[,name],trainSet$New.Bodyload,cex=0.15,main=paste('Plot of new.bodyload vs.',name))
#  plot(trainSet[,name],trainSet$New.Bodyload/trainSet$Duration,cex=0.15,main=paste('Plot of new.bodyload/duration vs.',name))
#}
#dev.off()

#---------------------------Look at univariate plots to give an idea of how everything is related to new.bodyload--------------#
#Null model sets the benchmark for performance
intercept=mean(trainSet$New.Bodyload)
mean((intercept-trainSet$New.Bodyload)^2)
#Null model MSE 2142.121
( 1 - (sum((trainSet[,'New.Bodyload']-intercept)^2)/sum((trainSet[,'New.Bodyload']-mean(trainSet[,'New.Bodyload']))^2)))
#0 effron R^2 (obviously!)
mean((intercept-testSet$New.Bodyload)^2)
#2091.215

#----------------Look at tree model with errors plotted------------------------#

treeModel=rpart('New.Bodyload~.',trainSet[,numericCols],control = rpart.control(maxdepth=6,cp = 0.005))
#,control = rpart.control(cp = 0.05,maxdepth = 5)
summary(treeModel)
#Variable importance
#                  Distance Metabolic.Load.Time.Zone.1                   Duration             Impacts.Zone.1 Metabolic.Load.Time.Zone.2 
#                        17                         16                         16                         15                         15 
#Metabolic.Load.Time.Zone.3             Impacts.Zone.3             Impacts.Zone.2             Impacts.Zone.4       Accelerations.Zone.1 
#                        13                          3                          2                          1                          1 
fancyRpartPlot(treeModel,sub="")	
runDiagnostics(treeModel,trainSet)
#looks to be strong heteroscedasticity, model struggles to predict well on higher values
mean((trainSet$New.Bodyload-predict(treeModel,trainSet))^2)
#223.489
mean((testSet$New.Bodyload-predict(treeModel,testSet))^2)
#268.4247

effronRSquare(treeModel,trainSet)
#0.8956693
adjustedRSquare(treeModel,trainSet,length(treeModel$variable.importance))


#----------------Look at tree model with errors plotted------------------------#


#------------------------------Simple Linear Model-------------------------------#

#Start off with a simple linear model including all variables
linearModel=lm('New.Bodyload~.',trainSet[,numericCols])
summary(linearModel)
#Residuals:
#  Min      1Q  Median      3Q     Max 
#-45.862  -3.139  -0.850   1.775 188.788 

#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#(Intercept)                          -4.100e+04  4.267e+04  -0.961 0.336745    
#Duration                              1.290e-01  1.566e-01   0.823 0.410267    
#Distance                              1.754e-03  1.519e-03   1.155 0.248163    
#Max.Speed                            -2.365e-01  4.044e-02  -5.848 5.26e-09 ***
#  Avg.Speed                             1.812e-01  2.401e-01   0.755 0.450505    
#Avg.HR                               -8.813e-03  3.244e-03  -2.717 0.006604 ** 
#  HR.Exertion                           4.909e-03  6.893e-03   0.712 0.476386    
#Time.in.HR.Zone.1                     1.865e-01  1.595e-01   1.169 0.242399    
#Time.in.HR.Zone.2                     1.749e-01  1.589e-01   1.101 0.270981    
#Time.in.HR.Zone.3                     4.615e-01  1.793e-01   2.574 0.010087 *  
#  Time.in.HR.Zone.4                     3.992e-02  1.797e-01   0.222 0.824212    
#Time.in.HR.Zone.5                     2.610e-01  1.850e-01   1.411 0.158325    
#Time.in.HR.Zone.6                     3.832e-01  1.850e-01   2.071 0.038363 *  
# Speed.Exertion                       -9.398e-04  1.249e-04  -7.524 6.10e-14 ***
#  Accelerations.Zone.1                  7.105e-02  2.814e-02   2.525 0.011604 *  
# Accelerations.Zone.2                  1.347e-01  5.512e-02   2.444 0.014538 *  
#  Accelerations.Zone.3                 -4.219e-01  2.008e-01  -2.102 0.035617 *  
#  Decelerations.Zone.1                  1.982e-02  2.292e-02   0.865 0.387179    
#Decelerations.Zone.2                 -2.740e-02  4.492e-02  -0.610 0.541902    
#Decelerations.Zone.3                  1.439e-01  9.344e-02   1.540 0.123632    
#Impacts.Zone.1                       -1.182e-03  3.060e-04  -3.864 0.000113 ***
#  Impacts.Zone.2                        3.494e-02  1.147e-03  30.449  < 2e-16 ***
# Impacts.Zone.3                        1.772e-01  5.905e-03  30.016  < 2e-16 ***
# Impacts.Zone.4                        2.303e-01  2.410e-02   9.555  < 2e-16 ***
#  Impacts.Zone.5                        2.001e-01  5.939e-02   3.369 0.000759 ***
#  Impacts.Zone.6                        1.524e+00  6.807e-02  22.394  < 2e-16 ***
#  Collisions                            6.690e-01  2.947e-02  22.698  < 2e-16 ***
#  Running.Series                       -1.536e-01  8.979e-02  -1.711 0.087193 .  
##Running.Imbalance                    -5.379e-02  3.087e-02  -1.742 0.081482 .  
#Running.Imbalance.Standard.Deviation  8.398e-02  8.094e-02   1.038 0.299544    
#Running.Symmetry.Footstrikes          2.101e-02  4.486e-03   4.684 2.88e-06 ***
#  Metabolic.Load.Time.Zone.1           -2.892e-02  1.077e-01  -0.269 0.788282    
#Metabolic.Load.Time.Zone.2            3.167e+00  4.097e-01   7.731 1.24e-14 ***
#  Metabolic.Load.Time.Zone.3           -4.303e+00  1.389e+00  -3.097 0.001964 ** 
#  Metabolic.Load.Time.Zone.4           -6.376e+00  2.346e+00  -2.718 0.006579 ** 
#  Metabolic.Load.Time.Zone.5           -9.625e+00  3.225e+00  -2.984 0.002856 ** 
#  Metabolic.Power.Average               1.917e-01  1.848e-01   1.037 0.299743    
#High.Metabolic.Power.Distance         2.200e-03  5.205e-03   0.423 0.672567    
#Repeat.Sprints                        6.282e-02  1.427e-02   4.401 1.09e-05 ***
#  Sprint.Total.Distance                -9.392e-03  2.042e-03  -4.600 4.32e-06 ***
#  Work.Rate.Interval.Distance           7.045e-04  5.270e-04   1.337 0.181381    
#PlayerWeight                         -2.088e-02  1.414e-02  -1.477 0.139813    
#TimeHM                                2.758e-05  2.870e-05   0.961 0.336680    
#---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#Residual standard error: 9.879 on 5939 degrees of freedom
#Multiple R-squared:  0.9548,	Adjusted R-squared:  0.9545 
#F-statistic:  2985 on 42 and 5939 DF,  p-value: < 2.2e-16
predict(linearModel,se.fit=TRUE)$se.fit
se = predict(m,se.fit=TRUE)$se.fit
#Plot the residuals
runDiagnostics(linearModel,trainSet)
#Large values of nbl are not well predicted (underpredicted). Seems to be some heteroscedasticity
plot(linearModel)
MSE(linearModel)
TestMSE(linearModel)

#--------------------CAR analysis------------------#
#http://www.statmethods.net/stats/rdiagnostics.html
#Non-constant variance test
ncvTest(linearModel)
#low p-value means reject the null hypothesis that the variance of the residuals is constant 
#and infer that heteroscedasticity is present
#Non-constant Variance Score Test 
#Variance formula: ~ fitted.values 
#Chisquare = 1861.602    Df = 1     p = 0 

# plot studentized residuals vs. fitted values 
spreadLevelPlot(linearModel)

#Some variables like duration & distance don't appear to be signigicant in the model. Evaluate Collinearity, this is known to cause problems
#when inverting covariance matrices
#In statistics, the variance inflation factor (VIF) quantifies the severity of multicollinearity in an ordinary least squares regression 
#analysis. It provides an index that measures how much the variance (the square of the estimate's standard deviation) of an estimated 
#regression coefficient is increased because of collinearity.
#var(Beta_i)/var(Beta_i)min gives the variance inflation factor, the amount that the variance of a coefficient increases due to other
#predictors being present in the model. 
#1/(1−R2k)
#VIFk=1/(1−Rk2)
#where R2kRk2  is the R2-value obtained by regressing the kth predictor on the remaining predictors
#The general rule of thumb is that VIFs exceeding 4 warrant further investigation, while VIFs exceeding 10 are signs of serious multicollinearity requiring correction
vif(linearModel) # variance inflation factors 
names(which(vif(linearModel)>10)) # apparently values greater than 10 can cause significant problems
#[1] "Duration"                      "Distance"                      "Avg.Speed"                    
#[4] "HR.Exertion"                   "Time.in.HR.Zone.1"             "Time.in.HR.Zone.2"            
#[7] "Time.in.HR.Zone.3"             "Time.in.HR.Zone.4"             "Time.in.HR.Zone.5"            
#[10] "Time.in.HR.Zone.6"             "Accelerations.Zone.1"          "Decelerations.Zone.1"         
#[13] "Impacts.Zone.1"                "Running.Series"                "Running.Symmetry.Footstrikes" 
#[16] "Metabolic.Load.Time.Zone.1"    "Metabolic.Load.Time.Zone.2"    "Metabolic.Load.Time.Zone.3"   
#[19] "Metabolic.Load.Time.Zone.4"    "Metabolic.Load.Time.Zone.5"    "Metabolic.Power.Average"      
#[22] "High.Metabolic.Power.Distance" "Repeat.Sprints"                "Sprint.Total.Distance"        
#[25] "Work.Rate.Interval.Distance"  
#Multicollinearity looks to be a major problem in this model, agrees with correlation matrix


# Evaluate Nonlinearity
# component + residual plot 
crPlots(linearModel)
# Ceres plots 
ceresPlots(linearModel)

# Test for Autocorrelated Errors
durbinWatsonTest(linearModel)

# Global test of model assumptions
library(gvlma)
gvmodel <- gvlma(linearModel) 
summary(gvmodel)


#BoxCox transformation might be able to help hereroscedasticity

#bc <- boxcox(linearModel)
#(trans <- bc$x[which.max(bc$y)])
#boxCoxModel <- lm(New.Bodyload^trans~.,trainSet[,numericCols])
#summary(boxCoxModel)
#trainSet$New.Bodyload=trainSet$New.Bodyload^trans
#runDiagnostics(boxCoxModel,trainSet)
#trainSet$New.Bodyload=trainSet$New.Bodyload^(1/trans)
#ncvTest(boxCoxModel)
#plot(boxCoxModel)
#Doesn't seem to make a difference

#---------------------CAR analysis-------------------#

#look at residual plot vs. date to detect seasonal effects
residualPlot=ggplot(trainSet,aes(x=as.Date(trainSet$Date),y=resid(linearModel)))
residualPlot+geom_point(size=0.1)+stat_smooth()+geom_hline(yintercept = 0,color='red',size=0.15,linetype="dotted")

#Find MSE
mean((trainSet$New.Bodyload-predict.lm(linearModel,trainSet))^2)
#96.88407

#Roughly speaking, it is the average amount that the response will deviate from the true regression line
#The RSE is considered a measure of the lack of fit of the model to the data

#R^2 measures the proportion of variability in Y that can be explained using X
#it can be shown that in the simple linear regression setting, R^2 = r^2
#------------------------------Simple Linear Model-------------------------------#

#-------------------------------------Forward Subset Selection---------------------------------------#
#Before running a best subset selection, run a forward subset selection to get an idea of whether the model
#will require a lot of variables to predict or just a few. If the model requires lots of variables to achieve
#a low MSE and high R^2, then it could be the case that a best subset selection won't run. (since 45 choose x gets big)

#Reordering variables and trying again:
#  Warning message:
#  In leaps.setup(x, y, wt = wt, nbest = nbest, nvmax = nvmax, force.in = force.in,  :
#                   1  linear dependencies found

#function to get top x variables in regression model
getForwardSelectionTopX=function(a){
  #perform regression
  forwardSelection=regsubsets(New.Bodyload~.,data = trainSet[,numericCols], nvmax = a,method='forward')
  #extract from the summary the top x variables
  x=summary(forwardSelection)
  y=as.data.frame(x$which)
  names(y)[unlist(lapply(names(y),function(x) TRUE%in%y[,x]))]
}
#create list of variables of top 10 models
models=lapply(c(1:10),getForwardSelectionTopX)
#extract the relevant variables
getVars=function(numberOfVars){
  models[[numberOfVars]][2:length(models[[numberOfVars]])]
}

#Find what top 10 variables are included in a greedy fashion (if x appears in best model, it must also appear in second best)
#Look at linear models for first 10 varaibles
LinearModels=lapply(c(1:10),function(x) lm(as.formula(paste('New.Bodyload~',paste(getVars(x),collapse='+'))),trainSet[,numericCols]))
lapply(LinearModels,summary)
#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#(Intercept)                 3.6818483  0.6008408   6.128 9.48e-10 ***
#  Duration                    0.2946235  0.0120545  24.441  < 2e-16 ***
#  Max.Speed                  -0.2927142  0.0290386 -10.080  < 2e-16 ***
#  Avg.Speed                   0.5528906  0.0939082   5.888 4.13e-09 ***
#  Speed.Exertion             -0.0007943  0.0001119  -7.099 1.40e-12 ***
#  Impacts.Zone.2              0.0340486  0.0010603  32.112  < 2e-16 ***
#  Impacts.Zone.3              0.1783090  0.0056267  31.690  < 2e-16 ***
#  Impacts.Zone.4              0.2728646  0.0212253  12.856  < 2e-16 ***
#  Impacts.Zone.6              1.6992449  0.0616884  27.546  < 2e-16 ***
#  Collisions                  0.6537241  0.0272120  24.023  < 2e-16 ***
#  Metabolic.Load.Time.Zone.2  2.8274480  0.1037673  27.248  < 2e-16 ***
#Residual standard error: 10.02 on 5971 degrees of freedom
#Multiple R-squared:  0.9532,	Adjusted R-squared:  0.9531 
#F-statistic: 1.217e+04 on 10 and 5971 DF,  p-value: < 2.2e-16

plot(unlist(lapply(LinearModels,function(x)summary(x)$r.squared)),ylim=c(0,1),ylab='R^2 value',type='b',cex=0.5,main='R^2 of forward selection models with x variables')


#Before moving on, plot residuals to try and visually spot the effects of non-linearity
runDiagnostics(LinearModels[[10]],trainSet)
#Model is still underpredicting large values 
#overall not doing a bad job though
#Non-Linear terms could help improve the model as the residuals suggest that there is room for improving the fit
lapply(LinearModels[2:10],vif)
par(mfrow=c(2,3))
par(mar=c(12,4,2,2))
barplot(vif(LinearModels[[2]]),las=2,main='VIF',ylim=c(0,10),ylab='VIF')
barplot(vif(LinearModels[[3]]),las=2,main='VIF',ylim=c(0,10),ylab='VIF')
barplot(vif(LinearModels[[4]]),las=2,main='VIF',ylim=c(0,10),ylab='VIF')
barplot(vif(LinearModels[[5]]),las=2,main='VIF',ylim=c(0,10),ylab='VIF')
barplot(vif(LinearModels[[6]]),las=2,main='VIF',ylim=c(0,10),ylab='VIF')
barplot(vif(LinearModels[[7]]),las=2,main='VIF',ylim=c(0,10),ylab='VIF')
#Duration                  Max.Speed                  Avg.Speed 
#6.645785                   1.448330                   1.689741 
#Speed.Exertion             Impacts.Zone.2             Impacts.Zone.3 
#1.194595                   7.468578                   7.750210 
#Impacts.Zone.4             Impacts.Zone.6                 Collisions 
#5.296125                   1.884166                   3.281616 
#Metabolic.Load.Time.Zone.2 
#5.643623 

#durbinWatsonTest can reveal details about autocorrelation apparently
library(car)
durbinWatsonTest(LinearModels[[10]])
#Notice that Duration is the first variable in the model, with small standard error, wheras was 
#insignificant in other models (apparently)


#-------------------------------------Forward Subset Selection---------------------------------------#

#-------------------------Best Subset Selection-----------------------#

#Forward selection has indicated that fewer than 7 variables explain almost all of the variance in New.Bodyload
factorial(43)/(factorial(5)*factorial(43-5))
#962,598 models need to be tested for five variables!
bestSelectionLinear=regsubsets(New.Bodyload~.,data = trainSet[,numericCols], nvmax = 7,method='exhaustive')
dframeBestSelectionLinear=as.data.frame(summary(bestSelectionLinear)$which)
getBestSelectionLinearNames=function(x){
  names(dframeBestSelectionLinear)[unlist(lapply(names(dframeBestSelectionLinear),function(b) TRUE%in%dframeBestSelectionLinear[x,b]))][2:(x+1)]
}
#bestSelectionLinearNames=names(dframeBestSelectionLinear)[unlist(lapply(names(dframeBestSelectionLinear),function(b) TRUE%in%dframeBestSelectionLinear[6,b]))][2:6]
#getBestSelectionLinearNames(2)
#[2:length(getBestSelectionLinearNames(6))]
#bestSelectionLinearModel=lm(as.formula(paste('New.Bodyload~',paste(bestSelectionLinearNames,collapse='+'))),trainSet[,numericCols])
BestSubsetLinearModels=lapply(c(1:7),function(x) lm(as.formula(paste('New.Bodyload~',paste(getBestSelectionLinearNames(x),collapse='+'))),trainSet[,numericCols]))
lapply(BestSubsetLinearModels,summary)
#Model using 7 variables
#Residuals:
#  Min      1Q  Median      3Q     Max 
#-41.271  -3.163  -0.901   1.657 190.192 
#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#(Intercept)                0.383973   0.195072   1.968   0.0491 *  
#  Duration                   0.238764   0.010071  23.707   <2e-16 ***
#  Impacts.Zone.2             0.034871   0.001067  32.675   <2e-16 ***
#  Impacts.Zone.3             0.174445   0.005657  30.838   <2e-16 ***
#  Impacts.Zone.4             0.258313   0.021486  12.022   <2e-16 ***
#  Impacts.Zone.6             1.718833   0.062520  27.493   <2e-16 ***
#  Collisions                 0.639917   0.027570  23.211   <2e-16 ***
#  Metabolic.Load.Time.Zone.2 3.021499   0.094435  31.995   <2e-16 ***
#Residual standard error: 10.16 on 5974 degrees of freedom
#Multiple R-squared:  0.9519,	Adjusted R-squared:  0.9518 

vif(BestSubsetLinearModels[[7]])
#Duration             Impacts.Zone.2             Impacts.Zone.3             Impacts.Zone.4             Impacts.Zone.6                 Collisions 
#4.511876                   7.358782                   7.619188                   5.278591                   1.882301                   3.276219 
#Metabolic.Load.Time.Zone.2 
#4.546228 

runDiagnostics(bestSelectionLinearModel,trainSet)
#looks quite similar to others 

#Use ANOVA tests to decide how many variables are actually necessary
anova(LinearModels[[5]],bestSelectionLinearModel)

unlist(lapply(BestSubsetLinearModels,MSE))
#[1] 369.1312 202.2844 151.2791 133.7440 115.5075 105.5748 103.0809
unlist(lapply(BestSubsetLinearModels,TestMSE))
#[1] 380.7791 205.8617 160.5433 142.5996 129.4086 119.0694 116.9408

#-------------------------Best Subset Selection-----------------------#

#----------------------------Run Lasso Model to Compare Forward Subset with --------------------------------#

set.seed(1)
lasso=glmnet(as.matrix(trainSet[,setdiff(numericCols,'New.Bodyload')]),trainSet$New.Bodyload,alpha=1)
summary(lasso)
#plot how coefficients become non-zero when lambda gets smaller
plot(lasso,label=TRUE)

#Finds the x variables which can appear together in the model (all others are shrunk to 0)
getTopXLasso=function(lassoModel,x){
  which(lasso$beta[,which(lassoModel$df==x)[1]]>0)
}
length(lapply(c(2:10),function(x) names(getTopXLasso(lasso,x))))

lassoNames=lapply(c(2:10),function(x) names(getTopXLasso(lasso,x)))
lapply(c(1:8),function(x) setdiff(lassoNames[[x+1]],lassoNames[[x]]))
unlist(names(getTopXLasso(lasso,6)))
#setdiff(unlist(names(getTopXLasso(lasso,4))),unlist(names(getTopXLasso(lasso,3))))

#To find the optimum value of lambda, use cross validation (default 10) to train 10 different models and 
#test on the remaining data
cvfit = cv.glmnet(as.matrix(trainSet[,setdiff(numericCols,'New.Bodyload')]),trainSet$New.Bodyload,alpha=1)
plot(cvfit)
bestLambda=cvfit$lambda.min
#value for lambda of 0.01865597 gives smallest CV MSE
coef(cvfit,s=bestLambda)
#create a grid to search through for lambda
grid=10^seq(10,-2, length =100)
#Build a new glmnet (not using cross validation)
fullGlmNet=glmnet(as.matrix(trainSet[,setdiff(numericCols,'New.Bodyload')]),trainSet$New.Bodyload,alpha=1)
#s is the value of lambda, use the optimal value 
lasso.coef=predict(fullGlmNet ,type="coefficients",s=cvfit$lambda.min) 
#print non-zero coefficients
LassoCoefficients=lasso.coef[,1][lasso.coef[,1]!=0]
#There are a total of 28 predictors that are non-zero (which minimise CV MSE)
LassoCoefficients
#(Intercept)                             Duration                             Distance 
#-4.166456e+04                         2.869047e-01                         2.054162e-04 
#Max.Speed                            Avg.Speed                               Avg.HR 
#-2.089322e-01                         3.070508e-01                        -8.259107e-03 
#HR.Exertion                    Time.in.HR.Zone.1                    Time.in.HR.Zone.3 
#3.564339e-03                         7.840137e-03                         2.449901e-01 
#Time.in.HR.Zone.4                    Time.in.HR.Zone.5                    Time.in.HR.Zone.6 
#-1.131392e-01                         3.848471e-02                         2.272084e-01 
#Speed.Exertion                 Accelerations.Zone.1                 Accelerations.Zone.2 
#-8.929011e-04                         4.127267e-02                         9.937427e-02 
#Accelerations.Zone.3                 Decelerations.Zone.1                 Decelerations.Zone.2 
#-4.288941e-01                         1.208632e-02                        -1.967542e-02 
#Decelerations.Zone.3                       Impacts.Zone.1                       Impacts.Zone.2 
#1.135207e-01                        -5.475687e-04                         3.451600e-02 
#Impacts.Zone.3                       Impacts.Zone.4                       Impacts.Zone.5 
#1.783464e-01                         2.317882e-01                         2.086351e-01 
#Impacts.Zone.6                           Collisions                       Running.Series 
#1.542065e+00                         6.628137e-01                        -4.041502e-02 
#Running.Imbalance Running.Imbalance.Standard.Deviation         Running.Symmetry.Footstrikes 
#-5.057190e-02                         6.360532e-02                         1.457499e-02 
#Metabolic.Load.Time.Zone.1           Metabolic.Load.Time.Zone.2           Metabolic.Load.Time.Zone.3 
#8.250774e-02                         2.644060e+00                        -1.248675e+00 
#Metabolic.Load.Time.Zone.4           Metabolic.Load.Time.Zone.5              Metabolic.Power.Average 
#-3.535587e+00                        -5.512051e+00                         7.411306e-02 
#High.Metabolic.Power.Distance                       Repeat.Sprints                Sprint.Total.Distance 
#-4.187320e-03                         5.278032e-02                        -4.922285e-03 
#Work.Rate.Interval.Distance                         PlayerWeight                               TimeHM 
#1.145925e-03                        -1.739028e-02                         2.801583e-05
length(LassoCoefficients)-1
#41 predictors (Intercept not included)

#Predictions
lassoPredictions=predict.cv.glmnet(cvfit ,s=bestLambda,newx=as.matrix(trainSet[,setdiff(numericCols,'New.Bodyload')]))
mean((lassoPredictions -trainSet$New.Bodyload)^2)
#97.14124
lassoTestPredictions=predict.cv.glmnet(cvfit ,s=bestLambda,newx=as.matrix(testSet[,setdiff(numericCols,'New.Bodyload')]))
mean((lassoTestPredictions -testSet$New.Bodyload)^2)
#110.7204
( 1 - (sum((trainSet[,'New.Bodyload']-predict.cv.glmnet(cvfit,s=bestLambda,as.matrix(trainSet[,setdiff(numericCols,'New.Bodyload')])))^2)/sum((trainSet[,'New.Bodyload']-mean(trainSet[,'New.Bodyload']))^2)))
#0.9546518
#----------------------------Run Lasso Model to Compare Forward Subset with --------------------------------#

#----------------------------------Polynomial Regression----------------------------#
#Use poly(Distance,3) to avoid having correlations between polynomial terms
#This should help address some of the following problems associated with linear regression: 
#1. Possible Non-linearity of the response-predictor relationships.
#2. Correlation of error terms.
#3. Non-constant variance of error terms.
#4. Outliers.
#5. High-leverage points.
#6. Collinearity.

#Based on graphs of the data, there definintely seems to be non-linear trends once new.bodyload is >120. 
#One way to detect these would be to introduce polynomial terms for each variable.
#There are ~60 predictors, taking up to the -4,4th power for each would mean searching through 43*9=387 variables
#This should be feasible using the forward selection algorithm as before. Best subset for small values will also be reasonable

#generate up to 6th degree polynomial terms for all variables
#For powers<0, will get inf for zero values
#Already checked for NA values so inf values correspond to zero
trainSetPoly=trainSet
for(name in names(trainSet[,numericCols])){
  if(name!='New.Bodyload'){
  tempvec=trainSet[,name]
  bindVec=c()
  for(i in setdiff(seq(-4,4,1),c(0,1))){
    bindVec=cbind(bindVec,tempvec^i)
  }
  bindVec=as.data.frame(bindVec)
  names(bindVec)=c(unlist(lapply(setdiff(seq(-4,4,1),c(0,1)),function(x) paste(as.character(name),'^',x,sep=''))))
  trainSetPoly=cbind(trainSetPoly,bindVec)
  }
}
replaceInf=function(x){
  replace(x,x==Inf,0)
}

#names of columns containing infinite values
infCols=names(trainSetPoly)[which(unlist(lapply(names(trainSetPoly),function(x) any(is.infinite(trainSetPoly[,x])))))]
#replace the infinite values with 0
trainSetPoly[,infCols]=sapply(trainSetPoly[,infCols],replaceInf)
numericColsPoly=names(trainSetPoly)[sapply(trainSetPoly, is.numeric)]

#First build a linear regression model containing all variables
polyFullRegression=lm(New.Bodyload~.,trainSetPoly[,numericColsPoly])
summary(polyFullRegression)
#AdjustedR^2=0.9615
MSE(polyFullRegression)
#77.92312

#Need to create corresponding test set with transformed predictors to find test MSE
testSetPoly=testSet
for(name in names(testSet[,numericCols])){
  if(name!='New.Bodyload'){
    tempvec=testSet[,name]
    bindVec=c()
    for(i in setdiff(seq(-4,4,1),c(0,1))){
      bindVec=cbind(bindVec,tempvec^i)
    }
    bindVec=as.data.frame(bindVec)
    names(bindVec)=c(unlist(lapply(setdiff(seq(-4,4,1),c(0,1)),function(x) paste(as.character(name),'^',x,sep=''))))
    testSetPoly=cbind(testSetPoly,bindVec)
  }
}

infColsTest=names(testSetPoly)[which(unlist(lapply(names(testSetPoly),function(x) any(is.infinite(testSetPoly[,x])))))]
#replace the infinite values with 0
testSetPoly[,infCols]=sapply(testSetPoly[,infCols],replaceInf)
numericColsPolyTest=names(testSetPoly)[sapply(testSetPoly, is.numeric)]
mean((predict(polyFullRegression,testSetPoly)-testSetPoly$New.Bodyload)^2)
#159.6915
length(polyFullRegression$coefficients)


#Now perform forward stepwise regression as before
getBestSelectionTopXPoly=function(a){
  print(paste('Model searching for top',a,'variables'))
  bestSelection=regsubsets(New.Bodyload~.,data = trainSetPoly[,numericColsPoly], nvmax = a,method='forward',really.big=TRUE)
  x=summary(bestSelection)
  y=as.data.frame(x$which)
  names(y)[unlist(lapply(names(y),function(b) TRUE%in%y[,b]))]
}
#Find top 10 models
modelsPoly=lapply(c(0:10),getBestSelectionTopXPoly)
getVars=function(numberOfVars){
  modelsPoly[[numberOfVars]][2:length(modelsPoly[[numberOfVars]])]
}

#Find what top 10 variables are included in a greedy fashion (if x appears in best model, it must also appear in second best)
#Look at linear models for first 10 varaibles
LinearModelsPoly=lapply(c(1:10),function(x) lm(as.formula(paste('New.Bodyload~',paste(getVars(x),collapse='+'))),trainSetPoly[,numericColsPoly]))
lapply(LinearModelsPoly,summary)

#Disappointing! No non-linear term appeared in the model
#For 5 variables best subset gave 
#Residuals:
#  Min      1Q  Median      3Q     Max 
#-62.897  -3.915  -1.275   2.149 189.174 
#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#(Intercept)    1.374805   0.215159    6.39 1.79e-10 ***
#  Duration       0.443705   0.009449   46.96  < 2e-16 ***
#  Impacts.Zone.2 0.054471   0.000978   55.70  < 2e-16 ***
#  Impacts.Zone.3 0.135789   0.006102   22.25  < 2e-16 ***
#  Impacts.Zone.4 0.421527   0.023117   18.23  < 2e-16 ***
#  Impacts.Zone.6 2.149872   0.066031   32.56  < 2e-16 ***
#Residual standard error: 11.31 on 5976 degrees of freedom
#Multiple R-squared:  0.9404,	Adjusted R-squared:  0.9403 
#F-statistic: 1.885e+04 on 5 and 5976 DF,  p-value: < 2.2e-16


###----------------------------------  BE CAREFUL TAKES A FEW MINUTES TO RUN!!!!!!!! -------------------------------###
bestSelectionPoly=regsubsets(New.Bodyload~.,data = trainSetPoly[,numericColsPoly], nvmax = 3,method='exhaustive',really.big=TRUE)
x=summary(bestSelectionPoly)
y=as.data.frame(x$which)
sapply(y,which)
names(y)[unlist(lapply(names(y),function(b) TRUE%in%y[4,b]))]
PolynomialBestSubset=lm(as.formula(paste('New.Bodyload~',paste(names(y)[unlist(lapply(names(y),function(b) TRUE%in%y[4,b]))][2:5],collapse='+'))),trainSetPoly[,numericColsPoly])
summary(PolynomialBestSubset)
#Best subset selection shouldn't be different to the linear regresion subset selection unless
#non-linear transformations included! No idea why this has happened....
#Residuals:
#  Min      1Q  Median      3Q     Max 
#-41.474  -4.346  -1.066   1.875 191.739 
#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#(Intercept)                0.482164   0.222028   2.172   0.0299 *  
#  Impacts.Zone.2             0.032588   0.001164  27.994   <2e-16 ***
#  Impacts.Zone.3             0.245064   0.004727  51.845   <2e-16 ***
#  Collisions                 1.311361   0.025460  51.507   <2e-16 ***
#  Metabolic.Load.Time.Zone.2 4.281856   0.095150  45.001   <2e-16 ***
#Residual standard error: 11.57 on 5977 degrees of freedom
#Multiple R-squared:  0.9376,	Adjusted R-squared:  0.9375 
#F-statistic: 2.244e+04 on 4 and 5977 DF,  p-value: < 2.2e-16

###----------------------------------  BE CAREFUL TAKES A FEW MINUTES TO RUN!!!!!!!! -------------------------------@@@

MSE(PolynomialBestSubset)
#133.744
TestMSE(PolynomialBestSubset)
#142.5996

#anova checks whether one model is superior to another:
lapply(c(1:10),function(x) anova(LinearModels[[x]],LinearModelsPoly[[x]]))

#Again run diagnostics 
runDiagnostics(LinearModelsPoly[[3]],trainSet)
#Same problem as all other models, heterscedstaticity present
#Negligible difference adding non-linear terms to regression

#Reset NumericCols
numericCols=names(trainSet)[sapply(trainSet, is.numeric)]
nonNumericCols=names(trainSet)[!sapply(trainSet, is.numeric)]
#add position to another variable filter as it could be discriminatory
numericCols=setdiff(numericCols,'Outlierindicator')
#free up some RAM
rm(trainSetPoly)

#----------------------------------Polynomial Regression----------------------------#

#----------------------------Linear Regression With Interaction Terms-------------------#

#apparently important variables in the model
importantVars=c('Distance','Duration','Collisions','Metabolic.Load.Time.Zone.2'
                ,'Impacts.Zone.2','Impacts.Zone.3','Impacts.Zone.4','Impacts.Zone.6','New.Bodyload','Max.Speed','Speed.Exertion')

#Expect to see interaction with Duration for some of these.
lapply(importantVars,function(x) print(plotGGScatter('Duration',x,trainSet)))

#Add all possible interaction terms in relevant variables
trainSetInteraction=trainSet
for(name in importantVars){
  if(length(importantVars)>1){
    if(name!='New.Bodyload'){
      tempvec=trainSet[,name]
      bindVec=c()
      for(i in setdiff(importantVars,c('New.Bodyload',name))){
        #print(paste('adding ',name,'*', i))
        bindVec=cbind(bindVec,tempvec*(trainSetInteraction[,i]))
      }
      bindVec=as.data.frame(bindVec)
      names(bindVec)=c(unlist(lapply(setdiff(importantVars,c('New.Bodyload',name)),function(x) paste(as.character(name),'*',x,sep=''))))
      trainSetInteraction=cbind(trainSetInteraction,bindVec)
    }
    importantVars=setdiff(importantVars,name)
  }
}

numericColsInteraction=names(trainSetInteraction)[sapply(trainSetInteraction, is.numeric)]
nonNumericColsInteraction=names(trainSetInteraction)[!sapply(trainSetInteraction, is.numeric)]

#Perform best subset selection to find the top 5 variables possibly including interaction variables
#The criterion for best should be changed to R^2 or something similar here
#-------------------------BE CAREFUL THIS TAKES A FEW MINUTES TO RUN!!!!!!----------------------------------#

bestSelectionInteraction=regsubsets(New.Bodyload~.,data = trainSetInteraction[,numericColsInteraction], nvmax = 5,method='exhaustive',really.big=TRUE)

#-------------------------BE CAREFUL THIS TAKES A FEW MINUTES TO RUN!!!!!!----------------------------------#

dframeBestSelectionInteraction=as.data.frame(summary(bestSelectionInteraction)$which)
bestSelectionInteractionNames=names(dframeBestSelectionInteraction)[unlist(lapply(names(dframeBestSelectionInteraction),function(b) TRUE%in%dframeBestSelectionInteraction[5,b]))]

interactionModel=lm(as.formula(paste('New.Bodyload~',paste(setdiff(bestSelectionInteractionNames,'(Intercept)'),collapse='+'))),trainSetInteraction[,numericColsInteraction])
summary(interactionModel)
#Residuals:
#  Min      1Q  Median      3Q     Max 
#-62.828  -3.563  -1.295   1.695 189.981 
#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#(Intercept)                3.6770763  0.2189044   16.80   <2e-16 ***
#  Impacts.Zone.2             0.0329358  0.0010854   30.35   <2e-16 ***
#  Impacts.Zone.3             0.2427414  0.0043049   56.39   <2e-16 ***
#  Impacts.Zone.6             2.2780889  0.0591897   38.49   <2e-16 ***
#  Metabolic.Load.Time.Zone.2 3.8422935  0.0894858   42.94   <2e-16 ***
#  `Duration*Collisions`      0.0106171  0.0002926   36.28   <2e-16 ***
#Residual standard error: 10.72 on 5976 degrees of freedom
#Multiple R-squared:  0.9464,	Adjusted R-squared:  0.9463 
#F-statistic: 2.109e+04 on 5 and 5976 DF,  p-value: < 2.2e-16

MSE(interactionModel)
#114.8654
testSet[,'Duration*Collisions']=testSet$Duration*testSet$Collisions
TestMSE(interactionModel)

vif(interactionModel)
#Impacts.Zone.2             Impacts.Zone.3             Impacts.Zone.6 Metabolic.Load.Time.Zone.2      `Duration*Collisions` 
#      6.832770                   3.961183                   1.514555                   3.664591                   2.547588 

runDiagnostics(interactionModel,trainSetInteraction)

rm(trainSetInteraction)
rm(bestSelectionInteraction)

#-----------------------------------Linear Regression With Interaction Terms--------------------------------#

#----------------------------------- Piecewise Linear Regression -------------------------------------#
piecewiseFormula=as.formula("New.Bodyload ~ Distance:I(Distance>3500)+
                                              Collisions+
                                              Metabolic.Load.Time.Zone.2:I(Metabolic.Load.Time.Zone.2>8)+
                                              Impacts.Zone.2:I(Impacts.Zone.2>1000)+
                                              Impacts.Zone.3:I(Impacts.Zone.3>150)+
                                              Impacts.Zone.4:I(Impacts.Zone.4>45)+
                                              Impacts.Zone.6")
LinearModelPiecewise=lm(piecewiseFormula,trainSet[,numericCols])
summary(LinearModelPiecewise)
length(LinearModelPiecewise$coef)
runDiagnostics(LinearModelPiecewise,trainSet)

#----------------------------------- Piecewise Linear Regression -------------------------------------#

#----------------------------------------homoscedasticity analysis-------------------------------------#

#A problem with all of the models so far is that they cannot predict high values of New.Bodyload accurately.
#Take a look here to see why. plotGGScatter is an effective way of diagnosing the problem
plotGGScatter('Impacts.Zone.2',"New.Bodyload",trainSet)
#mild non-linear relationship
plotGGScatter('Impacts.Zone.3',"New.Bodyload",trainSet)
#mild non-linear relationship
plotGGScatter('Impacts.Zone.4',"New.Bodyload",trainSet)
#mild non-linear relationship
plotGGScatter('Impacts.Zone.5',"New.Bodyload",trainSet)
#mild non-linear relationship
plotGGScatter('Impacts.Zone.6',"New.Bodyload",trainSet)
#mild non-linear relationship
plotGGScatter('Metabolic.Load.Time.Zone.2',"New.Bodyload",trainSet)
#stronger non-linear relationship
plotGGScatter('Distance',"New.Bodyload",trainSet)
#mild non-linear relationship
plotGGScatter('Duration',"New.Bodyload",trainSet)
#mild non-linear relationship
plotGGScatter('Collisions',"New.Bodyload",trainSet)
#mild non-linear relationship

#For most of the variables, there is a linear relationship with New.Bodyload until New.Bodyload values exceed 120, in which case
#New.Bodyload no longer has a linear relationship with the model

#Look at fitting a logistic regression model to first predict high values of New.Bodyload? 
#Clustering the higher values may be another method. homoscedasticity seems to be inherent to the data, so let a 
#clustering algorithm try and detect the pattern
#Goal of clustering is to partition the data into two subsets, one of which has 'normal' values of NBL
#and the other has other values of NBL.

#First try clustering as logistic regression involves deciding on the 'breakpoint' manually.


#trainSet$highNBL=ifelse(trainSet$New.Bodyload>=120)
set.seed(1)
#Only try and cluster on the data with relevant variables, otherwise clustering just 
#on noise
importantVars=c('Distance','Duration','Collisions','Metabolic.Load.Time.Zone.2'
  ,'Impacts.Zone.2','Impacts.Zone.3','Impacts.Zone.4','Impacts.Zone.6','New.Bodyload')
#First just include distance and NBL
kMeansDurNBL=kmeans(trainSet[,importantVars],
                    2,nstart=20)
table(kMeansDurNBL$cluster)   
#1    2 
#1125 4857 

#Agrees with how the bivariable graphs look
kMeansDurNBL$centers
#Distance Duration Collisions Metabolic.Load.Time.Zone.2 Impacts.Zone.2 Impacts.Zone.3 Impacts.Zone.4 Impacts.Zone.6 New.Bodyload
#1 4853.215 79.19297  19.755556                   7.983911       913.8320      158.10578      30.792889       4.470222    131.83124
#2 1010.987 13.83067   4.698168                   1.704941       220.5435       40.30348       7.678608       1.155240     30.37499
kMeansDurNBL$size
#[1] 1125 4857

trainSet$clusters=kMeansDurNBL$cluster
plotGGScatterCluster('Impacts.Zone.2',"New.Bodyload",trainSet)
#mild non-linear relationship
plotGGScatterCluster('Impacts.Zone.3',"New.Bodyload",trainSet)
#mild non-linear relationship
plotGGScatterCluster('Impacts.Zone.4',"New.Bodyload",trainSet)
#mild non-linear relationship
plotGGScatterCluster('Impacts.Zone.5',"New.Bodyload",trainSet)
#mild non-linear relationship
plotGGScatterCluster('Impacts.Zone.6',"New.Bodyload",trainSet)
#mild non-linear relationship
plotGGScatterCluster('Metabolic.Load.Time.Zone.2',"New.Bodyload",trainSet)
#stronger non-linear relationship
plotGGScatterCluster('Distance',"New.Bodyload",trainSet)
#mild non-linear relationship
plotGGScatterCluster('Duration',"New.Bodyload",trainSet)
#mild non-linear relationship
plotGGScatterCluster('Collisions',"New.Bodyload",trainSet)


hClusteringDurNBL =hclust(dist(scale(trainSet[,c('Duration','New.Bodyload')])), method="complete")
plot(hClusteringDurNBL , main="Complete Linkage", xlab="", sub="",cex=.9)
hClusteringDurNBL$method
summary(trainSet[cutree(hClusteringDurNBL , 2)==1,'Duration'])
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#45.50   75.48   83.78   84.22   92.51  192.40 
summary(trainSet[cutree(hClusteringDurNBL , 2)==1,'New.Bodyload'])
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#57.6   124.5   145.2   145.7   167.6   199.8 
summary(trainSet[cutree(hClusteringDurNBL , 2)==2,'Duration'])
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#0.90    9.67   12.67   16.38   17.86   95.85 
summary(trainSet[cutree(hClusteringDurNBL , 2)==2,'New.Bodyload'])
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#1.30   16.35   27.42   33.31   43.10  197.90 

#The clusters don't seem to partition the Duration vs. NBL graph as well as they could.
plotGGScatter('Duration','New.Bodyload',trainSet[cutree(hClusteringDurNBL , 2)==2,])
plotGGScatter('Duration','New.Bodyload',trainSet[cutree(hClusteringDurNBL , 2)==1,])


#Data seems to naturally cluster into groups split around New.Bodyload and Duration

plot(cutree(hClusteringDurNBL , 2),trainSet$New.Bodyload,cex=0.1)
plot(cutree(hClusteringDurNBL , 3),trainSet$New.Bodyload,cex=0.1)
plot(cutree(hClusteringDurNBL , 4),trainSet$New.Bodyload,cex=0.1)
plot(cutree(hClusteringDurNBL , 5),trainSet$New.Bodyload,cex=0.1)


kMeansResult=kmeans(trainSet[,c('Distance','Duration','Collisions','Metabolic.Load.Time.Zone.2'
                                ,'Impacts.Zone.2','Impacts.Zone.3','Impacts.Zone.4','Impacts.Zone.6','New.Bodyload')],
                    2,nstart=20)
table(kMeansResult$cluster)   
#1    2 
#4857 1125 
#partitioned into two groups, one 18.80642% of data, other 81.19358%
kMeansResult$centers
#Centers look about right, agree quite strongly with graphs
trainSet$clusterIndicator=ifelse(kMeansResult$cluster==1,0,1)
#1 represents the 'strange' looking data, 0 the normal looking data

#look at how the clustering algorithm separated the data. 
#it seems like it is not doing the intended job, could try and split on just New.Bodyload
lapply(c('Distance','Duration','Collisions','Metabolic.Load.Time.Zone.2'
         ,'Impacts.Zone.2','Impacts.Zone.3','Impacts.Zone.4','Impacts.Zone.6'),function(x) plotGGScatter(x,"New.Bodyload",trainSet[trainSet$clusterIndicator==0,]))
lapply(c('Distance','Duration','Collisions','Metabolic.Load.Time.Zone.2'
         ,'Impacts.Zone.2','Impacts.Zone.3','Impacts.Zone.4','Impacts.Zone.6'),function(x) plotGGScatter(x,"New.Bodyload",trainSet[trainSet$clusterIndicator==1,]))

clusterFormula=as.formula("New.Bodyload ~ Duration+Collisions+Metabolic.Load.Time.Zone.2+
                                              Impacts.Zone.2+Impacts.Zone.3+Impacts.Zone.4+Impacts.Zone.6+Duration")
ClusteredModel=lm(clusterFormula,trainSet[trainSet$clusterIndicator==0,numericCols])
summary(ClusteredModel)
runDiagnostics(ClusteredModel,trainSet[trainSet$clusterIndicator==0,])
#K-means clustering into two groups doens't really help with regression. 

#Look at hierarchal clustering for inference: 
#Different linkages give different results
hClusteringComplete =hclust(dist(scale(trainSet[,numericCols])), method="complete")
hClusteringAverage =hclust(dist(scale(trainSet[,numericCols])), method ="average")
hClusteringSingle=hclust(dist(scale(trainSet[,numericCols])), method ="single")

plot(hClusteringComplete , main="Complete Linkage", xlab="", sub="",cex=.9)
plot(hClusteringAverage , main="Average Linkage", xlab="", sub="",cex=.9)
plot(hClusteringSingle , main="Single Linkage", xlab="", sub="",cex=.9)

#Difficult to interpret the hierarchal results. Would be easier for numeric data
plot(cutree(hClusteringComplete , 1),trainSet$New.Bodyload,cex=0.1)
plot(cutree(hClusteringComplete , 2),trainSet$New.Bodyload,cex=0.1)
plot(cutree(hClusteringComplete , 3),trainSet$New.Bodyload,cex=0.1)
plot(cutree(hClusteringComplete , 4),trainSet$New.Bodyload,cex=0.1)
plot(cutree(hClusteringComplete , 5),trainSet$New.Bodyload,cex=0.1)
plot(cutree(hClusteringComplete , 6),trainSet$New.Bodyload,cex=0.1)


#----------------------------------------homoscedasticity analysis-------------------------------------#

#----------------------------------------Linear Regression With Splines-----------------------------------#

#Looking at diagnostic plots, it seems like all of the models are having a hard time predicting new.bodyload values
#which are greater than 120; furthermore the models seem reluctant to make many predictions of values greater than
#200. This could be because there are errors in the data, it could also be the case that there is some inherent
#change that makes these values higher. (Could this be that they were all recorded around a certain date?
#Or perhaps around a certain time?) Either way, linear regression with splines could help to detect the
#non-linear part of predictions and make a piecewise model that is more accurate.

#Explore whether large values depend on date first
hist(trainSet[which(trainSet$New.Bodyload>120),'Date'],breaks='months')
summary(trainSet[which(trainSet$New.Bodyload>120),'DrillTime'])
#Doesn't seem to be anything significant, could check whether they are distributed evenly across
#data but doesn't seem worthwhile.

#fit a regression spline
#check where cut points should be from graph

#need to detach car package to avoid naming conflicts
detach("package:car", unload=TRUE)
#bs=basis spline
#Could spend time on this figuring out how to split the variables in the 'best' manner
regressionSpline=lm(New.Bodyload~cut(Distance,4),data=trainSet)
summary(regressionSpline)
runDiagnostics(regressionSpline,trainSet)
plot(regressionSpline)
#also look at the gam function which is more suitable for this
gamModel=gam(New.Bodyload~lo(Distance,span=0.9) +Duration,data=trainSet)

require(mgcv)
gamobj=gam(New.Bodyload~s(Distance)+s(Duration)+s(Metabolic.Load.Time.Zone.2)+Impacts.Zone.2+Impacts.Zone.3+Impacts.Zone.6+Collisions,family=gaussian(link=identity),data=trainSet)
#Still seems to be heteroscedasticity
summary(gamobj)
runDiagnostics(gamobj,trainSet)
plot(gamobj , se=TRUE , col="red")


#----------------------------------------Linear Regression With Splines---------------------------------#

#----------------------------------Principal Components Analysis -----------------------------------------#
#doing this before principal components regression because dimension reducing techinques are key for this data set (interpretability motivating this)
#T&H
#When faced with a large set of correlated variables, principal components allow us to summarize this set with
#a smaller number of representative variables that collectively explain most of the variability in the original set
#To perform principal components regression, we simply use principal components as
#predictors in a regression model in place of the original larger set of variables.

apply(trainSet[,setdiff(numericCols,'New.Bodyload')],2,mean)
apply(trainSet[,setdiff(numericCols,'New.Bodyload')],2,var)
#largely different means and variances in the data. The data must be standardised to perform PCA in order to be able to find the directions where 
#the relative variance is highest
#scale ensures all variables have standard deviation of 1
biplot(principalComp)
principalComp=prcomp(trainSet[,setdiff(numericCols,c('New.Bodyload'))],scale=TRUE)
principalComp
summary(principalComp)
#Importance of components:
#  PC1    PC2     PC3     PC4     PC5     PC6     PC7     PC8     PC9   PC10    PC11    PC12
#Standard deviation     4.4569 1.7642 1.69396 1.40514 1.29562 1.12138 1.08602 1.05008 0.97942 0.9369 0.87461 0.86899
#Proportion of Variance 0.4729 0.0741 0.06832 0.04701 0.03997 0.02994 0.02808 0.02625 0.02284 0.0209 0.01821 0.01798
#Cumulative Proportion  0.4729 0.5471 0.61537 0.66238 0.70234 0.73228 0.76037 0.78662 0.80946 0.8304 0.84857 0.86655

#standard deviation of each vector
principalComp$sdev
#variance of each vector
principalComp$sdev^2
#proportion of variance in dataset explained - first 15 vectors
(principalComp$sdev^2/sum(principalComp$sdev^2))[1:15]
#[1] 0.47294402 0.07410115 0.06832108 0.04700966 0.03996757 0.02994052 0.02808189 0.02625391 0.02283973 0.02089968
#[11] 0.01821286 0.01797941 0.01555519 0.01371054 0.01237104
pve=(principalComp$sdev^2/sum(principalComp$sdev^2))
# 4.729440e-01 7.410115e-02 6.832108e-02 4.700966e-02 3.996757e-02 2.994052e-02 2.808189e-02 2.625391e-02 2.283973e-02 2.089968e-02 1.821286e-02

plot(pve , xlab=" Principal Component ", ylab="Proportion of Variance Explained ", ylim=c(0,1),type='b',cex=0.2,main = 'Proportion of variance explained by each vector')
#cumulative plot of proportion of variance explained
plot(cumsum(pve), xlab="Principal Component ", ylab="Cumulative Proportion of Variance Explained ", ylim=c(0,1),type='b',cex=0.2,main='Cumulative PVE')

#plot loading contributions of first 3 principal components
par(mar=c(16,4,1,1))
barplot(sort(principalComp$rotation[,1],decreasing=TRUE),las=2,main='Loadings of First Principal Component')
#Distance/Duration variables
barplot(sort(principalComp$rotation[,2],decreasing=TRUE),las=2)
#Rate variables (speed)
barplot(sort(principalComp$rotation[,3],decreasing=TRUE),las=2)
#Intensity? Low HR. vs. High HR.

par(mar=c(5,5,5,5))
#Conclusions:
#51% of the variance in the data can be explained by the first principal component, other principal components don't
#contribute much individually
#Should see highly correlated variables having approximately the same magnitude and direction
#Makes sense then that distance contributes highly to the first principle component; it is highly correlated to a lot of 
#other variables and most other variables are constructed from it.
#Second principal component seems to focus on speed variables.
#Come back to the interpretation of these graphs and figures



#----------------------------------Principal Components Analysis -----------------------------------------#



#----------------------------------Principal Components Regression---------------------------------#

#The principal components regression (PCR) approach involves constructing 
#the first M principal components, Z1,...,ZM, and then using these components
#as the predictors in a linear regression model that is fit
#using least squares. The key idea is that often a small number of principal
#components suffice to explain most of the variability in the data, as
#well as the relationship with the response. In other words, we assume that
#the directions in which X1,...,Xp show the most variation are the directions
#that are associated with Y (t&h)
library (pls)
set.seed(1)
principalCompRegression=pcr(New.Bodyload ~ ., data=trainSet[,numericCols], scale=TRUE,validation ="CV")
summary(principalCompRegression)
validationplot(principalCompRegression , val.type="MSEP")
#looks like MSE reaches an effective minimum using 7 components
#Use train in caret package to check

controlParamsPCR=trainControl(method = "cv", number = 10)
gridPCR=expand.grid(.ncomp=seq(3,30,1))
PCR_fit <- train(New.Bodyload~., data = trainSet[,numericCols],
                 method = "pcr",
                 preProcess = c("scale"),
                 tuneGrid = gridPCR,
                 metric='RMSE',
                 trControl = controlParamsPCR)
plot(PCR_fit)
#Plot reveals 23 is approximate optimum, generate models with no. terms up to 23
principalCompRegressionOpt=pcr(New.Bodyload ~ ., data=trainSet[,numericCols], scale=TRUE,ncomp=23)
summary(principalCompRegressionOpt)

R2(principalCompRegressionOpt)
#(Intercept)      1 comps      2 comps      3 comps      4 comps      5 comps      6 comps      7 comps      8 comps      9 comps     10 comps  
#0.0000       0.8490       0.8583       0.8962       0.9005       0.9024       0.9216       0.9219       0.9219       0.9289       0.9292  
#11 comps     12 comps     13 comps     14 comps     15 comps     16 comps     17 comps     18 comps     19 comps     20 comps     21 comps  
#0.9302       0.9309       0.9309       0.9309       0.9316       0.9352       0.9361       0.9375       0.9387       0.9388       0.9406  
#22 comps     23 comps  
#0.9425       0.9523  


par(mfrow=c(1,1))
ggplot(data=as.data.frame(cbind(c(1:20),
                  unlist(lapply(c(1:20),function(x) mean((trainSet$New.Bodyload-predict(principalCompRegressionOpt,ncomp=x,trainSet))^2))))),aes(V1,V2))+
  geom_line(size=0.1)+
  geom_point()+
  ggtitle('PCR MSE')+
  xlab('Number of Variables')+
  ylab('MSE')+
  geom_point(data=as.data.frame(cbind(c(1:20),
                                      unlist(lapply(c(1:20),function(x) mean((testSet$New.Bodyload-predict(principalCompRegressionOpt,ncomp=x,testSet))^2))))),aes(V1,V2),colour='red')+
  geom_line(data=as.data.frame(cbind(c(1:20),
                                unlist(lapply(c(1:20),function(x) mean((testSet$New.Bodyload-predict(principalCompRegressionOpt,ncomp=x,testSet))^2))))),aes(V1,V2),colour='red')
#9 variables seems to give the best model in terms of MSE vs. #predictors
#MSE for predictions made on train set
mean((trainSet$New.Bodyload-predict(principalCompRegressionOpt,ncomp=9,trainSet))^2)
#152.2612
#MSE for predictions made on test set
mean((testSet$New.Bodyload-predict(principalCompRegressionOpt,ncomp=9,testSet))^2)
#170.5392





#----------------------------------Principal Components Regression---------------------------------#

#----------------------------------------Poisson--------------------------------#

poissonModel <- cpglm(interactionFormula, data = trainSet)
runDiagnostics(poissonModel,trainSet)
print(ggplot(trainSet,aes(New.Bodyload, predict(poissonModel)))+geom_point(size=0.1)+geom_smooth(method="smooth.spline2",se=T)+ggtitle('Residual Plot of Model')+theme(plot.title = element_text(hjust = 0.5))+geom_abline(yintercept = 0,slope=1,colour='red',linetype="dashed"))
#Efron's R Square

RSquarePoisson <- 1 - (sum((trainSet$New.Bodyload-predict(poissonModel))^2)/sum((trainSet$New.Bodyload-mean(trainSet$New.Bodyload))^2))
r1 <- resid(poissonModel) / sqrt(poissonModel$phi)
plot(r1 ~ fitted(poissonModel), cex = 0.5)
qqnorm(r1, cex = 0.5)
qqline(r1)  

plot(resid(poissonModel),cex=0.1)

#----------------------------------------Poisson--------------------------------#



#---------------------------------Partial Least Squares Regression-------------------------------#
#The PCR approach that we just described involves identifying linear combinations,
#or directions, that best represent the predictors X1,...,Xp. These
#directions are identified in an unsupervised way, since the response Y is not
#used to help determine the principal component directions. That is, the
#response does not supervise the identification of the principal components.
#Consequently, PCR suffers from a drawback: there is no guarantee that the
#directions that best explain the predictors will also be the best directions
#to use for predicting the response. (t&h)


#Like PCR, PLS is a dimension reduction method, which first identifies squares
#a new set of features Z1,...,ZM that are linear combinations of the original
#features, and then fits a linear model via least squares using these M new
#features. But unlike PCR, PLS identifies these new features in a supervised
#way—that is, it makes use of the response Y in order to identify new
#features that not only approximate the old features well, but also that are
#related to the response. Roughly speaking, the PLS approach attempts to
#find directions that help explain both the response and the predictors. (T&H)

set.seed(1)
partialLeastSquares=plsr(New.Bodyload~., data=trainSet[,numericCols], scale=TRUE ,validation ="CV")
summary(partialLeastSquares)
validationplot(partialLeastSquares ,val.type="MSEP")
#Optimal reached using ~2 variables
controlParamsPCR=trainControl(method = "cv", number = 10)
gridPCR=expand.grid(.ncomp=seq(3,30,1))
PLS_fit <- train(New.Bodyload~., data = trainSet[,numericCols],
                 method = "pls",
                 preProcess = c("scale"),
                 tuneGrid = gridPCR,
                 metric='RMSE',
                 trControl = controlParamsPCR)
plot(PLS_fit)
#looks to be optimised for #components~10

PLSOpt=plsr(New.Bodyload~., data=trainSet[,numericCols],ncomp=15)
summary(PLSOpt)
#Data: 	X dimension: 5982 42 
#Y dimension: 5982 1
#Fit method: kernelpls
#Number of components considered: 15
#TRAINING: % variance explained
#              1 comps  2 comps  3 comps  4 comps  5 comps  6 comps  7 comps  8 comps  9 comps  10 comps  11 comps  12 comps  13 comps  14 comps
#X               23.43    94.13    97.84    98.94    99.65    99.84    99.89    99.95    99.97     99.98     99.99     99.99     99.99    100.00
#New.Bodyload    74.86    77.19    80.02    84.83    86.17    88.89    89.94    90.94    92.88     93.08     93.28     94.87     95.01     95.04
#15 comps
#X               100.00
#New.Bodyload     95.21

R2(PLSOpt)
#(Intercept)      1 comps      2 comps      3 comps      4 comps      5 comps      6 comps      7 comps      8 comps      9 comps     10 comps  
#0.0000       0.7486       0.7719       0.8002       0.8483       0.8617       0.8889       0.8994       0.9094       0.9288       0.9308  
#11 comps     12 comps     13 comps     14 comps     15 comps  
#0.9328       0.9487       0.9501       0.9504       0.9521  


ggplot(data=as.data.frame(cbind(c(1:15),
                                unlist(lapply(c(1:15),function(x) mean((trainSet$New.Bodyload-predict(PLSOpt,ncomp=x,trainSet))^2))))),aes(V1,V2))+
  geom_line(size=0.1)+
  geom_point()+
  ggtitle('PCR MSE')+
  xlab('Number of Variables')+
  ylab('MSE')+
  geom_point(data=as.data.frame(cbind(c(1:15),
                                      unlist(lapply(c(1:15),function(x) mean((testSet$New.Bodyload-predict(PLSOpt,ncomp=x,testSet))^2))))),aes(V1,V2),colour='red')+
  geom_line(data=as.data.frame(cbind(c(1:15),
                                     unlist(lapply(c(1:15),function(x) mean((testSet$New.Bodyload-predict(PLSOpt,ncomp=x,testSet))^2))))),aes(V1,V2),colour='red')
#MSE optimised around 12
mean((trainSet$New.Bodyload-predict(PLSOpt,ncomp=12,trainSet))^2)
#109.8898
#MSE for predictions made on test set
mean((testSet$New.Bodyload-predict(PLSOpt,ncomp=12,testSet))^2)
#120.5699

summary(trainSet$New.Bodyload-predict(PLSOpt,trainSet,ncomp=12))
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#-49.250  -3.650  -1.088   0.000   2.005 188.700 
summary(testSet$New.Bodyload-predict(PLSOpt,testSet,ncomp=12))
#Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
#-42.1100  -3.9640  -1.1670  -0.3148   1.8470 179.9000 

runDiagnostics(PLSOpt,trainSet)
#---------------------------------Partial Least Squares Regression-------------------------------#


#Throwing the kitchen sink! GBM is a non-linear model that uses some advanced maths (picking functions that maximise gradient)
#to optimise predictions. Don't understand much of the underlying concept but have seen them used before
#Try a simple GBM to get some relative importance figures

simpleGBM=gbm(New.Bodyload ~ .,data=trainSet[,numericCols],n.trees=1000,interaction.depth = 3,shrinkage = 0.001,verbose=TRUE)
best.iter <- gbm.perf(simpleGBM,method="OOB")
simpleGBM$n.trees=best.iter
summary(simpleGBM) 
#                                                                      var      rel.inf
#Distance                                                         Distance 59.687548869
#Impacts.Zone.2                                             Impacts.Zone.2 20.054202722
#Impacts.Zone.3                                             Impacts.Zone.3  9.945122613
#Duration                                                         Duration  8.066492708
#Impacts.Zone.4                                             Impacts.Zone.4  1.175635824
#Metabolic.Load.Time.Zone.1                     Metabolic.Load.Time.Zone.1  0.957395481
#Collisions                                                     Collisions  0.105202255
#Impacts.Zone.6                                             Impacts.Zone.6  0.008399527




#try and plot new.bodyload for an individual player
plot(anonymisedData[anonymisedData$Athlete=='21',c('New.Bodyload')],type='l')



View(outlierRemoved[,which(names(outlierRemoved)%in%c('Athlete','Metabolic.Load..relative.','Distance','Equivalent.Distance','Metabolic.Load..absolute.','RelativeVar'))])
outlierRemoved$RelativeVar=outlierRemoved$Metabolic.Load..absolute./outlierRemoved$Metabolic.Load..relative.
outlierRemoved[is.na(outlierRemoved$RelativeVar)|!is.finite(outlierRemoved$RelativeVar),'RelativeVar']=mean(outlierRemoved[!is.na(outlierRemoved$RelativeVar)&is.finite(outlierRemoved$RelativeVar),'RelativeVar'])
summary(outlierRemoved$RelativeVar)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#49.00   65.14   70.88   73.28   80.48  133.00 

#---------------------------Bayesian network modelling-------------------------------------#

#---------------------------Bayesian network modelling-------------------------------------#



#-------------------------------Forward subset selection----------------------------------------#



#Can't progress until correlation structure is found
library('rpart')
library('rattle')
library('rpart.plot')
ctrl = rpart.control(maxdepth=6)
treeModel=rpart('New.Bodyload~.',trainSet[,append(numericCols,'Drill')],control = rpart.control(cp = 0.005,maxdepth = 5),method = "anova")
summary(treeModel)
fancyRpartPlot(treeModel)	
tmp=printcp(treeModel)

sum(anonymisedData$Work.Rate.Interval.Count==anonymisedData$Work.Rate.Interval.Duration)
#two columns are exactly equal,just keep count
anonymisedData$Work.Rate.Interval.Duration=NULL
#Can't progress until correlation structure is found
vars=setdiff(names(trainSet),c('Session','Date','Start.Time','Day.Code','Participation','Zone','Drill'))
forwardSelection=regsubsets(New.Bodyload~.,data = trainSet[,numericCols], nvmax = 8,method='forward')
x=summary(forwardSelection)
forwardSelection$first
linearModel=lm('New.Bodyload~Duration+I(Duration^2)+I(Duration^3)+Impacts.Zone.6+I(Max.Speed^6)+I(Accelerations.Zone.1^2)', trainSet[,numericCols])
summary(linearModel)
names(trainSet)[which(!vars%in%names(trainSet))]
trainSet$predictions=predict.lm(linearModel,trainSet[,numericCols])
mean((trainSet$New.Bodyload-trainSet$predictions)^2)
plot(trainSet$New.Bodyload[c(1:200)],type='l',cex=0.1)
lines(trainSet$predictions[c(1:200)],col="brown",cex=0.1)



lasso.mod=glmnet(as.matrix(trainSet[,setdiff(numericCols,'New.Bodyload')]),trainSet$New.Bodyload,alpha=1)
plot(lasso.mod,label=TRUE)
coef(lasso.mod,s=10)


for(name in names(trainSet[,numericCols])){
  n <- readline(prompt="Enter an integer: ")
  if(n==0){
    break
  }
  else{
    print(paste('plotting ',name,' vs. new.bodyload'))
    plot(trainSet$Metabolic.Power.Average,trainSet$New.Bodyload,cex=0.1,main=paste('plotting ',name,' vs. new.bodyload'))
  }
}

#------------------------------------K Nearest Neighbors-------------------------------------#

controlParams=trainControl(method = "cv", number = 10)
grid=expand.grid(.k=seq(3,55,2))
Knn_fit <- train(New.Bodyload~., data = trainSet[,numericCols],
                    method = "knn",
                    preProcess = c("center", "scale"),
                    tuneGrid = grid,
                    metric='RMSE',
                    trControl = controlParams)

#Optimal value for k seems to be 11
plot(Knn_fit,main='KNN cross-validation plot for choosing k')

knnRegression=knn.reg(train=trainSet[,numericCols], y=trainSet$New.Bodyload, k = 11, algorithm='brute')
knnRegression$R2Pred
#0.8520155
MSE(knnRegression)
#317.0008

knnRegressionTest=knn.reg(train=testSet[,numericCols], y=testSet$New.Bodyload, k = 11, algorithm='brute')
MSE(knnRegressionTest)
#410.6426



#------------------------------------K Nearest Neighbors-------------------------------------#


















