#install.packages(xTable)
library(Hmisc)
library(Metrics)
library(xtable)
library(analogue)
library(pls)
library(dplyr)

#Read data from files
i2011 = read.csv("gt_2011.csv")
i2012 = read.csv("gt_2012.csv")
i2013 = read.csv("gt_2013.csv")
i2014 = read.csv("gt_2014.csv")
i2015 = read.csv("gt_2015.csv")

#drop CO
d2011 = subset(i2011, select = -c(CO))
d2012 = subset(i2012, select = -c(CO))
d2013 = subset(i2013, select = -c(CO))
d2014 = subset(i2014, select = -c(CO))
d2015 = subset(i2015, select = -c(CO))

#aggregate all data
dall = rbind(d2011,d2012,d2013,d2014,d2015) 

s2011 = summary(d2011)
s2012 = summary(d2012)
s2013 = summary(d2013)
s2014 = summary(d2014)
s2015 = summary(d2015)
sdall = summary(dall)

#means
m2011 = colMeans(d2011)
m2012 = colMeans(d2012)
m2013 = colMeans(d2013)
m2014 = colMeans(d2014)
m2015 = colMeans(d2015)
mdall = colMeans(dall)

#sd
sd2011 = apply(d2011,2,sd)
sd2012 = apply(d2012,2,sd)
sd2013 = apply(d2013,2,sd)
sd2014 = apply(d2014,2,sd)
sd2015 = apply(d2015,2,sd)
sddall = apply(dall,2,sd)

#min
min2011 = apply(d2011,2,min)
min2012 = apply(d2012,2,min)
min2013 = apply(d2013,2,min)
min2014 = apply(d2014,2,min)
min2015 = apply(d2015,2,min)
mindall = apply(dall,2,min)

#max
max2011 = apply(d2011,2,max)
max2012 = apply(d2012,2,max)
max2013 = apply(d2013,2,max)
max2014 = apply(d2014,2,max)
max2015 = apply(d2015,2,max)
maxdall = apply(dall,2,max)

#range
r2011 = max2011 - min2011
r2012 = max2012 - min2012
r2013 = max2013 - min2013
r2014 = max2014 - min2014
r2015 = max2015 - min2015
rdall = maxdall - mindall

#extremes: 1% and 99% percentile
e2011 = apply(d2011,2,quantile, probs = c(0.01,0.99))
e2012 = apply(d2012,2,quantile, probs = c(0.01,0.99))
e2013 = apply(d2013,2,quantile, probs = c(0.01,0.99))
e2014 = apply(d2014,2,quantile, probs = c(0.01,0.99))
e2015 = apply(d2015,2,quantile, probs = c(0.01,0.99))
edall = apply(dall,2,quantile, probs = c(0.01,0.99))

#spearman correlations -- sc201x$r for correlation, sc201x$p for p-values
sc2011 = rcorr(as.matrix(d2011),type =c("spearman"))
sc2012 = rcorr(as.matrix(d2012),type =c("spearman"))
sc2013 = rcorr(as.matrix(d2013),type =c("spearman"))
sc2014 = rcorr(as.matrix(d2014),type =c("spearman"))
sc2015 = rcorr(as.matrix(d2015),type =c("spearman"))
scdall = rcorr(as.matrix(dall),type =c("spearman"))

#difference between sc2011 and sc2015
sc2011_2015 = sc2015$r - sc2011$r

#Group statistics
t2011 <- data.frame(m2011,sd2011,min2011,max2011, r2011,e2011[1,],e2011[2,])
colnames(t2011) <- c("mean", "sd", "min", "max","range","1%","99%")
t2012 <- data.frame(m2012,sd2012,min2012,max2012, r2012,e2012[1,],e2012[2,])
colnames(t2012) <- c("mean", "sd", "min", "max","range","1%","99%")
t2013 <- data.frame(m2013,sd2013,min2013,max2013, r2013,e2013[1,],e2013[2,])
colnames(t2013) <- c("mean", "sd", "min", "max","range","1%","99%")
t2014 <- data.frame(m2014,sd2014,min2014,max2014, r2014,e2014[1,],e2014[2,])
colnames(t2014) <- c("mean", "sd", "min", "max","range","1%","99%")
t2015 <- data.frame(m2015,sd2015,min2015,max2015, r2015,e2015[1,],e2015[2,])
colnames(t2015) <- c("mean", "sd", "min", "max","range","1%","99%")
tdall <- data.frame(mdall,sddall,mindall,maxdall, rdall,edall[1,],edall[2,])
colnames(tdall) <- c("mean", "sd", "min", "max","range","1%","99%")

#difference between t2011 and t2015
t2011_2015 <- t2015 - t2011

#print results automatically to a latex format
print(xtable(t2011, type = "latex"), file = "t2011.tex")
print(xtable(t2012, type = "latex"), file = "t2012.tex")
print(xtable(t2013, type = "latex"), file = "t2013.tex")
print(xtable(t2014, type = "latex"), file = "t2014.tex")
print(xtable(t2015, type = "latex"), file = "t2015.tex")
print(xtable(t2011_2015, type = "latex"), file = "t2011_2015.tex")
print(xtable(tdall, type = "latex"), file = "tdall.tex")

#print correlation matrices
print(xtable(sc2011$r, type = "latex"), file = "sc2011.tex")
print(xtable(sc2012$r, type = "latex"), file = "sc2012.tex")
print(xtable(sc2013$r, type = "latex"), file = "sc2013.tex")
print(xtable(sc2014$r, type = "latex"), file = "sc2014.tex")
print(xtable(sc2015$r, type = "latex"), file = "sc2015.tex")
print(xtable(sc2011_2015, type = "latex"), file = "sc2011_2015.tex")
print(xtable(scdall$r, type = "latex"), file = "scdall.tex")

#Linear regression (Phase 1)
trainingSet = rbind(d2011,d2012)
validationSet = d2013
testSet = rbind(d2014,d2015)

#-------------------------------------------

#train model on trainingSet, use all other features as regressors\independent variables
s = getDescStat(trainingSet)
ntrainingSet = meanNormalize(s, trainingSet) 

lm1 = lm(NOX ~ AT + AP + AH + AFDP + GTEP + TIT + TAT + TEY + CDP, data = ntrainingSet)
modelSummary1 = summary(lm1)
Rsquared1 = modelSummary1$r.squared
#spearman correlations of features with respect to NOX
sc_NOX1 = cor(ntrainingSet, method = "spearman")["NOX",]

nvalidationSet = meanNormalize(s, validationSet) 

#apply model to predict NOX of validationSet
prediction1 = predict(lm1, nvalidationSet)
mae1 = mae(nvalidationSet$NOX, prediction1)
corScore1 = cor(prediction1,nvalidationSet$NOX, method ="spearman")

#-------------------------------------------

#train model on combined trainingSet and validationSet, use all other features as regressors\independent variables
training_validationSet = rbind(trainingSet,validationSet)

#normalize
s = getDescStat(training_validationSet)
ntraining_validationSet = meanNormalize(s, training_validationSet) 

lm2 = lm(NOX ~ AT + AP + AH + AFDP + GTEP + TIT + TAT + TEY + CDP, data = ntraining_validationSet)
modelSummary2 = summary(lm2)
Rsquared2 = modelSummary2$r.squared
#spearman correlations of features with respect to NOX
sc_NOX2 = cor(ntraining_validationSet, method = "spearman")["NOX",]

ntestSet = meanNormalize(s, testSet)

#apply model to predict NOX of testSet
prediction2 = predict(lm2, ntestSet)
mae2 = mae(ntestSet$NOX, prediction2)
corScore2 = cor(prediction2,ntestSet$NOX, method ="spearman")

#-------------------------------------------

#Mean normalize the data based on provided descriptive statistics 
getDescStat <- function(data)
{
  means = colMeans(data)
  min = apply(data,2,min)
  max = apply(data,2,max)
  min_max = max - min
  #can't figure out how to do this with a build in command, so just hardcode :)
  
  res <- data.frame(means,min_max)
  colnames(res) <- c("mean", "min_max")
  res
}


#Mean normalize the data based on provided descriptive statistics 
meanNormalize <- function(descStat, data)
{
  means = descStat$mean
  minmax = descStat$min_max
  #hardcoding this because I don't know a nice way to do it
  nAT = (data$AT - means[1]) / minmax[1]
  nAP = (data$AP - means[2]) / minmax[2]
  nAH = (data$AH - means[3]) / minmax[3]
  nAFDP = (data$AFDP - means[4]) / minmax[4]
  nGTEP = (data$GTEP - means[5]) / minmax[5]
  nTIT = (data$TIT - means[6]) / minmax[6]
  nTAT = (data$TAT - means[7]) / minmax[7]
  nTEY = (data$TEY - means[8]) / minmax[8]
  nCDP = (data$CDP - means[9]) / minmax[9]
  nNOX = (data$NOX - means[10]) / minmax[10]
  
  normalized <- data.frame(nAT,nAP,nAH,nAFDP, nGTEP,nTIT,nTAT,nTEY,nCDP,data$NOX)
  colnames(normalized) <- c("AT", "AP", "AH", "AFDP","GTEP","TIT","TAT","TEY","CDP", "NOX")
  normalized
}

features2 <- list()
features2[["1"]] <- CDP*AFDP
features2[["2"]] <- TEY*TAT
features2[["3"]] <- AT*TIT
features2[["4"]] <- AT*TEY
features2[["5"]] <- AP*AFDP
features2[["6"]] <- AP
features2[["7"]] <- TIT / TAT / AT
features2[["8"]] <- AT 
features2[["9"]] <- GTEP-CDP-AP
features2[["10"]] <- AFDP / AH
features2[["NOX"]] = NOX

selectedfeaturecorrelations = rcorr(as.matrix(as.data.frame(features2)),)$r[,"NOX"]

#-------------------------------------------
#Best feature transformation function
transformFeatures <- function(data)
{
  # list of brute forced features
  # selected ones are uncommented
  AT = data$AT
  AP = data$AP
  AH = data$AH
  AFDP = data$AFDP
  GTEP = data$GTEP
  TIT = data$TIT
  TAT = data$TAT
  CDP = data$CDP
  TEY = data$TEY
  NOX = data$NOX
  features <- list()
  #features[["0"]] <- AFDP*CDP*TIT
  #features[["1"]] <- sqrt(TIT*TIT + AFDP*AFDP + TAT*TAT)
  #features[["2"]] <- (TIT*TIT + AFDP*AFDP)
  #features[["3"]] <- TIT*AFDP
  #features[["4"]] <- TEY*AFDP
  #features[["9"]] <- CDP*TAT*TIT
  #features[["10"]] <- log2(CDP*AFDP*TIT)
  #features[["11"]] <- sqrt(AT*AT+TIT*TIT+TAT*TAT)
  #features[["12"]] <- AT*AP
  #features[["13"]] <- AH*AT
  #[["14"]] <- GTEP*TAT
  #features[["15"]] <- TAT*TEY
  #features[["16"]] <- CDP*TEY
  #features[["17"]] <- TAT*CDP
  #features[["18"]] <- AP*AP
  #features[["19"]] <- AFDP*AFDP
  #features[["20"]] <- sqrt(AT)
  #features[["21"]] <- sqrt(TIT)
  #features[["23"]] <- sqrt(TAT)
  #features[["24"]] <- sqrt(TEY)
  #features[["25"]] <- sqrt(CDP)
  #features[["26"]] <- log(AFDP)
  #features[["27"]] <- log(TIT)
  #features[["28"]] <- log(TAT)
  #features[["29"]] <- log(CDP)
  #features[["30"]] <- AT*AFDP
  #features[["35"]] <- AT*TIT
  #features[["36"]] <- AT*GTEP
  #features[["37"]] <- AT*TIT*GTEP
  #features[["38"]] <- AT*AT
  #features[["40"]] <- log2(CDP)
  #features[["41"]] <- log2(TAT)
  #features[["42"]] <- log2(AT)
  #features[["43"]] <- TIT / TAT
  #features[["45"]] <- TIT / AT
  #features[["46"]] <- TAT / AT
  #features[["47"]] <- AT / CDP
  #features[["48"]] <- AT / AH
  #features[["55"]] <- AFDP
  #features[["56"]] <- AH
  #features[["53"]] <- TIT-TAT
  #features[["49"]] <- AH / AP
  #features[["6"]] <- CDP*TIT
  #features[["7"]] <- TEY*TIT
  #features[["52"]] <- AT-TAT
  #features[["33"]] <- AT*CDP
  features[["1"]] <- CDP*AFDP
  features[["2"]] <- TEY*TAT
  features[["3"]] <- AT*TIT
  features[["4"]] <- AT*TEY
  features[["5"]] <- AP*AFDP
  features[["6"]] <- AP
  #features[["7"]] <- TIT / TAT / AT
  features[["8"]] <- AT 
  features[["9"]] <- GTEP-CDP-AP
  #features[["10"]] <- AFDP / AH
  features[["NOX"]] = NOX
  
  
  #general feature selection
  correlationResults = rcorr(as.matrix(as.data.frame(features)),)$r[,"NOX"]
  #sort them based on highest correlation
  correlationRanking = sort(abs(correlationResults),decreasing = TRUE)
  
  #take top 20 features + NOX
  selectedFeatures = tail(correlationRanking,21)
  names(selectedFeatures)
  
  #resultFeatures <- data.frame(NOX,feature1,feature2, etcetc)
  #colnames(resultFeatures) <- c("NOX", "feature1", "feature2", "etcetc")
  
  #Add NOX back to feature list
  
  #placeholder feature return:
  resultFeatures = as.data.frame(features)
}

#-------------------------------------------

#train model based on a subset of the features --- the features as selected by Kaya et al.

#train best model on trainingSet

#normalize
s = getDescStat(trainingSet)
ntrainingSet = meanNormalize(s, trainingSet) 

blmData = transformFeatures(ntrainingSet) #transform original data into new features
blm = lm(NOX ~., data = blmData) 
modelSummaryb = summary(blm)
Rsquaredb = modelSummaryb$r.square
#spearman correlations of features with respect to NOX
sc_NOXb = cor(blmData, method = "spearman")["NOX",]

#apply model to predict NOX of validationSet

#normalize
nvalidationSet = meanNormalize(s, validationSet) 

predictionb = predict(blm, transformFeatures(nvalidationSet))
maeb1 = mae(nvalidationSet$NOX, predictionb)
corScoreb1 = cor(predictionb,nvalidationSet$NOX, method ="spearman")

#------------------------------

#train best model on trainingSet + validationSet
#normalize
s = getDescStat(rbind(trainingSet,validationSet))
ntrainingvalidationSet = meanNormalize(s, rbind(trainingSet,validationSet)) 

blmData2 = transformFeatures(ntrainingvalidationSet) #transform original data into new features
blm2 = lm(NOX ~., data = blmData2) 
modelSummaryb2 = summary(blm2)
Rsquaredb2 = modelSummaryb2$r.square
#spearman correlations of features with respect to NOX
sc_NOXb2 = cor(blmData2, method = "spearman")["NOX",]

#normalize
ntestSet = meanNormalize(s, testSet) 

#apply model to predict NOX of testSet
predictionb2 = predict(blm2, transformFeatures(ntestSet))
maeb2 = mae(ntestSet$NOX, predictionb2)
corScoreb2 = cor(predictionb2,ntestSet$NOX, method ="spearman")


#-------------------------------------------

#Phase 3
#divide validationData into 10 blocks
n <- 10
blocks = split(validationSet, factor(sort(rank(row.names(validationSet))%%n)))

basepredictions <- list()
basePerformanceMAE <- list()
basePerformanceR2 <- list()
basePerformanceCorScore <- list()
basePerformanceSC <- list()  #spearman correlation of our features in predicted data with respect to NOX
basePerformanceSCP <- list() #spearman correlation p-values

bestpredictions <- list()
bestPerformanceMAE <- list() 
bestPerformanceR2 <- list()
bestPerformanceCorScore <- list()
bestPerformanceSC <- list()
bestPerformanceSCP <- list()

performanceDifMAE <- list()

data = trainingSet
for(i in 0:9)
{
  li = paste('',i, sep='')
  
  #calculateDescriptive statistics over our original data for normalization
  descStat = getDescStat(data)
  normalizedData = meanNormalize(descStat, data)
  
  
  #train baseline model
  baseData = normalizedData #use original data as the feature list
  baselm = lm(NOX ~., data = baseData)
  
  #predict block using baseline model
  baseBlockData = meanNormalize(descStat,blocks[[li]]) #use original block data as the features
  
  basepredictions[[li]] <- predict(baselm, baseBlockData)
  basePerformanceMAE[[li]] <- mae(baseBlockData$NOX, basepredictions[[li]])
  basePerformanceR2[[li]] <- summary(baselm)$r.squared
  basePerformanceCorScore[[li]] <- cor(baseBlockData$NOX,basepredictions[[li]], method ="spearman")
  
  #train best model
  bestData = transformFeatures(normalizedData) #transform original data into new features
  bestlm = lm(NOX ~., data = bestData) 
  
  #predict block using best model
  bestBlockData = transformFeatures(meanNormalize(descStat,blocks[[li]])) 
  
  bestpredictions[[li]] <- predict(bestlm, bestBlockData)
  bestPerformanceMAE[[li]] <- mae(bestBlockData$NOX, bestpredictions[[li]])
  bestPerformanceR2[[li]] <- summary(bestlm)$r.squared
  bestPerformanceCorScore[[li]] <- cor(bestBlockData$NOX,bestpredictions[[li]], method ="spearman")
  
  #record performance dif in terms of Mean Absolute Error
  performanceDifMAE[[li]] <- bestPerformanceMAE[[li]] - basePerformanceMAE[[li]]
  
  #add block into data
  data = rbind(data, blocks[[li]])
}

resultTable = data.frame(unlist(basePerformanceMAE),unlist(bestPerformanceMAE), unlist(basePerformanceR2),unlist(bestPerformanceR2), unlist(basePerformanceCorScore),unlist(bestPerformanceCorScore))

tTestMAE = t.test(unlist(basePerformanceMAE),unlist(bestPerformanceMAE))
tTestR2 = t.test(unlist(basePerformanceR2),unlist(bestPerformanceR2))
tTestCorScore = t.test(unlist(basePerformanceCorScore),unlist(bestPerformanceCorScore))

#compare prediction of the entire set (done in an online style) with the original method where we predict the entire year
combinedBasePredictions = flatten(basepredictions)
combinedBasePredictionsMAE = mae(validationSet$NOX,unlist(combinedBasePredictions))
combinedBasePredictionsCorScore = cor(validationSet$NOX,unlist(combinedBasePredictions),method ="spearman")
basePredictionDif = mae1 - combinedBasePredictionsMAE #we can see we have a lower MAE
basePredictionDifCorScore = corScore1 - combinedBasePredictionsCorScore 

combinedBestPredictions = flatten(bestpredictions)
combinedBestPredictionsMAE = mae(validationSet$NOX,unlist(combinedBestPredictions))
combinedBestPredictionsCorScore = cor(validationSet$NOX,unlist(combinedBestPredictions),method ="spearman")
bestPredictionDif = maeb1 - combinedBestPredictionsMAE
bestPredictionDifCorScore = corScoreb1 - combinedBestPredictionsCorScore 


#----------------------------------------------------------------------------------------

#Repeat the whole shebang for step 6

#divide testData into 20 blocks
n <- 20
blocks = split(testSet, factor(sort(rank(row.names(testSet))%%n)))

basepredictions <- list()
basePerformanceMAE <- list()
basePerformanceR2 <- list()
basePerformanceCorScore <- list()
basePerformanceSC <- list()  #spearman correlation of our features in predicted data with respect to NOX
basePerformanceSCP <- list() #spearman correlation p-values

bestpredictions <- list()
bestPerformanceMAE <- list() 
bestPerformanceR2 <- list()
bestPerformanceCorScore <- list()
bestPerformanceSC <- list()
bestPerformanceSCP <- list()

performanceDifMAE <- list()

data = rbind(trainingSet,validationSet)
for(i in 0:19)
{
  li = paste('',i, sep='')
  
  #calculateDescriptive statistics over our original data for normalization
  descStat = getDescStat(data)
  normalizedData = meanNormalize(descStat, data)
  
  
  #train baseline model
  baseData = normalizedData #use original data as the feature list
  baselm = lm(NOX ~., data = baseData)
  
  #predict block using baseline model
  baseBlockData = meanNormalize(descStat,blocks[[li]]) #use original block data as the features
  
  basepredictions[[li]] <- predict(baselm, baseBlockData)
  basePerformanceMAE[[li]] <- mae(baseBlockData$NOX, basepredictions[[li]])
  basePerformanceR2[[li]] <- summary(baselm)$r.squared
  basePerformanceCorScore[[li]] <- cor(baseBlockData$NOX,basepredictions[[li]], method ="spearman")
  
  #train best model
  bestData = transformFeatures(normalizedData) #transform original data into new features
  bestlm = lm(NOX ~., data = bestData) 
  
  #predict block using best model
  bestBlockData = transformFeatures(meanNormalize(descStat,blocks[[li]])) 
  
  bestpredictions[[li]] <- predict(bestlm, bestBlockData)
  bestPerformanceMAE[[li]] <- mae(bestBlockData$NOX, bestpredictions[[li]])
  bestPerformanceR2[[li]] <- summary(bestlm)$r.squared
  bestPerformanceCorScore[[li]] <- cor(bestBlockData$NOX,bestpredictions[[li]], method ="spearman")
  
  #record performance dif in terms of Mean Absolute Error
  performanceDifMAE[[li]] <- bestPerformanceMAE[[li]] - basePerformanceMAE[[li]]
  
  #add block into data
  data = rbind(data, blocks[[li]])
}

#not sure how to do a t-test to determine a performance difference on the sc correlations
#but I suppose, for the MAE and R2 it would be:

resultTableTestSet = data.frame(unlist(basePerformanceMAE),unlist(bestPerformanceMAE), unlist(basePerformanceR2),unlist(bestPerformanceR2), unlist(basePerformanceCorScore),unlist(bestPerformanceCorScore))

testSet_tTestMAE = t.test(unlist(basePerformanceMAE),unlist(bestPerformanceMAE))
testSet_tTestR2 = t.test(unlist(basePerformanceR2),unlist(bestPerformanceR2)) #is this actually R^2 w
testSet_tTestCorScore = t.test(unlist(basePerformanceCorScore),unlist(bestPerformanceCorScore))

#compare prediction of the entire set (done in an online style) with the original method where we predict the entire year
combinedBasePredictions_ = flatten(basepredictions)
combinedBasePredictionsMAE_ = mae(testSet$NOX,unlist(combinedBasePredictions_))
combinedBasePredictionsCorScore_ = cor(testSet$NOX,unlist(combinedBasePredictions_),method ="spearman")
basePredictionDif_ = mae2 - combinedBasePredictionsMAE_ 
basePredictionDifCorScore_ = corScore2 - combinedBasePredictionsCorScore_

combinedBestPredictions_ = flatten(bestpredictions)
combinedBestPredictionsMAE_ = mae(testSet$NOX,unlist(combinedBestPredictions_))
combinedBestPredictionsCorScore_ = cor(testSet$NOX,unlist(combinedBestPredictions_),method ="spearman")
bestPredictionDif_ = maeb2 - combinedBestPredictionsMAE_
bestPredictionDifCorScore_ = corScoreb1 - combinedBestPredictionsCorScore_

