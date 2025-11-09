#This will be the main program to produce outputs for RF, DT and Logistic Using the training and testing sets which have sampled proportionally from each year

# Clear variables in memory
rm(list=ls())

# Set a seed for reproducibility
set.seed(7406)

library(class)
library(MASS)
library(randomForest)
library(caret)
library(ranger)

# load the preprocessed dataset which is created below
crime = read.csv("SF_Police_Reports_2003_to_2018_cleaned_RF.csv",header=TRUE)
# The code below preprocesses the original dataset for use in RF, tree and logistic regression
# and outputs the resulting csv and has been commented out 
# crime = read.csv("SF_Police_Reports_2003_to_2018_cleaned.csv",header=TRUE)
# crime = as.data.frame(crime) #convert to data frame
# 
# high = c('Kidnapping', 'Sex Offenses, Forcible', 'Weapon Laws', 
#          'Arson', 'Robbery', 'Assault', 'Burglary', 'Vehicle Theft',
#          'Larceny/Theft')
# 
# crime$Crime_Level = 0
# crime$Crime_Level = ifelse(crime$Category %in% high, 1, 0)
# 
# 
# # Convert categorical variables to numeric
# # DayOfWeek: Sunday = 1, Monday = 2, Tuesday = 3, Wednesday = 4, Thursday = 5, Friday = 6, Saturday = 7
# # PdDistrict: Bayview = 1, Central = 2, Ingleside = 3, Mission = 4, Northern = 5, Park = 6, Richmond = 7, Southern = 8, Taraval = 9, Tenderloin = 10
# crime[crime=="Sunday"] = 1
# crime[crime=="Monday"] = 2
# crime[crime=="Tuesday"] = 3
# crime[crime=="Wednesday"] = 4
# crime[crime=="Thursday"] = 5
# crime[crime=="Friday"] = 6
# crime[crime=="Saturday"] = 7
# crime[crime=="Bayview"] = 1
# crime[crime=="Central"] = 2
# crime[crime=="Ingleside"] = 3
# crime[crime=="Mission"] = 4
# crime[crime=="Northern"] = 5
# crime[crime=="Park"] = 6
# crime[crime=="Richmond"] = 7
# crime[crime=="Southern"] = 8
# crime[crime=="Taraval"] = 9
# crime[crime=="Tenderloin"] = 10
# 
# crime$DayOfWeek = as.factor(crime$DayOfWeek)
# crime$PdDistrict = as.factor(crime$PdDistrict)
# 
# write the preprocessed dataset so it does not need to be done repeatedly
# write.csv(crime, "C:\\Users\\amswi\\Desktop\\ISYE 7406 Data Mining and Statistical Learning\\Project\\SF_Police_Reports_2003_to_2018_cleaned_RF.csv", row.names = FALSE)



#Improvement for future: take a sample from each year to make a more manageable dataset
#######################################################
# Random Forest
n = dim(crime)[1]; # total number of observations
#n1 = round(n/40); # number of observations randomly selected for training data ~ 50000
n1 = round(n*0.03); # number of observations randomly selected for training data ~ 60000
n2 = round(n/40); # number of observations randomly selected for training data ~ 50000

#randomly select from each year proportional to the years total percentage of overall incidence, total ~50000
years = c(2003, 2004, 2005, 2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018)
crimeTrain = NULL
crimeTest = NULL

for (i in years){
  crime.temp = crime[crime$Year == i,]
  n.temp = dim(crime.temp)[1]
  n.ratio = n.temp/n
  flag = sort(sample(1:n.temp, n.ratio*n1))
  crime.temp = crime.temp[flag,]
  n.temp = dim(crime.temp)[1]
  flag = sort(sample(1:n.temp, n.ratio*n2))
  crimeTrain = rbind(crimeTrain, crime.temp[flag,])
  crimeTest = rbind(crimeTest, crime.temp[-flag,])
  # print(i)
  # print(length(which(crimeTrain$Year==i))/n2)
  # print(n.ratio)
  # print(dim(crimeTrain)[1]/n2)
  # print(length(which(crimeTest$Year==i))/(n1-n2))
}

# # randomly select 50000 observations as testing data
# flag = sort(sample(1:n, n1))
# crimeTrain = crime[flag,]
# crimeLeft = crime[-flag,]

# newn = dim(crime)[1]; # total number of remaining observations
# n2 = round(newn/202); # number of observations randomly selected for testing data ~ 10000
# flag = sort(sample(1:n, n2))
# crimeTest = crime[flag,] # ~10000 samples for testing

#######################################################
# 
# # create repeatable training and testing set for comparing models before CV and tuning
# flag = seq(1, n, round(n/(n*.005))) #
# #crimeTrain = crime[-flag,]
# #crimeTest = crime[flag,]
# crimeTest = crime[-flag,]
# crimeTrain = crime[flag,]

#Crime_Level ~. -Category-Descript-Resolution-X-Y

#######################################################
# Random Forest Tuning
# tune_grid = expand.grid(mtry = c(5,6,7)) #omit 1,2,3,4 to prevent overfitting
# treeNum = c(100, 200, 500, 800, 1000)
# nodesize = c(1:5)
# 
# tC = trainControl(method = "repeatedcv", number = 10)
# mtryRF = train(as.factor(Crime_Level)~. -Category-Descript-Resolution-X-Y, crimeTrain, method = "rf",
#                metric = "Accuracy", tuneGrid = tune_grid,
#                trControl = tC, importance = TRUE)
# 
# bestmtry = mtryRF$bestTune
# 
# treesList = list()
# nodeList = list()
# for (i in treeNum){
#   treeRF = train(as.factor(Crime_Level)~. -Category-Descript-Resolution-X-Y, crimeTrain, method = "rf",
#                 metric = "Accuracy", tuneGrid = expand.grid(mtry = bestmtry),
#                 trControl = tC, importance = TRUE, ntree = i)
#  colLab = toString(i)
#  treesList[[colLab]] = treeRF
#}

#summary(resamples(treesList)) #
#dotplot(resamples(treesList))

#for (i in nodesize){
#  nodeRF = train(as.factor(Crime_Level)~. -Category-Descript-Resolution-X-Y, crimeTrain, method = "rf",
#                 metric = "Accuracy", tuneGrid = expand.grid(mtry = bestmtry),
#                 trControl = tC, importance = TRUE, ntree = 20, nodesize = i)
#  colLab = toString(i)
#  nodeList[[colLab]] = nodeRF
#}

#summary(resamples(nodeList)) #
#dotplot(resamples(nodeList))


#Improvement for future: take a sample from each year to make a more manageable dataset
#######################################################
# Random Forest

#randomly select from each year proportional to the years total percentage of overall incidence, total ~50000
years = c(2003, 2004, 2005, 2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018)
crimeTrain = NULL
crimeTest = NULL

for (i in years){
  crime.temp = crime[crime$Year == i,]
  n.temp = dim(crime.temp)[1]
  n.ratio = n.temp/n
  flag = sort(sample(1:n.temp, n.ratio*n1))
  crime.temp = crime.temp[flag,]
  n.temp = dim(crime.temp)[1]
  flag = sort(sample(1:n.temp, n.ratio*n2))
  crimeTrain = rbind(crimeTrain, crime.temp[flag,])
  crimeTest = rbind(crimeTest, crime.temp[-flag,])
}

# n = dim(crime)[1]; # total number of observations
# n1 = round(n/40); # number of observations randomly selected for training data ~ 50000
# 
# # randomly select 50000 observations as testing data in each loop
# flag = sort(sample(1:n, n1))
# crimeTrain = crime[flag,]
# crimeLeft = crime[-flag,]
# 
# newn = dim(crime)[1]; # total number of remaining observations
# n2 = round(newn/202); # number of observations randomly selected for testing data ~ 10000
# flag = sort(sample(1:n, n2))
# crimeTest = crime[flag,] # ~10000 samples for testing


# # create repeatable training and testing set for comparing models before CV and tuning
# flag = seq(1, n, round(n/(n*.005))) #
# #crimeTrain = crime[-flag,]
# #crimeTest = crime[flag,]
# crimeTest = crime[-flag,]
# crimeTrain = crime[flag,]


tune_grid = expand.grid(
  mtry = c(3, 4, 5, 6, 7),  # Number of variables to sample, omit 1,2,3 to prevent overfitting
  splitrule = c("gini"), # Splitting rule
  min.node.size = c(1:4) # Minimum node size, omit 5,6,7 to prevent overfitting
)

# Define cross-validation settings
tC = trainControl(
  method = "cv",       # Use k-fold cross-validation
  number = 5,          # Number of folds
  verboseIter = FALSE, # Don't show progress
  savePredictions = "final", # Save predictions for tuned model
)

rf1 = train(
  form = as.factor(Crime_Level) ~ DayOfWeek + PdDistrict + Year + Month + Hour + X + Y, # include X Y?
  data = crimeTrain,
  method = "ranger",
  trControl = tC,
  tuneGrid = tune_grid,
  metric = "Accuracy",
  num.trees = 1000 # was 500  w/ 0.40 accuracy 1000 w/ acc 0.563, 5000 w/ acc ?
)

print(rf1)
plot(rf1)

best_tune = rf1$bestTune
print(paste("Best mtry is", best_tune[1])) # mtry = 4, min.node.size = 3
print(paste("Best min.node.size is", best_tune[3]))

# importance(rfTune, type=2)
# varImpPlot(rfTune)

rfTuned = ranger(formula = as.factor(Crime_Level) ~ DayOfWeek + PdDistrict + Year + Month + Hour + X + Y, # include X Y?
                 data = crimeTrain,
                 num.trees = 1000, # was 500 w/ 0.40 accuracy, 1000 with acc 0.563
                 mtry = c(best_tune[1,1]),
                 min.node.size = c(best_tune[1,3]),
                 importance = "impurity"
                 )
importance(rfTuned) # RF variable importance with regard to Gini index
rfPredTuned = predict(rfTuned, crimeTest[,1:12], type="response")
table(rfPredTuned$predictions, crimeTest$Crime_Level)
mean(rfPredTuned$predictions != crimeTest$Crime_Level) # RF mean error
rfAcc = 1 - mean(rfPredTuned != crimeTest$Crime_Level) # RF testing Accuracy
print(paste("RF Training Accuracy =", rfAcc))

#######################################################
#Decision Tree
library(rpart)
library(tree)
dt = rpart(Crime_Level ~ DayOfWeek + PdDistrict + Year + Month + Hour + X + Y, # include X Y?
           data = crimeTrain,
           method = "class",
           parm = list(split = "gini")
)
print(dt)

minCp = dt$cptable[which.min(dt$cptable[, "xerror"])] # min cp

# tC = trainControl(
#   method = "cv",       # Use k-fold cross-validation
#   number = 5,          # Number of folds
#   verboseIter = FALSE, # Don't show progress
#   savePredictions = "final", # Save predictions for tuned model
# )

# tune_grid = expand.grid(cp = (0.01, .0.05, .1), maxdepth = c(3, 5, 7), minbucket = c(1:4), minsplit = c(4:7))
tC = trainControl(
                  method = "cv",       # Use k-fold cross-validation
                  number = 5,          # Number of folds
                  verboseIter = FALSE, # Don't show progress
                  savePredictions = "final", # Save predictions for tuned model
)
# # Tune cp parameter
# dtTuneCp = train(
#                form = as.factor(Crime_Level) ~ DayOfWeek + PdDistrict + Year + Month + Hour + X + Y, # include X Y?
#                data = crimeTrain,
#                method = "rpart",
#                trControl = tC,
#                tuneGrid = expand.grid(cp = c(0.01, 0.05, 0.1)),
#                metric = "Accuracy",
#                parm = list(split = "gini")
# )
# print(dtTuneCp)

# Tune maxdepth parameter
dtTuneMd = train(
  form = as.factor(Crime_Level) ~ DayOfWeek + PdDistrict + Year + Month + Hour + X + Y, # include X Y?
  data = crimeTrain,
  method = "rpart2",
  trControl = tC,
  tuneGrid = expand.grid(maxdepth = c(3:7)),
  metric = "Accuracy",
  parm = list(split = "gini"),
  cp = minCp #dtTuneCp$bestTune
)
print(dtTuneMd$bestTune)

dtFinal = rpart(Crime_Level ~ DayOfWeek + PdDistrict + Year + Month + Hour + X + Y, # include X Y?
                data = crimeTrain,
                method = "class",
                parm = list(split = "gini"),
                cp = minCp, #dtTuneCp$bestTune,
                maxdepth = dtTuneMd$bestTune
)

# Training Accuracy
dtPredTrain = predict(dtFinal, crimeTrain[,1:12], type=c("class"), verbose = FALSE)
dtTrainAcc = 1 - mean(dtPredTrain != crimeTest$Crime_Level) # DT training Accuracy
print(paste("DT Training Accuracy =", dtTrainAcc))

#Testing Accuracy
dtPred = predict(dtFinal, crimeTest[,1:12], type=c("class"), verbose = FALSE) #c(4,5,7,8,9,10,11)
table(dtPred, crimeTest$Crime_Level)
mean(dtPred != crimeTest$Crime_Level) # mean error of decison tree
dtAcc = 1 - mean(dtPred != crimeTest$Crime_Level) # DT testing Accuracy
print(paste("DT Testing Accuracy =", dtAcc))

# pruning HAD NO EFFECT
# dtPrune = prune(dt, cp=0.01)
# 
# #Testing Accuracy after pruning
# dtPred2 = predict(dtPrune, crimeTest[,1:12], type=c("class"), verbose = FALSE) #c(4,5,7,8,9,10,11)
# table(dtPred2, crimeTest$Crime_Level)
# mean(dtPred2 != crimeTest$Crime_Level)
# sum(diag(table(dtPred2, crimeTest$Crime_Level)))/sum(table(dtPred2, crimeTest$Crime_Level))



#######################################################
#Linear Model

# library(glmnet)
# 
glm = glm(as.factor(Crime_Level) ~ DayOfWeek + PdDistrict + Year + Month + Hour + X + Y, # include X Y?
           family = binomial(link = "logit"),
           data = crimeTrain,
           trace = FALSE
           )
# ## Testing Error of logisitic regression
# lmTe = mean(predict(lm,crimeTest[,1:12]) != crimeTest[,13])
# print(paste("Linear Testing Error =", lmTe))

lm = lm(Crime_Level ~ DayOfWeek + PdDistrict + Year + Month + Hour + X + Y, # include X Y?
        data = crimeTrain
        )

# Stepwise variable selection to reduce variables
stepMod  = step(lm,
                trace = FALSE
                )

# Coefficents of stepMod
round(coef(stepMod),3)
summary(stepMod)

# # Stepwise model training  and  testing errors 
# MSEstepTrain = mean(resid(stepMod)^2)
stepPred = predict(stepMod, crimeTest[,1:12])
# MSEstepTest =   mean((stepPred - crimeTest[,13])^2)

# Accuracy of Step Model
stepPred = round(stepPred) #convert output to binary using 0.5 as round up threshold
table(stepPred, crimeTest$Crime_Level)
mean(stepPred != crimeTest$Crime_Level) # LM mean error
lmAcc = sum(diag(table(stepPred, crimeTest$Crime_Level)))/sum(table(stepPred, crimeTest$Crime_Level)) # LM Testing Accuracy
print(paste("Testing Accuracy =", lmAcc))

#######################################################

##END