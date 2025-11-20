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
# crime = read.csv("SF_Police_Reports_2003_to_2018_cleaned_RF.csv",header=TRUE)
# The code below preprocesses the original dataset for use in RF, tree and logistic regression
# and outputs the resulting csv
crime = read.csv("SF_Police_Reports_2003_to_2018_cleaned.csv",header=TRUE)
crime = as.data.frame(crime) #convert to data frame

high = c('Kidnapping', 'Sex Offenses, Forcible', 'Weapon Laws',
         'Arson', 'Robbery', 'Assault', 'Burglary', 'Vehicle Theft',
         'Larceny/Theft')

crime$Crime_Level = 0
crime$Crime_Level = ifelse(crime$Category %in% high, 1, 0)


# Convert categorical variables to numeric
# DayOfWeek: Sunday = 1, Monday = 2, Tuesday = 3, Wednesday = 4, Thursday = 5, Friday = 6, Saturday = 7
# PdDistrict: Bayview = 1, Central = 2, Ingleside = 3, Mission = 4, Northern = 5, Park = 6, Richmond = 7, Southern = 8, Taraval = 9, Tenderloin = 10
crime[crime=="Sunday"] = 1
crime[crime=="Monday"] = 2
crime[crime=="Tuesday"] = 3
crime[crime=="Wednesday"] = 4
crime[crime=="Thursday"] = 5
crime[crime=="Friday"] = 6
crime[crime=="Saturday"] = 7
crime[crime=="Bayview"] = 1
crime[crime=="Central"] = 2
crime[crime=="Ingleside"] = 3
crime[crime=="Mission"] = 4
crime[crime=="Northern"] = 5
crime[crime=="Park"] = 6
crime[crime=="Richmond"] = 7
crime[crime=="Southern"] = 8
crime[crime=="Taraval"] = 9
crime[crime=="Tenderloin"] = 10

crime$DayOfWeek = as.factor(crime$DayOfWeek)
crime$PdDistrict = as.factor(crime$PdDistrict)

# write the preprocessed dataset so it does not need to be done repeatedly
# write.csv(crime, <path>, row.names = FALSE)

# add interaction variables
crime$X2 = (crime$X)^2
crime$Y2 = (crime$Y)^2
crime$XY = (crime$X)*(crime$Y)
crime$XHour = (crime$X)*(crime$Hour)
crime$YHour = (crime$Y)*(crime$Hour)

#######################################################
# Random Forest
n = dim(crime)[1]; # total number of observations
n1 = round(n*0.03/5); # number of observations randomly selected for training and testing data ~ 12000
n2 = round(n/200); # number of observations randomly selected for training data ~ 10000

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

#######################################################
# Random Forest

tune_grid = expand.grid(
  mtry = c(3:8),  # Number of variables to sample, omit 1,2 to prevent overfitting
  splitrule = c("gini"), # Splitting rule
  min.node.size = c(1:6) # Minimum node size, omit 7,8 to prevent overfitting
)

# Define cross-validation settings
tC = trainControl(
  method = "cv",       # Use k-fold cross-validation
  number = 5,          # Number of folds
  verboseIter = FALSE, # Don't show progress
  savePredictions = "final", # Save predictions for tuned model
)

rf1 = train(
  form = as.factor(Crime_Level) ~ X + Y + Month + DayOfWeek + Y2 + XY + XHour + YHour,#DayOfWeek + PdDistrict + Year + Month + Hour + X + Y, # include X Y?
  data = crimeTrain,
  method = "ranger",
  trControl = tC,
  tuneGrid = tune_grid,
  metric = "Accuracy",
  num.trees = 1000 # was 500  w/ 0.40 accuracy 1000 w/ acc 0.563, 5000 w/ acc ?
)

#print(rf1)
plot(rf1) # Var Sel Plot 1

best_tune = rf1$bestTune
print(paste("Best mtry is", best_tune[1]))
print(paste("Best min.node.size is", best_tune[3]))

# importance(rf1, type=2) # Only works with random forest, not ranger
# varImpPlot(rf1) # Only works with random forest, not ranger

rfTuned = ranger(formula = as.factor(Crime_Level) ~ X + Y + Month + DayOfWeek + Y2 + XY + XHour + YHour,#DayOfWeek + PdDistrict + Year + Month + Hour + X + Y, # include X Y?
                 data = crimeTrain,
                 num.trees = 1000, # was 500 w/ 0.40 accuracy, 1000 with acc 0.563
                 mtry = c(best_tune[1,1]),
                 min.node.size = c(best_tune[1,5]),
                 importance = "impurity"
)
importance(rfTuned) # RF variable importance with regard to Gini index
rfPredTuned = predict(rfTuned, crimeTest[,c(1:12, 14:18)], type="response")
table(rfPredTuned$predictions, crimeTest$Crime_Level)
mean(rfPredTuned$predictions != crimeTest$Crime_Level) # RF mean error
rfAcc = 1 - mean(rfPredTuned$predictions != crimeTest$Crime_Level) # RF testing Accuracy
print(paste("RF Training Accuracy =", rfAcc))

#######################################################
#Decision Tree
library(rpart)
library(tree)
dt = rpart(Crime_Level ~ PdId + DayOfWeek + PdDistrict + X + Y + Year + Month + Hour + Season + DayOfWeek + X2 + Y2 + XY + XHour + YHour,#X + Y + Month + DayOfWeek + Y2 + XY + XHour + YHour, #DayOfWeek + Year + Month + Hour + X + Y, #Vars picked by testing in RF-DT-LR Var Sel Test #DayOfWeek + PdDistrict + Year + Month + Hour + X + Y, # include X Y?
           data = crimeTrain,
           method = "class",
           parm = list(split = "gini")
)
#print(dt)
plotcp(dt)
minCp = dt$cptable[which.min(dt$cptable[, "xerror"])] # min cp

tC = trainControl(
                  method = "cv",       # Use k-fold cross-validation
                  number = 10,          # Number of folds
                  verboseIter = FALSE, # Don't show progress
                  savePredictions = "final", # Save predictions for tuned model
)

# Tune maxdepth parameter
dtTuneMd = train(
  form = as.factor(Crime_Level) ~ PdId + DayOfWeek + PdDistrict + X + Y + Year + Month + Hour + Season + DayOfWeek + X2 + Y2 + XY + XHour + YHour,#X + Y + Month + DayOfWeek + Y2 + XY + XHour + YHour, #DayOfWeek + PdDistrict + Year + Month + Hour + X + Y, # include X Y?
  data = crimeTrain,
  method = "rpart2",
  trControl = tC,
  tuneGrid = expand.grid(maxdepth = c(6:15)),
  metric = "Accuracy",
  parm = list(split = "gini"),
  cp = minCp
)
print(dtTuneMd$bestTune)

dtFinal = rpart(Crime_Level ~ PdId + DayOfWeek + PdDistrict + X + Y + Year + Month + Hour + Season + DayOfWeek + X2 + Y2 + XY + XHour + YHour,#X + Y + Month + DayOfWeek + Y2 + XY + XHour + YHour, #DayOfWeek + PdDistrict + Year + Month + Hour + X + Y, # include X Y?
                data = crimeTrain,
                method = "class",
                parm = list(split = "gini"),
                cp = minCp,
                maxdepth = dtTuneMd$bestTune
)

# Training Accuracy
dtPredTrain = predict(dtFinal, crimeTrain[,c(1:12, 14:18)], type=c("class"), verbose = FALSE) #[,1:12]
dtTrainAcc = 1 - mean(dtPredTrain != crimeTrain$Crime_Level) # DT training Accuracy
print(paste("DT Training Accuracy =", dtTrainAcc))

#Testing Accuracy
dtPred = predict(dtFinal, crimeTest[,c(1:12, 14:18)], type=c("class"), verbose = FALSE) #c(4,5,7,8,9,10,11) #[,1:12]
table(dtPred, crimeTest$Crime_Level)
mean(dtPred != crimeTest$Crime_Level) # mean error of decison tree
dtAcc = 1 - mean(dtPred != crimeTest$Crime_Level) # DT testing Accuracy
print(paste("DT Testing Accuracy =", dtAcc))

# pruning HAD NO EFFECT
dtPrune = prune(dtFinal, cp=minCp)

#Testing Accuracy after pruning
dtPred2 = predict(dtPrune, crimeTest[,c(1:12, 14:18)], type=c("class"), verbose = FALSE) #c(4,5,7,8,9,10,11) #[,1:12]
table(dtPred2, crimeTest$Crime_Level)
mean(dtPred2 != crimeTest$Crime_Level)
prunedAcc = sum(diag(table(dtPred2, crimeTest$Crime_Level)))/sum(table(dtPred2, crimeTest$Crime_Level))
print(paste("Pruned DT Testing Accuracy =", round(prunedAcc, 4)))

#######################################################
#Linear Model

library(glmnet)
library(nnet)
library(performance)
library(car)

#remove interaction variables from data sets
for (i in 1:(length(crimeTrain[1,]) - 13)){
  crimeTrain = crimeTrain[,-14]
  crimeTest  = crimeTest[,-14]
}

ggcorrmat(crimeTrain)

#remove highly and slightly correlated variables from training set
crimeTrain2 = crimeTrain[,-7]
crimeTrain2 = crimeTrain2[,-8]
crimeTrain2 = crimeTrain2[,-1]

glm = multinom(Crime_Level ~ DayOfWeek + PdDistrict + Month + Hour + Year,
          data = crimeTrain2,
          trace = FALSE
)

# Stepwise variable selection to reduce variables
stepMod  = step(glm,
                trace = FALSE
                )

# Coefficents of stepMod
# summary(stepMod)

# Model predictions and accuracies 
glmPred = predict(glm, crimeTest[,1:12])
stepPred = predict(stepMod, crimeTest[,1:12])

glmCM = confusionMatrix(as.factor(glmPred), as.factor(crimeTest$Crime_Level))
glmCM$overall[1]
stepCM = confusionMatrix(as.factor(stepPred), as.factor(crimeTest$Crime_Level))
stepCM$overall[1]
#######################################################

##END