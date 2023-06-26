## Libraries ####
library(mvtnorm)
library(mvnormtest)
library(rgl)
library(car)
library(MASS)
library(class) #clustering
library(e1071) #support vector machine and naive bayes
library(tidyr)
library(dplyr)

# Functional data analysis
library(fda)
library(KernSmooth)
library(fields)
library(fdakma)

#Bayesian network library

#naive bayes classifier
library(caTools)
library(caret)
library(dplyr)

#lasso regression
library(glmnet)

# NEURAL NETWORKS

#install.packages('keras')
#install.packages('mlbench')
#install.packages('neuralnet')

library(keras)
library(mlbench)
library(dplyr)
library(magrittr)
library(neuralnet)

#install_keras()
library(tidyverse)
library(keras)
library(fastDummies)
library(caret)
library(tensorflow)
library(kerasR)

#Naive Bayes
library(naivebayes)
library(dplyr)
library(ggplot2)
library(psych)

#install.packages('bnlearn')

library(bnlearn)

## SHAPIRO TEST
load("~/R EXAM/databases/mcshapiro.test.RData")

# Support Vector Machine
#install.packages('e1071')
library(e1071)

#######
# import
#######

#Past years

Data_2009 <- read.csv("~/R EXAM/databases/player_db_2009.csv")
Data_2010 <- read.csv("~/R EXAM/databases/player_db_2010.csv")
Data_2011 <- read.csv("~/R EXAM/databases/player_db_2011.csv")
Data_2012 <- read.csv("~/R EXAM/databases/player_db_2012.csv")
Data_2013 <- read.csv("~/R EXAM/databases/player_db_2013.csv")
Data_2014 <- read.csv("~/R EXAM/databases/player_db_2014.csv")
Data_2015 <- read.csv("~/R EXAM/databases/player_db_2015.csv")
Data_2016 <- read.csv("~/R EXAM/databases/player_db_2016.csv")
Data_2017 <- read.csv("~/R EXAM/databases/player_db_2017.csv")
Data_2018 <- read.csv("~/R EXAM/databases/player_db_2018.csv")
Data_2019 <- read.csv("~/R EXAM/databases/player_db_2019.csv")
Data_2020 <- read.csv("~/R EXAM/databases/player_db_2020.csv")
Data_2021 <- read.csv("~/R EXAM/databases/player_db_2021.csv")
Data_2022 <- read.csv("~/R EXAM/databases/player_db_2022.csv")

#This year !

Data_2023 <- read.csv("~/R EXAM/databases/player_db_2023.csv")

#######
# First step of cleaning
#######

#Features names

features_name <- read.csv("~/R EXAM/databases/stats_columns_explained.csv", sep = ";", header=F, nrows = 1,colClasses = "character", fileEncoding = 'UTF-8-BOM')
colnames(Data_2009)<-c(features_name[1,],"posi","unknow")
colnames(Data_2010)<-c(features_name[1,],"posi","unknow")
colnames(Data_2011)<-c(features_name[1,],"posi","unknow")
colnames(Data_2012)<-c(features_name[1,],"posi","unknow")
colnames(Data_2013)<-c(features_name[1,],"posi","unknow")
colnames(Data_2014)<-c(features_name[1,],"posi","unknow")
colnames(Data_2015)<-c(features_name[1,],"posi","unknow")
colnames(Data_2016)<-c(features_name[1,],"posi","unknow")
colnames(Data_2017)<-c(features_name[1,],"posi","unknow")
colnames(Data_2018)<-c(features_name[1,],"posi","unknow")
colnames(Data_2019)<-c(features_name[1,],"posi","unknow")
colnames(Data_2020)<-c(features_name[1,],"posi","unknow")
colnames(Data_2021)<-c(features_name[1,],"posi","unknow")
colnames(Data_2022)<-c(features_name[1,],"posi","unknow")
colnames(Data_2023)<-c(features_name[1,],"posi","unknow")

#We convert the height of players into centimeters

total_players <- rbind(Data_2012,Data_2013,Data_2014,Data_2015,Data_2016,Data_2017,Data_2018,Data_2019,Data_2020,Data_2021,Data_2022)
total_players
summary(total_players)
Data <- total_players

Data <- separate(Data,ht, c('feet', 'inches'), "-", convert = TRUE) 
Data$feet <- as.numeric(Data$feet)

Data <- mutate(Data,ht_cm = (12*feet + inches)*2.54)

Data_2023 <- separate(Data_2023,ht, c('feet', 'inches'), "-", convert = TRUE) 
Data_2023 <- mutate(Data_2023,ht_cm = (12*feet + inches)*2.54)

# We get rid of all the useless variables and convert the team conference and nb of years spent in university
# as a numerical value (one hot encoding)

Data <- subset(Data, select = -c(unknow,type,num,team,feet,inches,year))
Data_2023 <- subset(Data_2023, select = -c(unknow,type,num,team,feet,inches))

levels(as.factor(Data$conf))
levels(as.factor(Data$yr))

Data$conf <- as.numeric(as.factor(Data$conf))
Data_2023$conf <- as.numeric(as.factor(Data_2023$conf))

#DELETE PLAYERS THAT DID NOT PLAY
#players whose time playing is too low to be considered

Data<-subset(Data,Min_per>70.0);
Data_2023 <- subset(Data_2023,Min_per>70.0);

Data$yr <- as.numeric(as.factor(Data$yr))
Data_2023$yr <- as.numeric(as.factor(Data_2023$yr))

# We create a 'drafted' category for our classifier
# We could keep the "pick" value as a target for a regressor, assessing an infinite value to players not drafted...

drafted <- Data$` pick` != '' 
Data <- cbind(Data, drafted)

Data <- subset(Data, select = -c(` pick`))
Data_2023 <- subset(Data_2023, select = -c(` pick`,year))

#REPLACE NULL AND NA VALUES BY THE MEAN OF EACH COLUMN -- CAN BE MODIFIED LATER : 

Data[is.na(Data)] <- 0

Data$Rec.Rank[is.na(Data$Rec.Rank)] <- 0

Data_2023[is.na(Data_2023)] <- 0

names(Data_2023)
names(Data)
Data_2023 <- Data_2023 %>% 
  rename(
    X.rimmade=" rimmade",
    X.rimmade.rimmiss = " rimmade+rimmiss",
    X.midmade = " midmade",
    X.midmade.midmiss = " midmade+midmiss",
    X.rimmade..rimmade.rimmiss. = " rimmade/(rimmade+rimmiss)",
    X.midmade..midmade.midmiss. = " midmade/(midmade+midmiss)",
    X.dunksmade = " dunksmade",
    X.dunksmiss.dunksmade = " dunksmiss+dunksmade",
    X.dunksmade..dunksmade.dunksmiss. = " dunksmade/(dunksmade+dunksmiss)",
    Rec.Rank = "Rec Rank",
    X.ast.tov = " ast/tov",
    X.drtg = " drtg",
    X.dporpag = " dporpag",
    X.stops = " stops",
    X.bpm = " bpm",
    X.obpm = " obpm",
    X.dbpm = " dbpm",
    X.gbpm = " gbpm"
  )

names(Data_2023)

Data <- Data %>% 
  rename(
    X.rimmade=" rimmade",
    X.rimmade.rimmiss = " rimmade+rimmiss",
    X.midmade = " midmade",
    X.midmade.midmiss = " midmade+midmiss",
    X.rimmade..rimmade.rimmiss. = " rimmade/(rimmade+rimmiss)",
    X.midmade..midmade.midmiss. = " midmade/(midmade+midmiss)",
    X.dunksmade = " dunksmade",
    X.dunksmiss.dunksmade = " dunksmiss+dunksmade",
    X.dunksmade..dunksmade.dunksmiss. = " dunksmade/(dunksmade+dunksmiss)",
    Rec.Rank = "Rec Rank",
    X.ast.tov = " ast/tov",
    X.drtg = " drtg",
    X.dporpag = " dporpag",
    X.stops = " stops",
    X.bpm = " bpm",
    X.obpm = " obpm",
    X.dbpm = " dbpm",
    X.gbpm = " gbpm"
  )

### Create Dummy variables for positions

#1st useful function that duplicates a col to a temp df

to_temp_dataf <- function(col){
  veccol<-as.vector(col);
  doubloncol<-which(duplicated(veccol));
  veccol<-veccol[-doubloncol];
  df <- data.frame(matrix(ncol = length(veccol), nrow = 0))
  colnames(df)<-veccol
  
  return(df)
}

#2nd useful function that transforms the categorical value to a binary

cat_to_num <- function(val,col,data,ndata){
  veccol<-as.vector(col);
  doubloncol<-which(duplicated(veccol));
  veccol<-veccol[-doubloncol];
  vecData<-vector();
  for( i in 1:(length(veccol))){
    if(val==veccol[i]){
      vecData<-c(vecData,1);
    }else{
      vecData<-c(vecData,0);
    };
  }
  tmpdf<-data.frame(matrix(vecData,ncol=length(veccol),nrow=1));
  colnames(tmpdf)<-veccol;
  tmpdf<-data.frame(tmpdf);
  ndata<-rbind(ndata,tmpdf);
  return(ndata)
}

# Final function that calls the two others to generate dummy variables

conc_dataf<-function(name,column,data){
  if(name %in% colnames(data)){
    tempdata<-to_temp_dataf(column);
    for( i in column){
      tempdata<-cat_to_num(i,column,data,tempdata);
    }
    data<-cbind(data,tempdata);
    data<-data[,-(which(colnames(data) == name))]
    return(data)
  }
}

# APPLY IT ON OUR DATASETS

Data<-conc_dataf("posi",Data$posi,Data);
Data_2023 <- conc_dataf("posi", Data_2023$posi,Data_2023)

#####

### Now let's split by positions ! 
#####

Data_PG <- subset(Data,(Pure.PG == 1 | Scoring.PG == 1 | Combo.G == 1), select = -c(Pure.PG,Scoring.PG,Combo.G,Wing.G,Wing.F,Stretch.4,PF.C,C)) ;

Data_PG_2023 <- subset(Data_2023,(Pure.PG == 1 | Scoring.PG == 1 | Combo.G == 1), select = -c(Pure.PG,Scoring.PG,Combo.G,Wing.G,Wing.F,Stretch.4,PF.C,C)) ;

Data_SG <- subset(Data,(Wing.G == 1 | Scoring.PG == 1 | Combo.G == 1), select = -c(Pure.PG,Scoring.PG,Combo.G,Wing.G,Wing.F,Stretch.4,PF.C,C)); 

Data_SG_2023 <- subset(Data_2023,(Wing.G == 1 | Scoring.PG == 1 | Combo.G == 1), select = -c(Pure.PG,Scoring.PG,Combo.G,Wing.G,Wing.F,Stretch.4,PF.C,C));

Data_SF <- subset(Data, (Wing.G == 1 | Wing.F == 1), select = -c(Pure.PG,Scoring.PG,Combo.G,Wing.G,Wing.F,Stretch.4,PF.C,C)) ;

Data_SF_2023 <- subset(Data_2023,(Wing.G == 1 | Wing.F == 1), select = -c(Pure.PG,Scoring.PG,Combo.G,Wing.G,Wing.F,Stretch.4,PF.C,C)) ;

Data_PF <- subset(Data, (Stretch.4 == 1 | PF.C == 1), select = -c(Pure.PG,Scoring.PG,Combo.G,Wing.G,Wing.F,Stretch.4,PF.C,C)) ;

Data_PF_2023 <- subset(Data_2023,(Stretch.4 == 1 | PF.C == 1), select = -c(Pure.PG,Scoring.PG,Combo.G,Wing.G,Wing.F,Stretch.4,PF.C,C)) ;

Data_C <- subset(Data,(C == 1 | PF.C == 1), select = -c(Pure.PG,Scoring.PG,Combo.G,Wing.G,Wing.F,Stretch.4,PF.C,C)) ;

Data_C_2023 <- subset(Data_2023,(C == 1 | PF.C == 1), select = -c(Pure.PG,Scoring.PG,Combo.G,Wing.G,Wing.F,Stretch.4,PF.C,C))

#####

##
# LASSO REG - PG
#####

df <- subset(Data_PG, select = -c(player_name))

#make this example reproducible
set.seed(1)

#Use 70% of our dataset as training set and remaining 30% as testing set
sample <- sample(c(TRUE, FALSE), nrow(df), replace=TRUE, prob=c(0.7,0.3))
train  <- df[sample, ]
test   <- df[!sample, ]

train_x <- data.matrix(as.data.frame(scale(train[,1:58])))
test_x <- data.matrix(as.data.frame(scale(test[,1:58])))

y_train <- as.factor(train$drafted)
y_test <- as.factor(test$drafted)

#perform k-fold cross-validation to find optimal lambda value
cv_model <- cv.glmnet(train_x, y_train, alpha = 1, family = "binomial", type.measure = 'mse')

#find optimal lambda value that minimizes train MSE
best_lam<- cv_model$lambda.min
best_lam

#0.0007562245

#produce plot of train MSE by lambda value
plot(cv_model)

#find coefficients of best model
best_model <- glmnet(train_x, y_train, alpha = 1, family = "binomial", lambda = best_lam)
coef(best_model)

# 5 most important variables : 
# ast : 0.87
# yr : -0.76
# ht_cm : 0.74
# X.obpm : 0.54
# adrtg : -0.57

# Interesting as we saw in data exploration that year was in fact very disparate among the groups.

train_pred = predict(best_model,s = best_lam, newx = train_x, type = "response")

y_pred = predict(best_model,s = best_lam, newx = test_x, type = "response" )

y_pred <- y_pred[,1]
y_pred

y_pred <- ifelse(y_pred >= 0.5, 1, 0)

# Evaluating model accuracy
# using confusion matrix

table <- confusionMatrix(data= as.factor(y_pred), reference = as.factor(y_test),mode = "everything")
table

# P-Value [Acc > NIR] : 0.002087  
# Accuracy : 0.9622 
# F1 : 0.9802
# Sensitivity : 0.9926  
# Specificity : 0.4306   
# Balanced Accuracy : 0.7120  

# computing model performance metrics
data.frame( R2 = R2(y_pred, as.numeric(y_test)),
            RMSE = RMSE(y_pred, as.numeric(y_test)),
            MAE = MAE(y_pred, as.numeric(y_test)))

### 0.3231957 R2 - 1.043457 RMSE 


PG_draft.pred = predict(best_model,s = best_lam, newx = data.matrix(as.data.frame(scale(Data_PG_2023[,2:59]))), type = "response" )

bbb <- as.numeric(PG_draft.pred)

bbb

Draft_2023 <- Data_PG_2023
Draft_2023 <- cbind(Draft_2023,pred = bbb)
Draft_2023 <- subset(Draft_2023, pred > 0.5)

Draft_2023[,"player_name"]

#####
# LASSO REGRESSION - SG
#####

df <- subset(Data_SG, select = -c(player_name))

#make this example reproducible
set.seed(1)

#Use 70% of our dataset as training set and remaining 30% as testing set
sample <- sample(c(TRUE, FALSE), nrow(df), replace=TRUE, prob=c(0.7,0.3))
train  <- df[sample, ]
test   <- df[!sample, ]

train_x <- data.matrix(as.data.frame(scale(train[,1:58])))
test_x <- data.matrix(as.data.frame(scale(test[,1:58])))

y_train <- as.factor(train$drafted)
y_test <- as.factor(test$drafted)

#perform k-fold cross-validation to find optimal lambda value
cv_model <- cv.glmnet(train_x, y_train, alpha = 1, family = "binomial", type.measure = 'mse')

#find optimal lambda value that minimizes train MSE
best_lam<- cv_model$lambda.min
best_lam

#0.0005137776

#produce plot of train MSE by lambda value
plot(cv_model)

#find coefficients of best model
best_model <- glmnet(train_x, y_train, alpha = 1, family = "binomial", lambda = best_lam)
coef(best_model)

# 5 most important variables : 
# TO_per : 0.82
# yr : -0.78
# ht_cm : 0.86
# X.obpm : 1.39
# porpag : 0.6

# Interesting to notice two important features : TO_per and porpag, we see that the offensive BOX +/- is
# VERY IMPORTANT here

train_pred = predict(best_model,s = best_lam, newx = train_x, type = "response")

y_pred = predict(best_model,s = best_lam, newx = test_x, type = "response" )

y_pred <- y_pred[,1]

y_pred <- ifelse(y_pred >= 0.5, 1, 0)

# Evaluating model accuracy
# using confusion matrix

table <- confusionMatrix(data= as.factor(y_pred), reference = as.factor(y_test),mode = "everything")
table

#  P-Value [Acc > NIR] : 0.03444 
# Accuracy : 0.9542 
# F1 : 0.9760
# Sensitivity : 0.9873  
# Specificity : 0.3922  
#  Balanced Accuracy : 0.6897 

# computing model performance metrics
data.frame( R2 = R2(y_pred, as.numeric(y_test)),
            RMSE = RMSE(y_pred, as.numeric(y_test)),
            MAE = MAE(y_pred, as.numeric(y_test)))

### 0.2315329 R2 - 1.043754 RMSE 

SG_draft.pred = predict(best_model,s = best_lam, newx = data.matrix(as.data.frame(scale(Data_SG_2023[,2:59]))), type = "response" )

bbb <- as.numeric(SG_draft.pred)

bbb

tp <- Data_SG_2023
tp <- cbind(tp, pred = bbb)
tp <- subset(tp, pred > 0.5 )

Draft_2023 <- rbind(Draft_2023,tp)

Draft_2023[,"player_name"]

#####
# LASSO REGRESSION - SF
#####

df <- subset(Data_SF, select = -c(player_name))

#make this example reproducible
set.seed(1)

#Use 70% of our dataset as training set and remaining 30% as testing set
sample <- sample(c(TRUE, FALSE), nrow(df), replace=TRUE, prob=c(0.7,0.3))
train  <- df[sample, ]
test   <- df[!sample, ]

train_x <- data.matrix(as.data.frame(scale(train[,1:58])))
test_x <- data.matrix(as.data.frame(scale(test[,1:58])))

y_train <- as.factor(train$drafted)
y_test <- as.factor(test$drafted)

#perform k-fold cross-validation to find optimal lambda value
cv_model <- cv.glmnet(train_x, y_train, alpha = 1, family = "binomial", type.measure = 'mse')

#find optimal lambda value that minimizes train MSE
best_lam<- cv_model$lambda.min
best_lam

#0.0006321677

#produce plot of train MSE by lambda value
plot(cv_model)

#find coefficients of best model
best_model <- glmnet(train_x, y_train, alpha = 1, family = "binomial", lambda = best_lam)
coef(best_model)

# 5 most important variables : 
# blk_per                           -0.88392262
# X.dunksmade                        0.69120800
# oreb                              -0.56420058
# X.obpm                             0.73188337
# blk                                1.06403702

# Interesting to notice that all values reward activity and athleticism !

train_pred = predict(best_model,s = best_lam, newx = train_x, type = "response")

y_pred = predict(best_model,s = best_lam, newx = test_x, type = "response" )

y_pred <- y_pred[,1]

y_pred <- ifelse(y_pred >= 0.5, 1, 0)

# Evaluating model accuracy
# using confusion matrix

table <- confusionMatrix(data= as.factor(y_pred), reference = as.factor(y_test),mode = "everything")
table

#  P-Value [Acc > NIR] : 0.03444 
# Accuracy : 0.9542 
#F1 : 0.9760
# Sensitivity : 0.9873  
# Specificity : 0.3922  
#  Balanced Accuracy : 0.6897 

# computing model performance metrics
data.frame( R2 = R2(y_pred, as.numeric(y_test)),
            RMSE = RMSE(y_pred, as.numeric(y_test)),
            MAE = MAE(y_pred, as.numeric(y_test)))

### 0.2315329 R2 - 1.043754 RMSE 

SF_draft.pred = predict(best_model,s = best_lam, newx = data.matrix(as.data.frame(scale(Data_SF_2023[,2:59]))), type = "response" )

bbb <- as.numeric(SF_draft.pred)

bbb


tp <- Data_SF_2023
tp <- cbind(tp, pred = bbb)
tp <- subset(tp, pred > 0.5)

Draft_2023 <- rbind(Draft_2023,tp)

Draft_2023[,"player_name"]


#####
# LASSO REG - PF
#####

df <- subset(Data_PF, select = -c(player_name))
#make this example reproducible
set.seed(1)

#Use 70% of our dataset as training set and remaining 30% as testing set
sample <- sample(c(TRUE, FALSE), nrow(df), replace=TRUE, prob=c(0.7,0.3))
train  <- df[sample, ]
test   <- df[!sample, ]

train_x <- data.matrix(as.data.frame(scale(train[,1:58])))
test_x <- data.matrix(as.data.frame(scale(test[,1:58])))

y_train <- as.factor(train$drafted)
y_test <- as.factor(test$drafted)

#perform k-fold cross-validation to find optimal lambda value
cv_model <- cv.glmnet(train_x, y_train, alpha = 1, family = "binomial", type.measure = 'mse')

#find optimal lambda value that minimizes train MSE
best_lam<- cv_model$lambda.min
best_lam

#0.009752269

#produce plot of train MSE by lambda value
plot(cv_model)

#find coefficients of best model
best_model <- glmnet(train_x, y_train, alpha = 1, family = "binomial", lambda = best_lam)
coef(best_model)

# 5 most important variables : 
# X.dporpag                          1.02796293
# Rec.Rank                           0.48540138
# FT_per                             0.30364595
# ogbpm                              0.33753188
# X.dunksmade                        0.28550200

# Interesting because the dporpag dominates the other values !

train_pred = predict(best_model,s = best_lam, newx = train_x, type = "response")

y_pred = predict(best_model,s = best_lam, newx = test_x, type = "response" )

y_pred <- y_pred[,1]

y_pred <- ifelse(y_pred >= 0.5, 1, 0)

# Evaluating model accuracy
# using confusion matrix

table <- confusionMatrix(data= as.factor(y_pred), reference = as.factor(y_test),mode = "everything")
table

#  P-Value [Acc > NIR] : 0.3290917  BAD 
# Accuracy : 0.899 
#F1 : 0.9760
# Sensitivity : 0.9811 
# Specificity : 0.2424  
# Balanced Accuracy : 0.6117  

# computing model performance metrics
data.frame( R2 = R2(y_pred, as.numeric(y_test)),
            RMSE = RMSE(y_pred, as.numeric(y_test)),
            MAE = MAE(y_pred, as.numeric(y_test)))

### 0.1178562 R2 - 1.043754 RMSE 

PF_draft.pred = predict(best_model,s = best_lam, newx = data.matrix(as.data.frame(scale(Data_PF_2023[,2:59]))), type = "response" )

bbb <- as.numeric(PF_draft.pred)

bbb

tp <- Data_PF_2023
tp <- cbind(tp, pred = bbb)
tp <- subset(tp, pred > 0.5)

Draft_2023 <- rbind(Draft_2023,tp)

Draft_2023[,"player_name"]

#####
# LASSO REG - C
#####

df <- subset(Data_C, select = -c(player_name))

#make this example reproducible
set.seed(1)

#Use 70% of our dataset as training set and remaining 30% as testing set
sample <- sample(c(TRUE, FALSE), nrow(df), replace=TRUE, prob=c(0.7,0.3))
train  <- df[sample, ]
test   <- df[!sample, ]

train_x <- data.matrix(as.data.frame(scale(train[,1:58])))
test_x <- data.matrix(as.data.frame(scale(test[,1:58])))

y_train <- as.factor(train$drafted)
y_test <- as.factor(test$drafted)

#perform k-fold cross-validation to find optimal lambda value
cv_model <- cv.glmnet(train_x, y_train, alpha = 1, family = "binomial", type.measure = 'mse')

#find optimal lambda value that minimizes train MSE
best_lam<- cv_model$lambda.min
best_lam

#0.002903338

#produce plot of train MSE by lambda value
plot(cv_model)

#find coefficients of best model
best_model <- glmnet(train_x, y_train, alpha = 1, family = "binomial", lambda = best_lam)
coef(best_model)

# 5 most important variables : 
# X.dporpag                          1.015485359
# ht_cm                              0.519961930
# X.rimmade.rimmiss                 -0.802926842
# ogbpm                              1.336642164
# TO_per                             0.962228910

# ....

train_pred = predict(best_model,s = best_lam, newx = train_x, type = "response")

y_pred = predict(best_model,s = best_lam, newx = test_x, type = "response" )

y_pred <- y_pred[,1]

y_pred <- ifelse(y_pred >= 0.5, 1, 0)

# Evaluating model accuracy
# using confusion matrix

table <- confusionMatrix(data = as.factor(y_pred), reference = as.factor(y_test),mode = "everything")
table

#  P-Value [Acc > NIR] : 0.06318  
# Accuracy : 0.9024 
#F1 : 0.9451
# Sensitivity : 0.9640 
# Specificity : 0.4865   
# Balanced Accuracy : 0.7252  

# computing model performance metrics
data.frame( R2 = R2(y_pred, as.numeric(y_test)),
            RMSE = RMSE(y_pred, as.numeric(y_test)),
            MAE = MAE(y_pred, as.numeric(y_test)))

### 0.2674042 R2 - 1.080392 RMSE 

C_draft.pred = predict(best_model,s = best_lam, newx = data.matrix(as.data.frame(scale(Data_C_2023[,2:59]))), type = "response" )

bbb <- as.numeric(C_draft.pred)

bbb

tp <- Data_C_2023
tp <- cbind(tp, pred = bbb)
tp <- subset(tp, pred > 0.5)

Draft_2023 <- rbind(Draft_2023,tp)

Draft_2023[,"player_name"]

Draft_order <- Draft_2023[order(-Draft_2023$pred),]

Draft_order[,"player_name"]

Draft_order = Draft_order[!duplicated(Draft_order$player_name), ]

Draft_order[,"player_name"]