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

#Random Forest

library(randomForest)
library(datasets)
library(caret)

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
library(keras)

library(tensorflow)


#install.packages('bnlearn')

library(bnlearn)

## SHAPIRO TEST
load("~/R EXAM/databases/mcshapiro.test.RData")

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

###
# Random Forest -- PG
######

data = Data_PG[,2:60]

data$drafted <- as.factor(data$drafted)

set.seed(120)  # Setting seed

# Train/Test split 

ind <- sample(2, nrow(data), replace = TRUE, prob = c(0.7, 0.3))
train <- data[ind==1,]
test <- data[ind==2,]

rf <- randomForest(drafted~., data=train, proximity=TRUE) 

print(rf)

# Confusion Matrix - train

p1 <- predict(rf, train)
confusionMatrix(p1, train$drafted)

# Sensitivity : 1.0000     
#Specificity : 1.0000     
#Prevalence : 0.9477     
#Detection Rate : 0.9477     
#Balanced Accuracy : 1.0000     


# Confusion Matrix - test

p2 <- predict(rf, test)
confusionMatrix(p2, test$drafted, mode = "everything")

# P-Value [Acc > NIR] : 0.02847 
# Accuracy : 0.9658 
# F1 : 0.9823 
# Sensitivity : 0.9940 
# Specificity : 0.3750   
# Balanced Accuracy : 0.6845 

plot(rf)

# Tune RF

t <- tuneRF(train[,-59], train[,59],
            stepFactor = 0.5,
            plot = TRUE,
            ntreeTry = 400,
            trace = TRUE,
            improve = 0.05)

rf <- randomForest(drafted~., data=train, mtry = 7 , ntree = 400, proximity=TRUE) 

p2 <- predict(rf, test)
confusionMatrix(p2, test$drafted)


### 0.1166795 R2 - 0.2456322 RMSE

# EVEN WORSE WHEN WE TRY TO TUNE !

rf <- randomForest(drafted~., data=train, proximity=TRUE) 

actual <- as.numeric(test$drafted)
predicted <- as.numeric(p2)

R2 <- 1 - (sum((actual-predicted)^2)/sum((actual-mean(actual))^2))
R2

caret::RMSE(predicted,actual)

### 0.2141332 R2 - 0 0.1850131 RMSE

varImpPlot(rf,
           sort = T,
           n.var = 10,
           main = "Top 10 - Variable Importance")
importance(rf)

MDSplot(rf, train$drafted)

p3 <- predict(rf, Data_PG_2023)
p3

drafted_PG <- subset(Data_PG_2023, p3 == 1)

otp <- drafted_PG

###
# Random Forest -- SG
######

data = Data_SG

data$drafted <- as.factor(data$drafted)

set.seed(120)  # Setting seed

# Train/Test split 

ind <- sample(2, nrow(data), replace = TRUE, prob = c(0.7, 0.3))
train <- data[ind==1,]
test <- data[ind==2,]

rf <- randomForest(drafted~., data=train, proximity=TRUE) 

print(rf)

# Confusion Matrix - train

p1 <- predict(rf, train)
confusionMatrix(p1, train$drafted)

# Sensitivity : 1.0000     
#Specificity : 1.0000     
#Prevalence : 0.9477     
#Detection Rate : 0.9477     
#Balanced Accuracy : 1.0000     


# Confusion Matrix - test

p2 <- predict(rf, test)
confusionMatrix(p2, test$drafted, mode = "everything")

# P-Value [Acc > NIR] : 0.01536
# Accuracy : Accuracy : 0.9595
#  F1 : 0.9790  
# Sensitivity : 0.9952 
# Specificity : 0.3077   
#  Balanced Accuracy : 0.6514 

plot(rf)

actual <- as.numeric(test$drafted)
predicted <- as.numeric(p2)

R2 <- 1 - (sum((actual-predicted)^2)/sum((actual-mean(actual))^2))
R2

caret::RMSE(predicted,actual)

### 0.1770863 R2 - 0 0.2011937 RMSE

varImpPlot(rf,
           sort = T,
           n.var = 10,
           main = "Top 10 - Variable Importance")
importance(rf)

MDSplot(rf, train$drafted)

p3 <- predict(rf, Data_SG_2023)
p3

drafted_SG <- subset(Data_SG_2023, p3 == 1)

otp <- rbind(drafted_PG, drafted_SG)



###
# Random Forest -- SF
######

data = Data_SF[,2:60]

data$drafted <- as.factor(data$drafted)

set.seed(120)  # Setting seed

# Train/Test split 

ind <- sample(2, nrow(data), replace = TRUE, prob = c(0.7, 0.3))
train <- data[ind==1,]
test <- data[ind==2,]

rf <- randomForest(drafted~., data=train, proximity=TRUE) 

print(rf)

# Confusion Matrix - train

p1 <- predict(rf, train)
confusionMatrix(p1, train$drafted)

# Sensitivity : 1.0000     
#Specificity : 1.0000     
#Prevalence : 0.9477     
#Detection Rate : 0.9477     
#Balanced Accuracy : 1.0000     


# Confusion Matrix - test

p2 <- predict(rf, test)
confusionMatrix(p2, test$drafted, mode = "everything")

# P-Value [Acc > NIR] : 0.06752        
# Accuracy : 0.9397   
# F1 : 0.9680 
# Sensitivity : 0.9867 
#  Specificity : 0.3485  
#  Balanced Accuracy : 0.6676 

plot(rf)

actual <- as.numeric(test$drafted)
predicted <- as.numeric(p2)

R2 <- 1 - (sum((actual-predicted)^2)/sum((actual-mean(actual))^2))
R2

caret::RMSE(predicted,actual)

###  0.1166795 R2 - 0.2456322 RMSE

varImpPlot(rf,
           sort = T,
           n.var = 10,
           main = "Top 10 - Variable Importance")
importance(rf)

MDSplot(rf, train$drafted)

p3 <- predict(rf, Data_SF_2023)
p3

drafted_SF <- subset(Data_SF_2023, p3 == 1)

otp <- rbind(otp, drafted_SF)

###
# Random Forest -- PF
######

data = Data_PF[,2:60]

data$drafted <- as.factor(data$drafted)

set.seed(120)  # Setting seed

# Train/Test split 

ind <- sample(2, nrow(data), replace = TRUE, prob = c(0.7, 0.3))
train <- data[ind==1,]
test <- data[ind==2,]

rf <- randomForest(drafted~., data=train, proximity=TRUE) 

print(rf)

# Confusion Matrix - train

p1 <- predict(rf, train)
confusionMatrix(p1, train$drafted)

# Sensitivity : 1.0000     
#Specificity : 1.0000     
#Prevalence : 0.9477     
#Detection Rate : 0.9477     
#Balanced Accuracy : 1.0000     


# Confusion Matrix - test

p2 <- predict(rf, test)
confusionMatrix(p2, test$drafted, mode = "everything")

# P-Value [Acc > NIR] : 0.3111957       
# Accuracy : 0.9215 
#  F1 : 0.9583  
# Sensitivity : 0.9888           
#  Specificity : 0.2308   
#    Balanced Accuracy : 0.6098

actual <- as.numeric(test$drafted)
predicted <- as.numeric(p2)

R2 <- 1 - (sum((actual-predicted)^2)/sum((actual-mean(actual))^2))
R2

caret::RMSE(predicted,actual)

###  0.02924229 R2 - 0.2801755 RMSE


plot(rf)

varImpPlot(rf,
           sort = T,
           n.var = 10,
           main = "Top 10 - Variable Importance")
importance(rf)

MDSplot(rf, train$drafted)

p3 <- predict(rf, Data_PF_2023)
p3

drafted_PF <- subset(Data_PF_2023, p3 == 1)

otp <- rbind(otp, drafted_PF)



###
# Random Forest -- C
######

data = Data_C[,2:60]

data$drafted <- as.factor(data$drafted)

set.seed(120)  # Setting seed

# Train/Test split 

ind <- sample(2, nrow(data), replace = TRUE, prob = c(0.7, 0.3))
train <- data[ind==1,]
test <- data[ind==2,]

rf <- randomForest(drafted~., data=train, proximity=TRUE) 

print(rf)

# Confusion Matrix - train

p1 <- predict(rf, train)
confusionMatrix(p1, train$drafted)

# Sensitivity : 1.0000     
#Specificity : 1.0000     
#Prevalence : 0.9477     
#Detection Rate : 0.9477     
#Balanced Accuracy : 1.0000     


# Confusion Matrix - test

p2 <- predict(rf, test)
confusionMatrix(p2, test$drafted, mode = "everything")

# P-Value [Acc > NIR] : 0.07559       
# Accuracy : 0.9155  
# F1 : 0.9538
# Sensitivity : 0.9841           
# Specificity : 0.3750    
# Balanced Accuracy : 0.6796  

actual <- as.numeric(test$drafted)
predicted <- as.numeric(p2)

R2 <- 1 - (sum((actual-predicted)^2)/sum((actual-mean(actual))^2))
R2

caret::RMSE(predicted,actual)

###  0.1547619 R2 - 0.2907009 RMSE
  

plot(rf)

varImpPlot(rf,
           sort = T,
           n.var = 10,
           main = "Top 10 - Variable Importance")
importance(rf)

MDSplot(rf, train$drafted)

p3 <- predict(rf, Data_C_2023)
p3

drafted_C <- subset(Data_C_2023, p3 == 1)

otp <- rbind(otp, drafted_C)

otp$player_name

otp = otp[!duplicated(otp$player_name), ]

otp[,"player_name"]