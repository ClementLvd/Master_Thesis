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
# LOG REG - PG
#####

df <- subset(Data_PG, select = -c(player_name))

#make this example reproducible
set.seed(1)

#Use 70% of our dataset as training set and remaining 30% as testing set
sample <- sample(c(TRUE, FALSE), nrow(df), replace=TRUE, prob=c(0.7,0.3))
train  <- df[sample, ]
test   <- df[!sample, ]

logreg_PG <- glm(drafted ~., data = train, family = "binomial")
summary(logreg_PG)


### AIC : 579.27

#confint(logreg_PG)

#install.packages("aod")
library(aod)

wald.test(b = coef(logreg_PG), Sigma = vcov(logreg_PG), Terms = 7:11)

#Chi-squared test:
#  X2 = 18.0, df = 5, P(> X2) = 0.0029

# Predict test data based on model
predict_reg <- predict(logreg_PG, 
                       test, type = "response")
#predict_reg 

# Changing probabilities
predict_reg <- ifelse(predict_reg > 0.5, 1, 0)

# Evaluating model accuracy
# using confusion matrix

table <- confusionMatrix(data= as.factor(predict_reg), reference = as.factor(test$drafted),mode = "everything")
table

# Accuracy : 0.96
# F1 : 0.9793
# Sensitivity : 0.9869
# Specificity : 0.5139

# P-value : good

missing_classerr <- mean(predict_reg != test$drafted)
print(paste('Accuracy =', 1 - missing_classerr))

# computing model performance metrics
data.frame( R2 = R2(predict_reg, test$drafted),
            RMSE = RMSE(predict_reg, test$drafted),
            MAE = MAE(predict_reg, test$drafted))

### 0.96 accuracy !

### 0.335 R2 - 0.1984 RMSE 

# ROC-AUC Curve
library(pROC)
roc_score = roc(test[,59], predict_reg) #AUC score
plot(roc_score ,main ="ROC curve -- Logistic Regression ")

### Let's eliminate some features by stepwise regression ! 

#define intercept-only model
intercept_only <- glm(drafted ~ 1, data = train)

#define model with all predictors
all <- lm(drafted ~ ., data=train)

#perform both direction stepwise regression
both <- step(intercept_only, direction='both', scope=formula(all), trace=0)

#view results of both² direction stepwise regression
both$anova

#view final model
both$coefficients

predict_reg <- predict(both, 
                       test, type = "response")

# computing model performance metrics
data.frame( R2 = R2(predict_reg, test$drafted),
            RMSE = RMSE(predict_reg, test$drafted),
            MAE = MAE(predict_reg, test$drafted))

### 0.235 R2 - 0.1999 RMSE 

PG_draft.prob = predict(logreg_PG, Data_PG_2023, type="response")

PG_draft.prob[is.na(PG_draft.prob)] <- 0

PG_draft.prob

Draft_2023 <- Data_PG_2023
Draft_2023 <- cbind(Draft_2023,prob = PG_draft.prob)
Draft_2023 <- subset(Draft_2023, prob >= 0.5)

Draft_2023[,"player_name"]



#####
# LOG REG - SG
#####

df <- subset(Data_SG, select = -c(player_name))


#make this example reproducible
set.seed(1)

#Use 70% of our dataset as training set and remaining 30% as testing set
sample <- sample(c(TRUE, FALSE), nrow(df), replace=TRUE, prob=c(0.7,0.3))
train  <- df[sample, ]
test   <- df[!sample, ]

logreg_PG <- glm(drafted ~., data = train, family = "binomial")
summary(logreg_PG)


### AIC : 824.7

#confint(logreg_PG)

#install.packages("aod")
library(aod)

wald.test(b = coef(logreg_PG), Sigma = vcov(logreg_PG), Terms = 7:11)

#Chi-squared test:
#  X2 = 18.0, df = 5, P(> X2) = 0.0029

# Predict test data based on model
predict_reg <- predict(logreg_PG, 
                       test, type = "response")
#predict_reg 

# Changing probabilities
predict_reg <- ifelse(predict_reg > 0.5, 1, 0)

# Evaluating model accuracy
# using confusion matrix
table <- confusionMatrix(data= as.factor(predict_reg), reference = as.factor(test$drafted),mode = "everything")
table

# Accuracy : 0.952
# F1 : 0.9747
# Sensitivity : 0.9804
# Specificity : 0.4706

missing_classerr <- mean(predict_reg != test$drafted)
print(paste('Accuracy =', 1 - missing_classerr))

# Accuracy : 0.952

# computing model performance metrics
data.frame( R2 = R2(predict_reg, test$drafted),
            RMSE = RMSE(predict_reg, test$drafted),
            MAE = MAE(predict_reg, test$drafted))

### 0.25 R2 - 0.219 RMSE 

# ROC-AUC Curve
library(pROC)
roc_score = roc(test[,59], predict_reg) #AUC score
plot(roc_score ,main ="ROC curve -- Logistic Regression ")

### Let's eliminate some features by stepwise regression ! 

#define intercept-only model
intercept_only <- glm(drafted ~ 1, data = train)

#define model with all predictors
all <- lm(drafted ~ ., data=train)

#perform both direction stepwise regression
both <- step(intercept_only, direction='both', scope=formula(all), trace=0)

#view results of both² direction stepwise regression
both$anova

#view final model
both$coefficients


predict_reg <- predict(both, 
                       test, type = "response")

# computing model performance metrics
data.frame( R2 = R2(predict_reg, test$drafted),
            RMSE = RMSE(predict_reg, test$drafted),
            MAE = MAE(predict_reg, test$drafted))

### 0.233 R2 - 0.200 RMSE 

SG_draft.prob = predict(logreg_PG, Data_SG_2023, type="response")

SG_draft.prob

tp <- Data_SG_2023
tp <- cbind(tp, prob = SG_draft.prob)
tp <- subset(tp, prob >= 0.5)

Draft_2023 <- rbind(Draft_2023,tp)

Draft_2023[,"player_name"]

#####
# LOG REG - SF
#####

df <- subset(Data_SF, select = -c(player_name))


#make this example reproducible
set.seed(1)

#Use 70% of our dataset as training set and remaining 30% as testing set
sample <- sample(c(TRUE, FALSE), nrow(df), replace=TRUE, prob=c(0.7,0.3))
train  <- df[sample, ]
test   <- df[!sample, ]

logreg_PG <- glm(drafted ~., data = train, family = "binomial")
summary(logreg_PG)


### AIC : 560.63

#confint(logreg_PG)

#install.packages("aod")
library(aod)

wald.test(b = coef(logreg_PG), Sigma = vcov(logreg_PG), Terms = 7:11)

#Chi-squared test:
#  X2 = 18.0, df = 5, P(> X2) = 0.0029

# Predict test data based on model
predict_reg <- predict(logreg_PG, 
                       test, type = "response")
#predict_reg 

# Changing probabilities
predict_reg <- ifelse(predict_reg > 0.5, 1, 0)

# Evaluating model accuracy
# using confusion matrix
table <- confusionMatrix(data= as.factor(predict_reg), reference = as.factor(test$drafted),mode = "everything")
table

# Accuracy : 0.9396
# F1 : 0.9673
# Sensitivity : 0.9811
# Specificity : 0.5122


missing_classerr <- mean(predict_reg != test$drafted)
print(paste('Accuracy =', 1 - missing_classerr))

# Accuracy : 0.9395

# computing model performance metrics
data.frame( R2 = R2(predict_reg, test$drafted),
            RMSE = RMSE(predict_reg, test$drafted),
            MAE = MAE(predict_reg, test$drafted))

### 0.3344837 R2 - 0.2457 RMSE 

# ROC-AUC Curve
library(pROC)
roc_score = roc(test[,59], predict_reg) #AUC score
plot(roc_score ,main ="ROC curve -- Logistic Regression ")

### Let's eliminate some features by stepwise regression ! 

#define intercept-only model
intercept_only <- glm(drafted ~ 1, data = train)

#define model with all predictors
all <- lm(drafted ~ ., data=train)

#perform both direction stepwise regression
both <- step(intercept_only, direction='both', scope=formula(all), trace=0)

#view results of both² direction stepwise regression
both$anova

#view final model
both$coefficients


predict_reg <- predict(both, 
                       test, type = "response")

# computing model performance metrics
data.frame( R2 = R2(predict_reg, test$drafted),
            RMSE = RMSE(predict_reg, test$drafted),
            MAE = MAE(predict_reg, test$drafted))

### 0.2992763 R2 - 0.237 RMSE 

SF_draft.prob = predict(logreg_PG, Data_SF_2023, type="response")

SF_draft.prob

tp <- Data_SF_2023
tp <- cbind(tp, prob = SF_draft.prob)
tp <- subset(tp, prob >= 0.5)

Draft_2023 <- rbind(Draft_2023,tp)

Draft_2023[,"player_name"]


#####
# LOG REG - PF
#####

df <- subset(Data_PF, select = -c(player_name))

#make this example reproducible
set.seed(1)

#Use 70% of our dataset as training set and remaining 30% as testing set
sample <- sample(c(TRUE, FALSE), nrow(df), replace=TRUE, prob=c(0.7,0.3))
train  <- df[sample, ]
test   <- df[!sample, ]

logreg_PG <- glm(drafted ~., data = train, family = "binomial")
summary(logreg_PG)


### AIC : 301.92

#confint(logreg_PG)

#install.packages("aod")
library(aod)

#wald.test(b = coef(logreg_PG), Sigma = vcov(logreg_PG), Terms = 7:11)


# Predict test data based on model
predict_reg <- predict(logreg_PG, 
                       test, type = "response")
#predict_reg 

# Changing probabilities
predict_reg <- ifelse(predict_reg > 0.5, 1, 0)

# Evaluating model accuracy
# using confusion matrix
table <- confusionMatrix(data= as.factor(predict_reg), reference = as.factor(test$drafted),mode = "everything")
table

# Accuracy : 0.8922
# F1 : 0.9401
# Sensitivity : 0.9418
# Specificity : 0.4516

missing_classerr <- mean(predict_reg != test$drafted)
print(paste('Accuracy =', 1 - missing_classerr))

# Accuracy : 0.88

# computing model performance metrics
data.frame( R2 = R2(predict_reg, test$drafted),
            RMSE = RMSE(predict_reg, test$drafted),
            MAE = MAE(predict_reg, test$drafted))

### 0.15 R2 - 0.338 RMSE 

# ROC-AUC Curve
library(pROC)
roc_score = roc(test[,59], predict_reg) #AUC score
plot(roc_score ,main ="ROC curve -- Logistic Regression ")

### Let's eliminate some features by stepwise regression ! 

#define intercept-only model
intercept_only <- glm(drafted ~ 1, data = train)

#define model with all predictors
all <- lm(drafted ~ ., data=train)

#perform both direction stepwise regression
both <- step(intercept_only, direction='both', scope=formula(all), trace=0)

#view results of both² direction stepwise regression
both$anova

#view final model
both$coefficients


predict_reg <- predict(both, 
                       test, type = "response")

# computing model performance metrics
data.frame( R2 = R2(predict_reg, test$drafted),
            RMSE = RMSE(predict_reg, test$drafted),
            MAE = MAE(predict_reg, test$drafted))

### 0.2314 R2 - 0.2756 RMSE 

PF_draft.prob = predict(logreg_PG, Data_PF_2023, type="response")

PF_draft.prob

tp <- Data_PF_2023
tp <- cbind(tp, prob = PF_draft.prob)
tp <- subset(tp, prob >= 0.5)

Draft_2023 <- rbind(Draft_2023,tp)

Draft_2023[,"player_name"]

#####
# LOG REG - C
#####

df <- subset(Data_C, select = -c(player_name))

#make this example reproducible
set.seed(1)

#Use 70% of our dataset as training set and remaining 30% as testing set
sample <- sample(c(TRUE, FALSE), nrow(df), replace=TRUE, prob=c(0.7,0.3))
train  <- df[sample, ]
test   <- df[!sample, ]

logreg_PG <- glm(drafted ~., data = train, family = "binomial")
summary(logreg_PG)


### AIC : 253

#confint(logreg_PG)

#install.packages("aod")
library(aod)

#wald.test(b = coef(logreg_PG), Sigma = vcov(logreg_PG), Terms = 7:11)


# Predict test data based on model
predict_reg <- predict(logreg_PG, 
                       test, type = "response")
#predict_reg 

# Changing probabilities
predict_reg <- ifelse(predict_reg > 0.5, 1, 0)

# Evaluating model accuracy
# using confusion matrix
table <- confusionMatrix(data= as.factor(predict_reg), reference = as.factor(test$drafted),mode = "everything")
table

# Accuracy : 0.8955
# F1 : 0.9393
# Sensitivity : 0.9280
# Specificity : 0.6757

missing_classerr <- mean(predict_reg != test$drafted)
print(paste('Accuracy =', 1 - missing_classerr))

# Accuracy : 0.895

# computing model performance metrics
data.frame( R2 = R2(predict_reg, test$drafted),
            RMSE = RMSE(predict_reg, test$drafted),
            MAE = MAE(predict_reg, test$drafted))

### 0.32 R2 - 0.323 RMSE 

# ROC-AUC Curve
library(pROC)
roc_score = roc(test[,59], predict_reg) #AUC score
plot(roc_score ,main ="ROC curve -- Logistic Regression ")

### Let's eliminate some features by stepwise regression ! 

#define intercept-only model
intercept_only <- glm(drafted ~ 1, data = train)

#define model with all predictors
all <- glm(drafted ~ ., data=train)

#perform both direction stepwise regression
both <- step(intercept_only, direction='both', scope=formula(all), trace=0)

#view results of both² direction stepwise regression
both$anova

#view final model
both$coefficients

predict_reg <- predict(both, 
                       test, type = "response")

# computing model performance metrics
data.frame( R2 = R2(predict_reg, test$drafted),
            RMSE = RMSE(predict_reg, test$drafted),
            MAE = MAE(predict_reg, test$drafted))

### 0.316 R2 - 0.2776 RMSE 

C_draft.prob = predict(logreg_PG, Data_C_2023, type="response")

C_draft.prob

tp <- Data_C_2023
tp <- cbind(tp, prob = C_draft.prob)
tp <- subset(tp, prob >= 0.5)

Draft_2023 <- rbind(Draft_2023,tp)

Draft_2023[,"player_name"]


### We order the players by their probability of getting drafted !

Draft_order <- Draft_2023[order(-Draft_2023$prob),]

Draft_order[,"player_name"]

Draft_order = Draft_order[!duplicated(Draft_order$player_name), ]

Draft_order[,"player_name"]
