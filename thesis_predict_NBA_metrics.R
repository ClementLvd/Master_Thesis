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

Data <- subset(Data, select = -c(unknow,type,num,team,feet,inches))
Data_2023 <- subset(Data_2023, select = -c(unknow,type,num,team,feet,inches))

levels(as.factor(Data$conf))
levels(as.factor(Data$yr))

Data$conf <- as.numeric(as.factor(Data$conf))
Data_2023$conf <- as.numeric(as.factor(Data_2023$conf))

#DELETE PLAYERS THAT DID NOT PLAY
#players whose time playing is too low to be considered

Data<-subset(Data, Min_per >= 50.0);
Data_2023 <- subset(Data_2023, Min_per >= 70.0);

# Remove duplicates from the college players, and keep only the last season in college for each player, as their best one !

Data <- Data[order(Data$year, Data$player_name, decreasing=TRUE),]
Data <- Data[!duplicated(Data$player_name),]
Data <- subset(Data, select = -c(year))

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

#Data<-conc_dataf("posi",Data$posi,Data);
Data_2023 <- conc_dataf("posi", Data_2023$posi,Data_2023)

#####

### Now let's split by positions ! 
#####

#Data_PG <- subset(Data,(Pure.PG == 1 | Scoring.PG == 1 | Combo.G == 1), select = -c(Pure.PG,Scoring.PG,Combo.G,Wing.G,Wing.F,Stretch.4,PF.C,C)) ;

Data_PG_2023 <- subset(Data_2023,(Pure.PG == 1 | Scoring.PG == 1 | Combo.G == 1), select = -c(Pure.PG,Scoring.PG,Combo.G,Wing.G,Wing.F,Stretch.4,PF.C,C)) ;

#Data_SG <- subset(Data,(Wing.G == 1 | Scoring.PG == 1 | Combo.G == 1), select = -c(Pure.PG,Scoring.PG,Combo.G,Wing.G,Wing.F,Stretch.4,PF.C,C)); 

Data_SG_2023 <- subset(Data_2023,(Wing.G == 1 | Scoring.PG == 1 | Combo.G == 1), select = -c(Pure.PG,Scoring.PG,Combo.G,Wing.G,Wing.F,Stretch.4,PF.C,C));

#Data_SF <- subset(Data, (Wing.G == 1 | Wing.F == 1), select = -c(Pure.PG,Scoring.PG,Combo.G,Wing.G,Wing.F,Stretch.4,PF.C,C)) ;

Data_SF_2023 <- subset(Data_2023,(Wing.G == 1 | Wing.F == 1), select = -c(Pure.PG,Scoring.PG,Combo.G,Wing.G,Wing.F,Stretch.4,PF.C,C)) ;

#Data_PF <- subset(Data, (Stretch.4 == 1 | PF.C == 1), select = -c(Pure.PG,Scoring.PG,Combo.G,Wing.G,Wing.F,Stretch.4,PF.C,C)) ;

Data_PF_2023 <- subset(Data_2023,(Stretch.4 == 1 | PF.C == 1), select = -c(Pure.PG,Scoring.PG,Combo.G,Wing.G,Wing.F,Stretch.4,PF.C,C)) ;

#Data_C <- subset(Data,(C == 1 | PF.C == 1), select = -c(Pure.PG,Scoring.PG,Combo.G,Wing.G,Wing.F,Stretch.4,PF.C,C)) ;

Data_C_2023 <- subset(Data_2023,(C == 1 | PF.C == 1), select = -c(Pure.PG,Scoring.PG,Combo.G,Wing.G,Wing.F,Stretch.4,PF.C,C))

#####

#### Now let's work on NBA players' college stats

# Import

nba_adv_2023 <- read.csv("~/R EXAM/databases/nba_adv_2023.csv")
nba_base_2023 <- read.csv("~/R EXAM/databases/nba_base_2023.csv")

nba_adv_2019 <- read.csv("~/R EXAM/databases/nba_adv_2019.csv")
nba_base_2019 <- read.csv("~/R EXAM/databases/nba_base_2019.csv")

nba_adv_2015 <- read.csv("~/R EXAM/databases/nba_adv_2015.csv")
nba_base_2015 <- read.csv("~/R EXAM/databases/nba_base_2015.csv")

nba_adv_2013 <- read.csv("~/R EXAM/databases/nba_adv_2013.csv")
nba_base_2013 <- read.csv("~/R EXAM/databases/nba_base_2013.csv")

nba_adv_2021 <- read.csv("~/R EXAM/databases/nba_adv_2021.csv")
nba_base_2021 <- read.csv("~/R EXAM/databases/nba_base_2021.csv")

nba_tot_2023 <- merge(nba_adv_2023,nba_base_2023,by="Rk")
nba_tot_2019 <- merge(nba_adv_2019,nba_base_2019,by="Rk")
nba_tot_2015 <- merge(nba_adv_2015,nba_base_2015,by="Rk")
nba_tot_2013 <- merge(nba_adv_2013,nba_base_2013,by="Rk")
nba_tot_2021 <- merge(nba_adv_2021,nba_base_2021,by="Rk")


# So we do have a database of stats for players actually playing in the NBA 

# Our goal is to find each of these players stats in college

## And take these college stats to predict their impact in modern NBA

# To do this : We will isolate players playing in the NBA

# Make a PCA by position !

# And then go and search their college stats --> Build a network 
# Target --> Principal Components in the NBA OR Win Shares/PER I don't know

# Use this to predict the way the 2023 class will play in the NBA ! 

# Remove duplicates on selected columns
nba_tot_2023 <- nba_tot_2023 %>% distinct(Player.x,Tm.x, .keep_all = TRUE)
nba_tot_2019 <- nba_tot_2019 %>% distinct(Player.x,Tm.x, .keep_all = TRUE)
nba_tot_2015 <- nba_tot_2015 %>% distinct(Player.x,Tm.x, .keep_all = TRUE)
nba_tot_2013 <- nba_tot_2013 %>% distinct(Player.x,Tm.x, .keep_all = TRUE)
nba_tot_2021 <- nba_tot_2021 %>% distinct(Player.x,Tm.x, .keep_all = TRUE)

nba_tot_2023 <- subset(nba_tot_2023, G.x >= 30 & MP.y >= 12.0, select = -c(Player.y,Pos.y,Tm.y,Age.y,G.y,Column1,X2))
nba_tot_2019 <- subset(nba_tot_2019, G.x >= 30 & MP.y >= 12.0, select = -c(Player.y,Pos.y,Tm.y,Age.y,G.y,Column1,X2))
nba_tot_2015 <- subset(nba_tot_2015, G.x >= 30 & MP.y >= 12.0, select = -c(Player.y,Pos.y,Tm.y,Age.y,G.y,Column1,X2))
nba_tot_2013 <- subset(nba_tot_2013, G.x >= 30 & MP.y >= 12.0, select = -c(Player.y,Pos.y,Tm.y,Age.y,G.y,Column1,X2))
nba_tot_2021 <- subset(nba_tot_2021, G.x >= 30 & MP.y >= 12.0, select = -c(Player.y,Pos.y,Tm.y,Age.y,G.y,Column1,X2))


nba_tot <- rbind(nba_tot_2023,nba_tot_2021)
nba_tot <- rbind(nba_tot,nba_tot_2019)
nba_tot <- rbind(nba_tot,nba_tot_2015)
nba_tot <- rbind(nba_tot,nba_tot_2013)

# We have isolated the players playing a significant role in the current NBA !

# Let's now search these player's college stats

transferred_players <- subset(nba_tot, Tm.x == 'TOT') 

# We isolate players that have been transferred during a season - to keep their total stats and not the individual stats in the teams

names_tf <- transferred_players$Player.x

nba_tot <- subset(nba_tot, !(Player.x %in% names_tf))

nba_tot <- rbind(nba_tot, transferred_players)

nba_PG <- subset(nba_tot, Pos.x == "PG", select = -c(Pos.x, Tm.x))
nba_SG <- subset(nba_tot, Pos.x == "SG", select = -c(Pos.x, Tm.x))
nba_SF <- subset(nba_tot, Pos.x == "SF", select = -c(Pos.x, Tm.x))
nba_PF <- subset(nba_tot, Pos.x == "PF", select = -c(Pos.x, Tm.x))
nba_C <- subset(nba_tot, Pos.x == "C", select = -c(Pos.x, Tm.x))

# We will try to build a model that predict stats in the NBA using stats in college !

names_PG <- nba_PG$Player.x
names_PG
#280 PGs, but how many of them went to college ?

names_SG <- nba_SG$Player.x
names_SF <- nba_SF$Player.x
names_PF <- nba_PF$Player.x
names_C <- nba_C$Player.x

#Isolate by positions
library(tidyverse)

ncaa_PG <- subset(Data, player_name %in% names_PG, select = -c(posi))

# Only 83 PGs...--> We have players that have several season in the NBA but one in the NBA
# + Foreign players + players drafted before 2009

ncaa_SG <- subset(Data, player_name %in% names_SG, select = -c(posi))
ncaa_SF <- subset(Data, player_name %in% names_SF, select = -c(posi))
ncaa_PF <- subset(Data, player_name %in% names_PF, select = -c(posi))
ncaa_C <- subset(Data, player_name %in% names_C, select = -c(posi))

# Now let's use these stats to predict some interesting metrics ! 

# We notice that we have not enough observations compared to the number of variables (100 vs 40)

# Let's reduce that using PCA !

ncaa_PG.pca <- prcomp(subset(ncaa_PG[1:59],select = -c(player_name)), scale = TRUE)
ncaa_PG.pcs <- as.data.frame(ncaa_PG.pca$x)
      

# Select the top 200 contributing individuals
fviz_pca_ind(ncaa_PG.pca,label="var", habillage=ncaa_PG$drafted,
             addEllipses=TRUE, ellipse.level=0.95, select.ind = list(contrib = 200))

# Let's perform a lasso regression 
# One of the first variables we will try to predict is the PER (Player Efficiency Rating)

ncaa_PG.names <- ncaa_PG[,1]

ncaa_PG.names

nba_PG <- subset(nba_PG, Player.x %in% ncaa_PG.names)

# We only keep the season where the players paid the most minutes ! 

nba_PG <-nba_PG[order(nba_PG$MP.x, decreasing=TRUE),]
nba_PG <- nba_PG[!duplicated(nba_PG$Player.x),]

x <- data.matrix(ncaa_PG.pcs[,1:15])
y <- unlist(subset(nba_PG, select = c(PER)))

# Lasso regression for PER

library(glmnet)

#perform k-fold cross-validation to find optimal lambda value
cv_model <- cv.glmnet(x, y, alpha = 1)

#find optimal lambda value that minimizes test MSE
best_lambda <- cv_model$lambda.min
best_lambda

# 0.23

#produce plot of test MSE by lambda value
plot(cv_model) 

#find coefficients of best model
best_model <- glmnet(x, y, alpha = 1, lambda = best_lambda)
coef(best_model)

#use fitted best model to make predictions
y_predicted <- predict(best_model, s = best_lambda, newx = x)

#find SST and SSE
sst <- sum((y - mean(y))^2)
sse <- sum((y_predicted - y)^2)

#find R-Squared
rsq <- 1 - sse/sst
rsq

# 0.39301

# Now we take only the PCs that matter !

x <- data.matrix(subset(ncaa_PG.pcs, select = c(PC7, PC8, PC6, PC10 , PC12 , PC13 , PC15 , PC14)))

#perform k-fold cross-validation to find optimal lambda value
cv_model <- cv.glmnet(x, y, alpha = 1)

#find optimal lambda value that minimizes test MSE
best_lambda <- cv_model$lambda.min
best_lambda

#0.005605453

#produce plot of test MSE by lambda value
plot(cv_model) 

#find coefficients of best model
best_model <- glmnet(x, y, alpha = 1, lambda = best_lambda)
coef(best_model)

#use fitted best model to make predictions
y_predicted <- predict(best_model, s = best_lambda, newx = x)

#find SST and SSE
sst <- sum((y - mean(y))^2)
sse <- sum((y_predicted - y)^2)

#find R-Squared
rsq <- 1 - sse/sst
rsq

# SAME !

# we will use it to predict this year ! 

# Prediction 2023 :

data_2023 <- subset(Data_PG_2023,select = -c(player_name)) %>% mutate_if(is.integer, as.numeric)

# Compute PCA coordinates of the new data
pcsnew <- as.data.frame(scale(data_2023, PG.pca$center, PG.pca$scale) %*% PG.pca$rotation)

pcsnew <- pcsnew[,1:15]

x_2023 <- data.matrix(subset(pcsnew, select = c(PC7, PC8, PC6, PC10 , PC12 , PC13 , PC15 , PC14)))

y_predicted <- predict(best_model, s = best_lambda, newx = x_2023)

otp <- as.data.frame(y_predicted)

otp <- cbind(otp, player_name = Data_PG_2023$player_name)

# the highest performer aren't on the big boards !

#install.packages("plotly")
library(plotly)

tp_PG <- as.data.frame(x)
tp_PG <- cbind(tp_PG,y)

fig <- plot_ly(tp_PG, x = ~PC15, y = ~PC14, z = ~y)
fig <- fig %>% add_markers()
fig <- fig %>% layout(scene = list(xaxis = list(title = 'PC15 college'),
                                   yaxis = list(title = 'PC14 college'),
                                   zaxis = list(title = 'NBA PER')),
               )

fig

## Let's try predicting something else ! 

#### VORP

#VORP - Value Over Replacement Player (available since the 1973-74 season in the NBA); a box score estimate of the points per 100 TEAM possessions that a player contributed above a replacement-level
#(-2.0) player, translated to an average team and prorated to an 82-game season.

x <- data.matrix(ncaa_PG.pcs[,1:15])
y <- unlist(subset(nba_PG, select = c(VORP)))

# Lasso regression for PER

library(glmnet)

#perform k-fold cross-validation to find optimal lambda value
cv_model <- cv.glmnet(x, y, alpha = 1)

#find optimal lambda value that minimizes test MSE
best_lambda <- cv_model$lambda.min
best_lambda

# 0.0652604

#produce plot of test MSE by lambda value
plot(cv_model) 

#find coefficients of best model
best_model <- glmnet(x, y, alpha = 1, lambda = best_lambda)
coef(best_model)

# PC15 / PC10 PC8 PC6 PC7 PC12/ PC14

#use fitted best model to make predictions
y_predicted <- predict(best_model, s = best_lambda, newx = x)

#find SST and SSE
sst <- sum((y - mean(y))^2)
sse <- sum((y_predicted - y)^2)

#find R-Squared
rsq <- 1 - sse/sst
rsq

# 0.3417914

# Now let us keep only interesting PC

# Now we take only the PCs that matter !

x <- data.matrix(subset(ncaa_PG.pcs, select = c(PC15, PC10, PC8, PC6, PC7, PC12, PC14)))

#perform k-fold cross-validation to find optimal lambda value
cv_model <- cv.glmnet(x, y, alpha = 1)

#find optimal lambda value that minimizes test MSE
best_lambda <- cv_model$lambda.min
best_lambda

# 0.001733374

#produce plot of test MSE by lambda value
plot(cv_model) 

#find coefficients of best model
best_model <- glmnet(x, y, alpha = 1, lambda = best_lambda)
coef(best_model)

#use fitted best model to make predictions
y_predicted <- predict(best_model, s = best_lambda, newx = x)

#find SST and SSE
sst <- sum((y - mean(y))^2)
sse <- sum((y_predicted - y)^2)

#find R-Squared
rsq <- 1 - sse/sst
rsq

#0.3169031

# WORSE !

tp_PG <- as.data.frame(x)
tp_PG <- cbind(tp_PG,y)

fig <- plot_ly(tp_PG, x = ~PC15, y = ~PC14, z = ~y)
fig <- fig %>% add_markers()
fig <- fig %>% layout(scene = list(xaxis = list(title = 'PC15 college'),
                                   yaxis = list(title = 'PC14 college'),
                                   zaxis = list(title = 'NBA VORP')),
)

fig

#### Predict PC in the NBA using PC in the NCAA

nba_PG.pca <- prcomp(subset(nba_PG, select = -c(Player.x)), scale = TRUE)
nba_PG.pcs <- as.data.frame(nba_PG.pca$x)


# Select the top 200 contributing individuals
fviz_pca_ind(nba_PG.pca,label="var",
              select.ind = list(contrib = 200))

# Now let us build a model to predict PC1 and PC2 in the NBA using PCs in the NCAA !!

# First PC1 ! 

x <- data.matrix(ncaa_PG.pcs[,1:15])
y <- nba_PG.pcs[,1]

tp_PG <- cbind(ncaa_PG.pcs[,1:15],NBA_PC1 = nba_PG.pcs[,1])

tp_PG <- cbind(tp_PG, NBA_PC2 = nba_PG.pcs[,2])

fig <- plot_ly(tp_PG, x = ~PC1, y = ~PC2, z = ~NBA_PC1 ,marker = list(color = ~NBA_PC2, colorscale = c('#FFE1A1', '#683531'), showscale = TRUE))
fig <- fig %>% add_markers()
fig <- fig %>% layout(scene = list(xaxis = list(title = 'PC1 college'),
                                   yaxis = list(title = 'PC2 college'),
                                   zaxis = list(title = 'NBA PC1')),
                      title = tp_PG$player_name)

fig

fig <- plot_ly(tp_PG, x = ~PC1, y = ~PC2, z = ~NBA_PC2 ,marker = list(color = ~NBA_PC1, colorscale = c('#FFE1A1', '#683531'), showscale = TRUE))
fig <- fig %>% add_markers()
fig <- fig %>% layout(scene = list(xaxis = list(title = 'PC1 college'),
                                   yaxis = list(title = 'PC2 college'),
                                   zaxis = list(title = 'NBA PC2')),
                      title = tp_PG$player_name)

fig

# We notice a form of polynomial model in the figure with PC1 and PC2 

PolynomMod <- lm(NBA_PC1 ~ PC1 + I(PC1^2) + PC2 + I(PC2^2), data = tp_PG)  

summary(PolynomMod)

# Not at all....

# Let's perform lasso regression 

library(glmnet)

#perform k-fold cross-validation to find optimal lambda value
cv_model <- cv.glmnet(x, y, alpha = 1)

#find optimal lambda value that minimizes test MSE
best_lambda <- cv_model$lambda.min
best_lambda

# 0.2773416

#produce plot of test MSE by lambda value
plot(cv_model) 

#find coefficients of best model
best_model <- glmnet(x, y, alpha = 1, lambda = best_lambda)
coef(best_model)

# Keep only PC14 PC15 - PC8 - PC13 - PC12- PC7 - PC6 -PC10

#use fitted best model to make predictions
y_predicted <- predict(best_model, s = best_lambda, newx = x)

#find SST and SSE
sst <- sum((y - mean(y))^2)
sse <- sum((y_predicted - y)^2)

#find R-Squared
rsq <- 1 - sse/sst
rsq

# Decent R2 !! 0.5286884

x <- data.matrix(subset(ncaa_PG.pcs, select = c(PC14, PC15, PC8, PC13, PC12, PC7, PC6, PC10)))
y <- nba_PG.pcs[,1]

#perform k-fold cross-validation to find optimal lambda value
cv_model <- cv.glmnet(x, y, alpha = 1)

#find optimal lambda value that minimizes test MSE
best_lambda <- cv_model$lambda.min
best_lambda

# 0.007366436

#produce plot of test MSE by lambda value
plot(cv_model) 

#find coefficients of best model
best_model <- glmnet(x, y, alpha = 1, lambda = best_lambda)
coef(best_model)

#use fitted best model to make predictions
y_predicted <- predict(best_model, s = best_lambda, newx = x)

#find SST and SSE
sst <- sum((y - mean(y))^2)
sse <- sum((y_predicted - y)^2)

#find R-Squared
rsq <- 1 - sse/sst
rsq

#R2 is the same ! 

# Now let use this model to predict this year's PC1

x_2023 <- data.matrix(subset(pcsnew, select = c(PC14, PC15, PC8, PC13, PC12, PC7, PC6, PC10)))

pc1_predicted <- predict(best_model, s = best_lambda, newx = x_2023)

# Now PC2 ! 

x <- data.matrix(ncaa_PG.pcs[,1:15])
y <- nba_PG.pcs[,2]

tp_PG <- cbind(ncaa_PG.pcs[,1:15],NBA_PC1 = nba_PG.pcs[,1])

tp_PG <- cbind(tp_PG, NBA_PC2 = nba_PG.pcs[,2])


fig <- plot_ly(tp_PG, x = ~PC1, y = ~PC2, z = ~NBA_PC2 ,marker = list(color = ~NBA_PC1, colorscale = c('#FFE1A1', '#683531'), showscale = TRUE))
fig <- fig %>% add_markers()
fig <- fig %>% layout(scene = list(xaxis = list(title = 'PC1 college'),
                                   yaxis = list(title = 'PC2 college'),
                                   zaxis = list(title = 'NBA PC2')),
                      title = tp_PG$player_name)

fig

# We notice a form of polynomial model in the figure with PC1 and PC2 

PolynomMod <- lm(NBA_PC2 ~ PC1 + I(PC1^2) + PC2 + I(PC2^2), data = tp_PG)  

summary(PolynomMod)

#perform k-fold cross-validation to find optimal lambda value
cv_model <- cv.glmnet(x, y, alpha = 1)

#find optimal lambda value that minimizes test MSE
best_lambda <- cv_model$lambda.min
best_lambda

# 0.3947256

#produce plot of test MSE by lambda value
plot(cv_model) 

#find coefficients of best model
best_model <- glmnet(x, y, alpha = 1, lambda = best_lambda)
coef(best_model)

# Keep only PC13...

#use fitted best model to make predictions
y_predicted <- predict(best_model, s = best_lambda, newx = x)

#find SST and SSE
sst <- sum((y - mean(y))^2)
sse <- sum((y_predicted - y)^2)

#find R-Squared
rsq <- 1 - sse/sst
rsq

# Impossible !!! 

# Let's plot PC1 and PER..

plot(otp$s1,pc1_predicted,main="PER and PC1 predicted",col.main='blue',xlab="PER prediction",ylab="PC1 prediction")

text(otp$s1+0.15,pc1_predicted + 0.25,otp$player_name,col='red')

#TAEVION KINSEY ! 