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


#install.packages('rstanarm')
library(rstanarm)

x <- as.data.frame(ncaa_PG.pcs[,1:15])
y <- unlist(subset(nba_PG, select = c(PER)))

PG_tp <- cbind(x, PER = y)

# We run a first bayesian linear regression model with default parameters !

model_bayes<- stan_glm(PER~. -PER, data=PG_tp, seed=111)

print(model_bayes, digits = 3)

#install.packages('bayesplot')
library(bayesplot)
mcmc_dens(model_bayes, pars = c("PC14"))+
  vline_at(-1.207, col="red")

mcmc_dens(model_bayes, pars=c("PC6"))+
  vline_at(0.627, col="red")

#install.packages('bayestestR')
library(bayestestR)
describe_posterior(model_bayes)

hdi(model_bayes)

eti(model_bayes)

# A lot of the Variables have 0 in their confidence interval - meaning their coefficient is non significant

# Otherwise if we just look at the ROPE %, we can keep the variables with low ROPE%

print(model_bayes, digits = 3)

# Prediction 2023 :

data_2023 <- subset(Data_PG_2023,select = -c(player_name))

data_2023 <- data_2023 %>% mutate_if(is.integer, as.numeric)

# Compute PC scores on 2023 data !

# center and scale the data
c.fun<-function(df, center, scale) {
  return((df-center)/scale )
}

centeredNewData<-apply(data_2023, MARGIN=1, FUN=c.fun, ncaa_PG.pca$center, ncaa_PG.pca$scale  )
pcsnew<-as.data.frame(t(t(ncaa_PG.pca$rotation) %*% centeredNewData))

x_2023 <- as.data.frame(subset(pcsnew, select = c(PC1,PC2,PC3,PC4,PC5,PC6,PC7,PC10,PC11,PC14,PC8,PC9,PC12,PC13,PC15)))


### Bayesian Model Averaging ! 

# calculate the model using stepAIC
step_model = stepAIC(lm(PER ~ . , data = PG_tp),direction="both")
step_model

#Step:  AIC=192.55
#PER ~ PC4 + PC6 + PC7 + PC8 + PC10 + PC12 + PC13 + PC14 + PC15

# run the BMA, specifying BIC as the standard by which the resulting models are judged
#install.packages('BAS')
library(BAS)


bma_PER = bas.lm(PER ~ . , data = PG_tp,
                   prior = "BIC", 
                   modelprior = uniform())

# display the results
summary(bma_PER)

image(bma_PER, top.models = 5, rotate=F, cex.axis = 1)

# making the prediction
y_predicted = predict(step_model, newdata = x_2023, estimator = "BMA")

otp <- as.data.frame(y_predicted)

otp <- cbind(otp, player_name = Data_PG_2023$player_name)

# Logan johnson - Cam Shelton - Omari Moore

# Again we see small, older PG


## Let's try predicting something else ! 

#### VORP

#VORP - Value Over Replacement Player (available since the 1973-74 season in the NBA); a box score estimate of the points per 100 TEAM possessions that a player contributed above a replacement-level
#(-2.0) player, translated to an average team and prorated to an 82-game season.

x <- as.data.frame(ncaa_PG.pcs[,1:15])
y <- unlist(subset(nba_PG, select = c(VORP)))

PG_tp <- cbind(x, VORP = y)

# We run a first bayesian linear regression model with default parameters !

model_bayes<- stan_glm(VORP~. , data=PG_tp, seed=111)

waic(model_bayes)

print(model_bayes, digits = 3)

describe_posterior(model_bayes)

hdi(model_bayes)

eti(model_bayes)

# A lot of the Variables have 0 in their confidence interval - meaning their coefficient is non significant

# Otherwise if we just look at the ROPE %, we can keep the variables with low ROPE%

print(model_bayes, digits = 3)

# Prediction 2023 :

data_2023 <- subset(Data_PG_2023,select = -c(player_name))

data_2023 <- data_2023 %>% mutate_if(is.integer, as.numeric)

# Compute PC scores on 2023 data !

# center and scale the data
c.fun<-function(df, center, scale) {
  return((df-center)/scale )
}

centeredNewData<-apply(data_2023, MARGIN=1, FUN=c.fun, ncaa_PG.pca$center, ncaa_PG.pca$scale  )
pcsnew<-as.data.frame(t(t(ncaa_PG.pca$rotation) %*% centeredNewData))

x_2023 <- as.data.frame(subset(pcsnew, select = c(PC1,PC2,PC3,PC4,PC5,PC6,PC7,PC10,PC11,PC14,PC8,PC9,PC12,PC13,PC15)))


### Bayesian Model Averaging ! 

# calculate the model using stepAIC
step_model = stepAIC(lm(VORP ~ . , data = PG_tp),direction="both")
step_model

# AIC = 23.31 !! way more precise than PER !

#waic(step_model)

#Step:  AIC=23.31
#VORP ~ PC4 + PC6 + PC7 + PC8 + PC10 + PC12 + PC14 + PC15


# run the BMA, specifying BIC as the standard by which the resulting models are judged
#install.packages('BAS')
library(BAS)

bma_VORP = bas.lm(VORP ~ . , data = PG_tp,
                 prior = "BIC", 
                 modelprior = uniform())

# display the results
summary(bma_VORP)

image(bma_VORP, top.models = 5, rotate=F, cex.axis = 1)

# making the prediction
y_predicted = predict(step_model, newdata = x_2023, estimator = "BMA")

otp <- cbind(otp, VORP = y_predicted)

# Cam shelton / Isaiah Moore + Dajuan Harris Jr. : Interesting bc not a high profile scorer -
# He is the defensive player of the year...So defense actually translates

#### Predict PC in the NBA using PC in the NCAA

nba_PG.pca <- prcomp(subset(nba_PG, select = -c(Player.x)), scale = TRUE)
nba_PG.pcs <- as.data.frame(nba_PG.pca$x)

### NBA PC1

PG_tp <- cbind(ncaa_PG.pcs[,1:15],NBA_PC1 = nba_PG.pcs[,1])

# We run a first bayesian linear regression model with default parameters !

model_bayes<- stan_glm(NBA_PC1~. , data=PG_tp, seed=111)

waic(model_bayes)

# High WAIC

print(model_bayes, digits = 3)

describe_posterior(model_bayes)

hdi(model_bayes)

eti(model_bayes)

# A lot of the Variables have 0 in their confidence interval - meaning their coefficient is non significant

# Otherwise if we just look at the ROPE %, we can keep the variables with low ROPE%

print(model_bayes, digits = 3)

# Prediction 2023 :

data_2023 <- subset(Data_PG_2023,select = -c(player_name))

data_2023 <- data_2023 %>% mutate_if(is.integer, as.numeric)

# Compute PC scores on 2023 data !

# center and scale the data
c.fun<-function(df, center, scale) {
  return((df-center)/scale )
}

centeredNewData<-apply(data_2023, MARGIN=1, FUN=c.fun, ncaa_PG.pca$center, ncaa_PG.pca$scale  )
pcsnew<-as.data.frame(t(t(ncaa_PG.pca$rotation) %*% centeredNewData))

x_2023 <- as.data.frame(subset(pcsnew, select = c(PC1,PC2,PC3,PC4,PC5,PC6,PC7,PC10,PC11,PC14,PC8,PC9,PC12,PC13,PC15)))


### Bayesian Model Averaging ! 

# calculate the model using stepAIC
step_model = stepAIC(lm(NBA_PC1 ~ . , data = PG_tp),direction="both")
step_model

# AIC = 193.21 !! way more precise than PER !

#waic(step_model)

#Step:  AIC = 193.21
#NBA_PC1 ~ PC4 + PC6 + PC7 + PC8 + PC10 + PC12 + PC13 + PC14 + PC15


# run the BMA, specifying BIC as the standard by which the resulting models are judged
#install.packages('BAS')
library(BAS)

bma_PC1 = bas.lm(NBA_PC1 ~ . , data = PG_tp,
                  prior = "BIC", 
                  modelprior = uniform())

# display the results
summary(bma_PC1)

image(bma_PC1, top.models = 5, rotate=F, cex.axis = 1)

# making the prediction
y_predicted = predict(step_model, newdata = x_2023, estimator = "BMA")

otp <- cbind(otp, NBA_PC1 = y_predicted)

# Here the PC14 is over-represented ! 

### NBA PC2

PG_tp <- cbind(ncaa_PG.pcs[,1:15],NBA_PC2 = nba_PG.pcs[,2])

# We run a first bayesian linear regression model with default parameters !

model_bayes<- stan_glm(NBA_PC2~. , data=PG_tp, seed=111)

waic(model_bayes)

# High WAIC

print(model_bayes, digits = 3)

describe_posterior(model_bayes)

hdi(model_bayes)

eti(model_bayes)

# A lot of the Variables have 0 in their confidence interval - meaning their coefficient is non significant

# Otherwise if we just look at the ROPE %, we can keep the variables with low ROPE%

print(model_bayes, digits = 3)

# Prediction 2023 :

data_2023 <- subset(Data_PG_2023,select = -c(player_name))

data_2023 <- data_2023 %>% mutate_if(is.integer, as.numeric)

# Compute PC scores on 2023 data !

# center and scale the data
c.fun<-function(df, center, scale) {
  return((df-center)/scale )
}

centeredNewData<-apply(data_2023, MARGIN=1, FUN=c.fun, ncaa_PG.pca$center, ncaa_PG.pca$scale  )
pcsnew<-as.data.frame(t(t(ncaa_PG.pca$rotation) %*% centeredNewData))

x_2023 <- as.data.frame(subset(pcsnew, select = c(PC1,PC2,PC3,PC4,PC5,PC6,PC7,PC10,PC11,PC14,PC8,PC9,PC12,PC13,PC15)))


### Bayesian Model Averaging ! 

# calculate the model using stepAIC
step_model = stepAIC(lm(NBA_PC2 ~ . , data = PG_tp),direction="both")
step_model


#Step:  AIC=145.83
#NBA_PC2 ~ PC13


# run the BMA, specifying BIC as the standard by which the resulting models are judged
#install.packages('BAS')
library(BAS)

bma_PC2 = bas.lm(NBA_PC2 ~ . , data = PG_tp,
                 prior = "BIC", 
                 modelprior = uniform())

# display the results
summary(bma_PC2)

image(bma_PC2, top.models = 5, rotate=F, cex.axis = 1)

# making the prediction
y_predicted = predict(step_model, newdata = x_2023, estimator = "BMA")

otp <- cbind(otp, NBA_PC2 = y_predicted)

# Now let us inspect the PC14 

#install.packages("devtools")
library("devtools")
library("factoextra")

var_PG <- get_pca_var(ncaa_PG.pca)
var_PG

# Contribution of variables
head(var_PG$contrib)

contribvar_PG <- as.data.frame(var_PG$contrib)

contribvar_PG <- contribvar_PG[order(contribvar_PG$Dim.14),]

# 10 highest contributors 

high_cont_PG <- contribvar_PG[48:58,]

var_names_PG <- rownames(high_cont_PG)
var_names_PG

# We see that the highest variables : yr/ dunks miss/ dunks made tend to favour physical and older players
# The blk and blk per is also quite high which explains why we see some defensive minded players !
