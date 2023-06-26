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

#####
# Bayesian Network PG
#####

data <- subset(Data_PG, select = -c(player_name))

library(dplyr)
data <- data %>% mutate_if(is.integer, as.numeric)

#make this example reproducible
set.seed(1)

#Use 70% of our dataset as training set and remaining 30% as testing set
sample <- sample(c(TRUE, FALSE), nrow(data), replace=TRUE, prob=c(0.7,0.3))
train  <- data[sample, ]
test  <- data[!sample, ]



train %<>% 
  mutate(conf= ntile(conf,10))
train %<>% 
  mutate(GP= ntile(GP,10))
train %<>% 
  mutate(Min_per= ntile(Min_per,10))
train %<>% 
  mutate(ORtg= ntile(ORtg,10))
train %<>% 
  mutate(usg= ntile(usg,10))
train %<>% 
  mutate(eFG= ntile(eFG,10))
train %<>% 
  mutate(TS_per= ntile(TS_per,10))
train %<>% 
  mutate(ORB_per= ntile(ORB_per,10))
train %<>% 
  mutate(DRB_per= ntile(DRB_per,10))
train %<>% 
  mutate(AST_per= ntile(AST_per,10))
train %<>% 
  mutate(TO_per= ntile(TO_per,10))
train %<>% 
  mutate(FTM= ntile(FTM,10))
train %<>% 
  mutate(FTA= ntile(FTA,10))
train %<>% 
  mutate(FT_per= ntile(FT_per,10))
train %<>% 
  mutate(twoPM= ntile(twoPM,10))
train %<>% 
  mutate(twoPA= ntile(twoPA,10))
train %<>% 
  mutate(twoP_per= ntile(twoP_per,10))
train %<>% 
  mutate(TPM= ntile(TPM,10))
train %<>% 
  mutate(TPA= ntile(TPA,10))
train %<>% 
  mutate(TP_per= ntile(TP_per,10))
train %<>% 
  mutate(blk_per= ntile(blk_per,10))
train %<>% 
  mutate(stl_per= ntile(stl_per,10))
train %<>% 
  mutate(ftr= ntile(ftr,10))
train %<>% 
  mutate(yr= ntile(yr,10))
train %<>% 
  mutate(porpag= ntile(porpag,10))
train %<>% 
  mutate(adjoe= ntile(adjoe,10))
train %<>% 
  mutate(pfr= ntile(pfr,10))
train %<>% 
  mutate(pid= ntile(pid,10))
train %<>% 
  mutate(Rec.Rank= ntile(Rec.Rank,10))
train %<>% 
  mutate(X.ast.tov= ntile(X.ast.tov,10))
train %<>% 
  mutate(X.rimmade= ntile(X.rimmade,10))
train %<>% 
  mutate(X.rimmade.rimmiss= ntile(X.rimmade.rimmiss,10))
train %<>% 
  mutate(X.midmade= ntile(X.midmade,10))
train %<>% 
  mutate(X.midmade.midmiss= ntile(X.midmade.midmiss,10))
train %<>% 
  mutate(X.rimmade..rimmade.rimmiss.= ntile(X.rimmade..rimmade.rimmiss.,10))
train %<>% 
  mutate(X.midmade..midmade.midmiss.= ntile(X.midmade..midmade.midmiss.,10))
train %<>% 
  mutate(X.dunksmade= ntile(X.dunksmade,10))
train %<>% 
  mutate(X.dunksmiss.dunksmade= ntile(X.dunksmiss.dunksmade,10))
train %<>% 
  mutate(X.dunksmade..dunksmade.dunksmiss.= ntile(X.dunksmade..dunksmade.dunksmiss.,10))
train %<>% 
  mutate(X.drtg= ntile(X.drtg,10))
train %<>% 
  mutate(adrtg= ntile(adrtg,10))
train %<>% 
  mutate(X.dporpag= ntile(X.dporpag,10))
train %<>% 
  mutate(X.stops= ntile(X.stops,10))
train %<>% 
  mutate(X.bpm= ntile(X.bpm,10))
train %<>% 
  mutate(X.obpm= ntile(X.obpm,10))
train %<>% 
  mutate(X.dbpm= ntile(X.dbpm,10))
train %<>% 
  mutate(X.gbpm= ntile(X.gbpm,10))
train %<>% 
  mutate(mp= ntile(mp,10))
train %<>% 
  mutate(ogbpm= ntile(ogbpm,10))
train %<>% 
  mutate(dgbpm= ntile(dgbpm,10))
train %<>% 
  mutate(oreb= ntile(oreb,10))
train %<>% 
  mutate(dreb= ntile(dreb,10))
train %<>% 
  mutate(treb= ntile(treb,10))
train %<>% 
  mutate(ast= ntile(ast,10))
train %<>% 
  mutate(stl= ntile(stl,10))
train %<>% 
  mutate(blk= ntile(blk,10))
train %<>% 
  mutate(pts= ntile(pts,10))
train %<>% 
  mutate(ht_cm= ntile(ht_cm,10))

index <- 1:ncol(train)
train[ , index] <- lapply(train[ , index], as.factor)
str(train)

tancl = tree.bayes(train, training = "drafted")
graphviz.plot(tancl)


test %<>% 
  mutate(conf= ntile(conf,10))
test %<>% 
  mutate(GP= ntile(GP,10))
test %<>% 
  mutate(Min_per= ntile(Min_per,10))
test %<>% 
  mutate(ORtg= ntile(ORtg,10))
test %<>% 
  mutate(usg= ntile(usg,10))
test %<>% 
  mutate(eFG= ntile(eFG,10))
test %<>% 
  mutate(TS_per= ntile(TS_per,10))
test %<>% 
  mutate(ORB_per= ntile(ORB_per,10))
test %<>% 
  mutate(DRB_per= ntile(DRB_per,10))
test %<>% 
  mutate(AST_per= ntile(AST_per,10))
test %<>% 
  mutate(TO_per= ntile(TO_per,10))
test %<>% 
  mutate(FTM= ntile(FTM,10))
test %<>% 
  mutate(FTA= ntile(FTA,10))
test %<>% 
  mutate(FT_per= ntile(FT_per,10))
test %<>% 
  mutate(twoPM= ntile(twoPM,10))
test %<>% 
  mutate(twoPA= ntile(twoPA,10))
test %<>% 
  mutate(twoP_per= ntile(twoP_per,10))
test %<>% 
  mutate(TPM= ntile(TPM,10))
test %<>% 
  mutate(TPA= ntile(TPA,10))
test %<>% 
  mutate(TP_per= ntile(TP_per,10))
test %<>% 
  mutate(blk_per= ntile(blk_per,10))
test %<>% 
  mutate(stl_per= ntile(stl_per,10))
test %<>% 
  mutate(ftr= ntile(ftr,10))
test %<>% 
  mutate(yr= ntile(yr,10))
test %<>% 
  mutate(porpag= ntile(porpag,10))
test %<>% 
  mutate(adjoe= ntile(adjoe,10))
test %<>% 
  mutate(pfr= ntile(pfr,10))
test %<>% 
  mutate(pid= ntile(pid,10))
test %<>% 
  mutate(Rec.Rank= ntile(Rec.Rank,10))
test %<>% 
  mutate(X.ast.tov= ntile(X.ast.tov,10))
test %<>% 
  mutate(X.rimmade= ntile(X.rimmade,10))
test %<>% 
  mutate(X.rimmade.rimmiss= ntile(X.rimmade.rimmiss,10))
test %<>% 
  mutate(X.midmade= ntile(X.midmade,10))
test %<>% 
  mutate(X.midmade.midmiss= ntile(X.midmade.midmiss,10))
test %<>% 
  mutate(X.rimmade..rimmade.rimmiss.= ntile(X.rimmade..rimmade.rimmiss.,10))
test %<>% 
  mutate(X.midmade..midmade.midmiss.= ntile(X.midmade..midmade.midmiss.,10))
test %<>% 
  mutate(X.dunksmade= ntile(X.dunksmade,10))
test %<>% 
  mutate(X.dunksmiss.dunksmade= ntile(X.dunksmiss.dunksmade,10))
test %<>% 
  mutate(X.dunksmade..dunksmade.dunksmiss.= ntile(X.dunksmade..dunksmade.dunksmiss.,10))
test %<>% 
  mutate(X.drtg= ntile(X.drtg,10))
test %<>% 
  mutate(adrtg= ntile(adrtg,10))
test %<>% 
  mutate(X.dporpag= ntile(X.dporpag,10))
test %<>% 
  mutate(X.stops= ntile(X.stops,10))
test %<>% 
  mutate(X.bpm= ntile(X.bpm,10))
test %<>% 
  mutate(X.obpm= ntile(X.obpm,10))
test %<>% 
  mutate(X.dbpm= ntile(X.dbpm,10))
test %<>% 
  mutate(X.gbpm= ntile(X.gbpm,10))
test %<>% 
  mutate(mp= ntile(mp,10))
test %<>% 
  mutate(ogbpm= ntile(ogbpm,10))
test %<>% 
  mutate(dgbpm= ntile(dgbpm,10))
test %<>% 
  mutate(oreb= ntile(oreb,10))
test %<>% 
  mutate(dreb= ntile(dreb,10))
test %<>% 
  mutate(treb= ntile(treb,10))
test %<>% 
  mutate(ast= ntile(ast,10))
test %<>% 
  mutate(stl= ntile(stl,10))
test %<>% 
  mutate(blk= ntile(blk,10))
test %<>% 
  mutate(pts= ntile(pts,10))
test %<>% 
  mutate(ht_cm= ntile(ht_cm,10))

index <- 1:ncol(test)
test[ , index] <- lapply(test[ , index], as.factor)
str(test)

tancl.trained = bn.fit(tancl, train)
pred.maxlik = predict(tancl.trained, node = "drafted", test, method = "parents")
pred.bayes = predict(tancl.trained, node = "drafted", test, method = "bayes-lw")

table <- confusionMatrix(data= pred.maxlik, reference = test$drafted ,mode = "everything")
table

# P-Value [Acc > NIR] : 1 
#Accuracy : 0.6363 
# F1 : 0.9802
# Sensitivity : 0.66721   
# Specificity : 0.4306   
# Balanced Accuracy : 0.7120  

# computing model performance metrics
data.frame( R2 = R2(as.numeric(pred.maxlik), as.numeric(test$drafted)))

### 0.111

##NAIVE BAYES ! 

nbcl = naive.bayes(train,training = "drafted")
graphviz.plot(nbcl, layout = "fdp")

nbcl.trained = bn.fit(nbcl, train)

pred.maxlik = predict(nbcl.trained, node = "drafted", test, method = "parents")
pred.bayes = predict(nbcl.trained, node = "drafted", test, method = "bayes-lw")

table <- confusionMatrix(data= pred.maxlik, reference = test$drafted ,mode = "everything")
table

# P-Value [Acc > NIR] : 1 
# Accuracy : 0.8803 
# F1 : 0.9802
# Sensitivity : 0.8863  
# Specificity : 0.7778   
# Balanced Accuracy : 0.8321  

# computing model performance metrics
data.frame( R2 = R2(as.numeric(pred.maxlik), as.numeric(test$drafted)))

### 0.1810625

coef(tancl.trained$drafted)
coef(nbcl.trained$drafted)


#### TOO MANY FEATURES ! 

# Let's make a pca and take the most significant components ! 

#install.packages('tidyr')
#install.packages("factoextra")

library(factoextra)
library(tidyr)

#####
# GOOD PCA - PG
#####


data <- subset(Data_PG, select = -c(player_name,drafted))

data <- data %>% mutate_if(is.integer, as.numeric)

PG.pca <- prcomp(data, scale = TRUE)

fviz_eig(PG.pca)

x <- PG.pca$rotation

#Rotate the PCA according to varimax

ncomp <- 15

rawLoadings     <- x[,1:ncomp] %*% diag(PG.pca$sdev, ncomp, ncomp)

rotatedLoadings <- varimax(rawLoadings)$loadings
invLoadings     <- t(pracma::pinv(rotatedLoadings))
scores          <- scale(data[,1:58]) %*% invLoadings

print(scores[1:5,])  

# Select the top 200 contributing individuals
fviz_pca_ind(PG.pca,label="var", habillage=Data_PG$drafted,
             addEllipses=TRUE, ellipse.level=0.95, select.ind = list(contrib = 200))

# Select the top 5 contributing variables
fviz_pca_var(PG.pca, select.var = list(contrib = 5))

# 50 best indiv + 5 best variables
fviz_pca_biplot(PG.pca, label="var", habillage=Data_PG$drafted,
                addEllipses=TRUE, ellipse.level=0.95, select.ind = list(contrib = 50), select.var = list(contrib = 5))

#porpag, adjoe, offensive plus minus ! 

summary(PG.pca)



#####
# GOOD PCA - SG
#####

data <- subset(Data_SG, select = -c(player_name,drafted))

data <- data %>% mutate_if(is.integer, as.numeric)

SG.pca <- prcomp(data, scale = TRUE)

fviz_eig(SG.pca)

# Select the top 200 contributing individuals
fviz_pca_ind(SG.pca,label="var", habillage=Data_SG$drafted,
             addEllipses=TRUE, ellipse.level=0.95, select.ind = list(contrib = 200))

# Select the top 3 contributing variables
fviz_pca_var(SG.pca, select.var = list(contrib = 3))

# 200 best indiv + 20 best variables
fviz_pca_biplot(SG.pca, label="var", habillage=Data_SG$drafted,
                addEllipses=TRUE, ellipse.level=0.95, select.ind = list(contrib = 200), select.var = list(contrib = 20))

#porpag, adjoe, offensive plus minus ! 

summary(SG.pca)

#####
# GOOD PCA - SF
#####

data <- subset(Data_SF, select = -c(player_name,drafted))

data <- data %>% mutate_if(is.integer, as.numeric)

SF.pca <- prcomp(data, scale = TRUE)

fviz_eig(SF.pca)

# Select the top 200 contributing individuals
fviz_pca_ind(SF.pca,label="var", habillage=Data_SF$drafted,
             addEllipses=TRUE, ellipse.level=0.95, select.ind = list(contrib = 200))

# Select the top 3 contributing variables
fviz_pca_var(SF.pca, select.var = list(contrib = 3))

# 200 best indiv + 20 best variables
fviz_pca_biplot(SF.pca, label="var", habillage=Data_SF$drafted,
                addEllipses=TRUE, ellipse.level=0.95, select.ind = list(contrib = 200), select.var = list(contrib = 20))

#porpag, adjoe, offensive plus minus ! 

summary(SF.pca)

#####
# GOOD PCA - PF
#####

data <- subset(Data_PF, select = -c(player_name,drafted))

data <- data %>% mutate_if(is.integer, as.numeric)

PF.pca <- prcomp(data, scale = TRUE)

fviz_eig(PF.pca)

# Select the top 200 contributing individuals
fviz_pca_ind(PF.pca,label="var", habillage=Data_PF$drafted,
             addEllipses=TRUE, ellipse.level=0.95, select.ind = list(contrib = 200))

# Select the top 3 contributing variables
fviz_pca_var(PF.pca, select.var = list(contrib = 3))

# 200 best indiv + 20 best variables
fviz_pca_biplot(PF.pca, label="var", habillage=Data_PF$drafted,
                addEllipses=TRUE, ellipse.level=0.95, select.ind = list(contrib = 200), select.var = list(contrib = 20))

#porpag, adjoe, offensive plus minus ! 

summary(PF.pca)
#####
# GOOD PCA - C
#####

data <- subset(Data_C, select = -c(player_name,drafted))

data <- data %>% mutate_if(is.integer, as.numeric)

C.pca <- prcomp(data, scale = TRUE)

fviz_eig(C.pca)

# Select the top 200 contributing individuals
fviz_pca_ind(C.pca,label="var", habillage=Data_C$drafted,
             addEllipses=TRUE, ellipse.level=0.95, select.ind = list(contrib = 200))

# Select the top 3 contributing variables
fviz_pca_var(C.pca, select.var = list(contrib = 3))

# 200 best indiv + 20 best variables
fviz_pca_biplot(C.pca, label="var", habillage=Data_C$drafted,
                addEllipses=TRUE, ellipse.level=0.95, select.ind = list(contrib = 200), select.var = list(contrib = 20))

#porpag, adjoe, offensive plus minus ! 

summary(PF.pca)





#####
 


#####
# BN - PG
#####

Pg_pc <- as.data.frame(PG.pca$x)

Pg_pc <- cbind(Data_PG$drafted)


pc.pg <- as.data.frame(PG.pca$x)
pc.pg <- pc.pg[,1:15]
pc.pg <- cbind(pc.pg, drafted = Data_PG$drafted)

## ANOVA

one.way <- aov(drafted ~ PC1, data = pc.pg)
summary(one.way)

## MANOVA

res.man <- manova(cbind(PC1, PC2, PC3, PC4, PC5, PC6) ~ drafted, data = pc.pg)
summary(res.man)

summary.aov(res.man)


library(magrittr)
pc.pg %<>% 
  mutate(PC1 = ntile(PC1,10))
pc.pg %<>% 
  mutate(PC2 = ntile(PC2,10))
pc.pg %<>% 
  mutate(PC3 = ntile(PC3,10))
pc.pg %<>% 
  mutate(PC4 = ntile(PC4,10))
pc.pg %<>% 
  mutate(PC5 = ntile(PC5,10))
pc.pg %<>% 
  mutate(PC6 = ntile(PC6,10))
pc.pg %<>%
  mutate(PC7 = ntile(PC7,10))
pc.pg %<>% 
  mutate(PC8 = ntile(PC8,10))
pc.pg %<>% 
  mutate(PC9 = ntile(PC9,10))
pc.pg %<>% 
  mutate(PC10 = ntile(PC10,10))
pc.pg %<>% 
  mutate(PC11 = ntile(PC11,10))
pc.pg %<>% 
  mutate(PC12 = ntile(PC12,10))
pc.pg %<>% 
  mutate(PC13 = ntile(PC13,10))
pc.pg %<>% 
  mutate(PC14 = ntile(PC14,10))
pc.pg %<>% 
  mutate(PC15 = ntile(PC15,10))

index <- 1:ncol(pc.pg)
pc.pg[ , index] <- lapply(pc.pg[ , index], as.factor)
str(pc.pg)


#make this example reproducible
set.seed(1)

#Use 70% of our dataset as training set and remaining 30% as testing set
sample <- sample(c(TRUE, FALSE), nrow(pc.pg), replace=TRUE, prob=c(0.7,0.3))
train  <- pc.pg[sample, ]
test   <- pc.pg[!sample, ]

tancl = tree.bayes(pc.pg, training = "drafted")
graphviz.plot(tancl)

nbcl = naive.bayes(pc.pg, training = "drafted")
graphviz.plot(nbcl, layout = "fdp")


tancl.trained = bn.fit(tancl, pc.pg)
nbcl.trained = bn.fit(nbcl, pc.pg)

#graphviz.plot(tancl.trained)


coef(tancl.trained$drafted)
coef(nbcl.trained$drafted)


coef(tancl.trained$PC5)

#cross validation

cv.tan = bn.cv(tancl, data = pc.pg, runs = 10, method = "k-fold",
               folds = 10, algorithm.args = list(training = "drafted"))
cv.tan

cv.nb = bn.cv(nbcl, data = pc.pg, runs = 10, method = "k-fold",
                 folds = 10, algorithm.args = list(training = "drafted"))
  
cv.nb

#avg loss ; nbcl : 0.07249839  ; tancl : 0.1487588 

plot(cv.tan, cv.nb, xlab = c("TAN",'NB'))


###TAN PERFORMANCE

## SIMPLE FIT - THIS ONE

pred.maxlik = predict(tancl.trained, node = "drafted", test, method = "parents")
pred.bayes = predict(tancl.trained, node = "drafted", test, method = "bayes-lw")

mean(abs(as.numeric(test$drafted) - as.numeric(pred.maxlik)))
#0.01621622
mean(abs(as.numeric(test$drafted) - as.numeric(pred.bayes)))
#0.01621622

# CROSS - VALIDATED MODEL !

#pred.maxlik = predict(tancl, node = "drafted", test, method = "parents")
#pred.bayes = predict(tancl, node = "drafted", test, method = "bayes-lw")

mean(abs(as.numeric(test$drafted) - as.numeric(pred.maxlik)))
#0.04864865
mean(abs(as.numeric(test$drafted) - as.numeric(pred.bayes)))
#0.04864865

table <- confusionMatrix(data= pred.maxlik, reference = test$drafted ,mode = "everything")
table

# P-Value [Acc > NIR] : 5.176e-13
# Accuracy : 0.9838     
# F1 : 0.9914
# Sensitivity : 0.9926    
# Specificity : 0.8333 
# Balanced Accuracy : 0.9130  

# computing model performance metrics
data.frame( R2 = R2(as.numeric(pred.maxlik), as.numeric(test$drafted)))

### 0.710154

###NAIVE BAYES PERFORMANCE

# SIMPLE FIT

#pred.maxlik = predict(nbcl.trained, node = "drafted", test, method = "parents")
#pred.bayes = predict(nbcl.trained, node = "drafted", test, method = "bayes-lw")

mean(abs(as.numeric(test$drafted) - as.numeric(pred.maxlik)))
#0.04092664
mean(abs(as.numeric(test$drafted) - as.numeric(pred.bayes)))
#0.04092664

# CROSS VALIDATED - THIS ONE !

pred.maxlik = predict(nbcl, node = "drafted", test, method = "parents")
pred.bayes = predict(nbcl, node = "drafted", test, method = "bayes-lw")

mean(abs(as.numeric(test$drafted) - as.numeric(pred.maxlik)))
#0.04092664
mean(abs(as.numeric(test$drafted) - as.numeric(pred.bayes)))
#0.04092664

table <- confusionMatrix(data = pred.maxlik, reference = test$drafted ,mode = "everything")
table

# P-Value [Acc > NIR] : 0.009997  
# Accuracy : 0.9591 
# F1 : 0.9786 
# Sensitivity : 0.9886       
# Specificity : 0.4583  
# alanced Accuracy : 0.7234   

# computing model performance metrics
data.frame( R2 = R2(as.numeric(pred.maxlik), as.numeric(test$drafted)))

### R2 :  0.2998

# Prediction 2023 :

data_2023 <- subset(Data_PG_2023,select = -c(player_name)) %>% mutate_if(is.integer, as.numeric)
  
# Compute PCA coordinates of the new data
pcsnew <- as.data.frame(scale(data_2023, PG.pca$center, PG.pca$scale) %*% PG.pca$rotation)

pcsnew <- pcsnew[,1:15]

pcsnew %<>% 
  mutate(PC1 = ntile(PC1,10))
pcsnew %<>% 
  mutate(PC2 = ntile(PC2,10))
pcsnew %<>% 
  mutate(PC3 = ntile(PC3,10))
pcsnew %<>% 
  mutate(PC4 = ntile(PC4,10))
pcsnew %<>% 
  mutate(PC5 = ntile(PC5,10))
pcsnew %<>% 
  mutate(PC6 = ntile(PC6,10))
pcsnew %<>%
  mutate(PC7 = ntile(PC7,10))
pcsnew %<>% 
  mutate(PC8 = ntile(PC8,10))
pcsnew %<>% 
  mutate(PC9 = ntile(PC9,10))
pcsnew %<>% 
  mutate(PC10 = ntile(PC10,10))
pcsnew %<>% 
  mutate(PC11 = ntile(PC11,10))
pcsnew %<>% 
  mutate(PC12 = ntile(PC12,10))
pcsnew %<>% 
  mutate(PC13 = ntile(PC13,10))
pcsnew %<>% 
  mutate(PC14 = ntile(PC14,10))
pcsnew %<>% 
  mutate(PC15 = ntile(PC15,10))

index <- 1:ncol(pcsnew)
pcsnew[ , index] <- lapply(pcsnew[ , index], as.factor)
str(pcsnew)

data_2023 <- cbind(data_2023, player_name = Data_PG_2023$player_name)

pred.2023_tan = predict(tancl.trained, node = "drafted", pcsnew, method = "bayes-lw")

head(pred.2023_tan)

data_2023 <- cbind(data_2023, drafted_tancl = pred.2023_tan)

#pcsnew <- cbind(pcsnew, drafted= factor(rep(0, n), levels = c("0", "1")))

pred.2023_nb = predict(nbcl.trained, node = 'drafted', pcsnew, method = 'bayes-lw')

head(pred.2023_nb)

data_2023 <- cbind(data_2023, drafted_nbcl = pred.2023_nb)

drafted_nb_cl <- subset(data_2023, drafted_nbcl == 1)
drafted_tan_cl <- subset(data_2023, drafted_tancl == 1)

drafted_nb_cl[,"player_name"]
drafted_tan_cl[,"player_name"]

draft_nb <- drafted_nb_cl
draft_tan <- drafted_tan_cl


#####
# BN - SG
#####
Sg_pc <- as.data.frame(SG.pca$x)

Sg_pc <- cbind(Data_SG$drafted)


pc.sg <- as.data.frame(SG.pca$x)
pc.sg <- pc.sg[,1:15]
pc.sg <- cbind(pc.sg, drafted = Data_SG$drafted)

#library(magrittr)
pc.sg %<>% 
  mutate(PC1 = ntile(PC1,10))
pc.sg %<>% 
  mutate(PC2 = ntile(PC2,10))
pc.sg %<>% 
  mutate(PC3 = ntile(PC3,10))
pc.sg %<>% 
  mutate(PC4 = ntile(PC4,10))
pc.sg %<>% 
  mutate(PC5 = ntile(PC5,10))
pc.sg %<>% 
  mutate(PC6 = ntile(PC6,10))
pc.sg %<>%
  mutate(PC7 = ntile(PC7,10))
pc.sg %<>% 
  mutate(PC8 = ntile(PC8,10))
pc.sg %<>% 
  mutate(PC9 = ntile(PC9,10))
pc.sg %<>% 
  mutate(PC10 = ntile(PC10,10))
pc.sg %<>% 
  mutate(PC11 = ntile(PC11,10))
pc.sg %<>% 
  mutate(PC12 = ntile(PC12,10))
pc.sg %<>% 
  mutate(PC13 = ntile(PC13,10))
pc.sg %<>% 
  mutate(PC14 = ntile(PC14,10))
pc.sg %<>% 
  mutate(PC15 = ntile(PC15,10))

index <- 1:ncol(pc.sg)
pc.sg[ , index] <- lapply(pc.sg[ , index], as.factor)
str(pc.sg)


#make this example reproducible
set.seed(1)

#Use 70% of our dataset as training set and remaining 30% as testing set
sample <- sample(c(TRUE, FALSE), nrow(pc.sg), replace=TRUE, prob=c(0.7,0.3))
train  <- pc.sg[sample, ]
test   <- pc.sg[!sample, ]


tancl = tree.bayes(pc.sg, training = "drafted")
graphviz.plot(tancl)

nbcl = naive.bayes(pc.sg,training = "drafted")
graphviz.plot(nbcl, layout = "fdp")


tancl.trained = bn.fit(tancl, pc.sg)
nbcl.trained = bn.fit(nbcl, pc.sg)


coef(tancl.trained$drafted)
coef(nbcl.trained$drafted)


coef(tancl.trained$PC5)

#cross validation

cv.tan = bn.cv(tancl, data = pc.sg, runs = 10, method = "k-fold",
               folds = 10, algorithm.args = list(training = "drafted"))
cv.tan

cv.nb = bn.cv(nbcl, data = pc.sg, runs = 10, method = "k-fold",
              folds = 10, algorithm.args = list(training = "drafted"))

cv.nb

#avg loss ; nbcl : 0.0498252 ; tancl : 0.07296487 

plot(cv.tan, xlab = "TAN")


###TAN PERFORMANCE

# SIMPLE FIT - THIS ONE

pred.maxlik = predict(tancl.trained, node = "drafted", test, method = "parents")
pred.bayes = predict(tancl.trained, node = "drafted", test, method = "bayes-lw")

mean(abs(as.numeric(test$drafted) - as.numeric(pred.maxlik)))
#0.03107961
mean(abs(as.numeric(test$drafted) - as.numeric(pred.bayes)))
#0.03107961

# CROSS VALIDATED

#pred.maxlik = predict(tancl, node = "drafted", test, method = "parents")
#pred.bayes = predict(tancl, node = "drafted", test, method = "bayes-lw")

mean(abs(as.numeric(test$drafted) - as.numeric(pred.maxlik)))
#0.2017448
mean(abs(as.numeric(test$drafted) - as.numeric(pred.bayes)))
#0.2017448

table <- confusionMatrix(data= pred.maxlik, reference = test$drafted ,mode = "everything")
table

# P-Value [Acc > NIR] : 4.686e-07 
# Accuracy : 0.9689
# F1 : 0.9835 
# Sensitivity : 0.9804      
# Specificity : 0.7745
# Balanced Accuracy : 0.8774   

# computing model performance metrics
data.frame( R2 = R2(as.numeric(pred.maxlik), as.numeric(test$drafted)))

### 0.517659

###NAIVE BAYES PERFORMANCE

#SIMPLE FIT 

#pred.maxlik = predict(nbcl.trained, node = "drafted", test, method = "parents")
#pred.bayes = predict(nbcl.trained, node = "drafted", test, method = "bayes-lw")

mean(abs(as.numeric(test$drafted) - as.numeric(pred.maxlik)))
#0.0523446
mean(abs(as.numeric(test$drafted) - as.numeric(pred.bayes)))
#0.0523446

#CROSS VALIDATED - THIS ONE

pred.maxlik = predict(nbcl, node = "drafted", test, method = "parents")
pred.bayes = predict(nbcl, node = "drafted", test, method = "bayes-lw")

mean(abs(as.numeric(test$drafted) - as.numeric(pred.maxlik)))
#0.04634678
mean(abs(as.numeric(test$drafted) - as.numeric(pred.bayes)))
#0.04634678

table <- confusionMatrix(data = pred.maxlik, reference = test$drafted ,mode = "everything")
table

# P-Value [Acc > NIR] : 0.0435
# Accuracy : 0.9537 
# F1 : 0.9757 
# Sensitivity : 0.9861      
# Specificity : 0.4020   
# Balanced Accuracy : 0.6941  

# computing model performance metrics
data.frame( R2 = R2(as.numeric(pred.maxlik), as.numeric(test$drafted)))

### R2 : 0.2314211

# Prediction 2023 :

data_2023 <- subset(Data_SG_2023,select = -c(player_name)) %>% mutate_if(is.integer, as.numeric)

# Compute PCA coordinates of the new data
pcsnew <- as.data.frame(scale(data_2023, SG.pca$center, SG.pca$scale) %*% SG.pca$rotation)

pcsnew <- pcsnew[,1:15]

# Compute PC scores on 2023 data !


pcsnew %<>% 
  mutate(PC1 = ntile(PC1,10))
pcsnew %<>% 
  mutate(PC2 = ntile(PC2,10))
pcsnew %<>% 
  mutate(PC3 = ntile(PC3,10))
pcsnew %<>% 
  mutate(PC4 = ntile(PC4,10))
pcsnew %<>% 
  mutate(PC5 = ntile(PC5,10))
pcsnew %<>% 
  mutate(PC6 = ntile(PC6,10))
pcsnew %<>%
  mutate(PC7 = ntile(PC7,10))
pcsnew %<>% 
  mutate(PC8 = ntile(PC8,10))
pcsnew %<>% 
  mutate(PC9 = ntile(PC9,10))
pcsnew %<>% 
  mutate(PC10 = ntile(PC10,10))
pcsnew %<>% 
  mutate(PC11 = ntile(PC11,10))
pcsnew %<>% 
  mutate(PC12 = ntile(PC12,10))
pcsnew %<>% 
  mutate(PC13 = ntile(PC13,10))
pcsnew %<>% 
  mutate(PC14 = ntile(PC14,10))
pcsnew %<>% 
  mutate(PC15 = ntile(PC15,10))

index <- 1:ncol(pcsnew)
pcsnew[ , index] <- lapply(pcsnew[ , index], as.factor)
str(pcsnew)

data_2023 <- cbind(data_2023, player_name = Data_SG_2023$player_name)

pred.2023_tan = predict(tancl.trained, node = "drafted", pcsnew, method = "bayes-lw")

head(pred.2023_tan)

data_2023 <- cbind(data_2023, drafted_tancl = pred.2023_tan)

pred.2023_nb = predict(nbcl.trained, node = "drafted", pcsnew, method = "bayes-lw")

head(pred.2023_nb)

data_2023 <- cbind(data_2023, drafted_nbcl = pred.2023_nb)

drafted_nb_cl <- subset(data_2023, drafted_nbcl == 1)
drafted_tan_cl <- subset(data_2023, drafted_tancl == 1)

drafted_nb_cl[,"player_name"]
drafted_tan_cl[,"player_name"]

draft_nb <- rbind(draft_nb,drafted_nb_cl)
draft_tan <- rbind(draft_tan,drafted_tan_cl)

#####
# BN - SF
#####

SF_pc <- as.data.frame(SF.pca$x)

SF_pc <- cbind(Data_SF$drafted)


pc.sf <- as.data.frame(SF.pca$x)
pc.sf <- pc.sf[,1:15]
pc.sf <- cbind(pc.sf, drafted = Data_SF$drafted)

#library(magrittr)
pc.sf %<>% 
  mutate(PC1 = ntile(PC1,10))
pc.sf %<>% 
  mutate(PC2 = ntile(PC2,10))
pc.sf %<>% 
  mutate(PC3 = ntile(PC3,10))
pc.sf %<>% 
  mutate(PC4 = ntile(PC4,10))
pc.sf %<>% 
  mutate(PC5 = ntile(PC5,10))
pc.sf %<>% 
  mutate(PC6 = ntile(PC6,10))
pc.sf %<>%
  mutate(PC7 = ntile(PC7,10))
pc.sf %<>% 
  mutate(PC8 = ntile(PC8,10))
pc.sf %<>% 
  mutate(PC9 = ntile(PC9,10))
pc.sf %<>% 
  mutate(PC10 = ntile(PC10,10))
pc.sf %<>% 
  mutate(PC11 = ntile(PC11,10))
pc.sf %<>% 
  mutate(PC12 = ntile(PC12,10))
pc.sf %<>% 
  mutate(PC13 = ntile(PC13,10))
pc.sf %<>% 
  mutate(PC14 = ntile(PC14,10))
pc.sf %<>% 
  mutate(PC15 = ntile(PC15,10))

index <- 1:ncol(pc.sf)
pc.sf[ , index] <- lapply(pc.sf[ , index], as.factor)
str(pc.sf)


#make this example reproducible
set.seed(1)

#Use 70% of our dataset as training set and remaining 30% as testing set
sample <- sample(c(TRUE, FALSE), nrow(pc.sf), replace=TRUE, prob=c(0.7,0.3))
train  <- pc.sf[sample, ]
test   <- pc.sf[!sample, ]


tancl = tree.bayes(pc.sf, training = "drafted")
graphviz.plot(tancl)

nbcl = naive.bayes(pc.sf,training = "drafted")
graphviz.plot(nbcl, layout = "fdp")


tancl.trained = bn.fit(tancl, pc.sf)
nbcl.trained = bn.fit(nbcl, pc.sf)


coef(tancl.trained$drafted)
coef(nbcl.trained$drafted)


coef(tancl.trained$PC5)

#cross validation

cv.tan = bn.cv(tancl, data = pc.sf, runs = 10, method = "k-fold",
               folds = 10, algorithm.args = list(training = "drafted"))
cv.tan

cv.nb = bn.cv(nbcl, data = pc.sf, runs = 10, method = "k-fold",
              folds = 10, algorithm.args = list(training = "drafted"))

cv.nb

#avg loss ; nbcl : 0.072 ; tancl : 0.004162878 

plot(cv.tan, xlab = "TAN")

###TAN PERFORMANCE

# SIMPLE FIT

#pred.maxlik = predict(tancl.trained, node = "drafted", test, method = "parents")
#pred.bayes = predict(tancl.trained, node = "drafted", test, method = "bayes-lw")

mean(abs(as.numeric(test$drafted) - as.numeric(pred.maxlik)))
#0.1046386
mean(abs(as.numeric(test$drafted) - as.numeric(pred.bayes)))
#0.1197411

# CROSS VALIDATED - THIS ONE !

pred.maxlik = predict(tancl, node = "drafted", test, method = "parents")
pred.bayes = predict(tancl., node = "drafted", test, method = "bayes-lw")

mean(abs(as.numeric(test$drafted) - as.numeric(pred.maxlik)))
#0.09277238
mean(abs(as.numeric(test$drafted) - as.numeric(pred.bayes)))
#0.1197411

table <- confusionMatrix(data= pred.maxlik, reference = test$drafted ,mode = "everything")
table

# P-Value [Acc > NIR] : 0.3477
# Accuracy : 0.9159  
# F1 : 0.9516
# Sensitivity : 0.9077     
# Specificity : 1.0000 
# Balanced Accuracy : 0.9538 

# computing model performance metrics
data.frame( R2 = R2(as.numeric(pred.maxlik), as.numeric(test$drafted)))

### 0.4651  

###NAIVE BAYES PERFORMANCE

# SIMPLE FIT 

#pred.maxlik = predict(nbcl.trained, node = "drafted", test, method = "parents")
#pred.bayes = predict(nbcl.trained, node = "drafted", test, method = "bayes-lw")

mean(abs(as.numeric(test$drafted) - as.numeric(pred.maxlik)))
#0.07335491
mean(abs(as.numeric(test$drafted) - as.numeric(pred.bayes)))
#0.07335491

# CROSSVALIDATED - THIS ONE !

pred.maxlik = predict(nbcl, node = "drafted", test, method = "parents")
pred.bayes = predict(nbcl, node = "drafted", test, method = "bayes-lw")

mean(abs(as.numeric(test$drafted) - as.numeric(pred.maxlik)))
#0.05501618
mean(abs(as.numeric(test$drafted) - as.numeric(pred.bayes)))
#0.05501618

table <- confusionMatrix(data = pred.maxlik, reference = test$drafted ,mode = "everything")
table

# P-Value [Acc > NIR] : 0.0562880  
# ccuracy : 0.945  
# F1 : 0.9703
# Sensitivity : 0.9858     
# Specificity : 0.5244   
# Balanced Accuracy : 0.7551  

# computing model performance metrics
data.frame( R2 = R2(as.numeric(pred.maxlik), as.numeric(test$drafted)))

### R2 : 0.376057

# Prediction 2023 :

data_2023 <- subset(Data_SF_2023,select = -c(player_name)) %>% mutate_if(is.integer, as.numeric)

# Compute PCA coordinates of the new data
pcsnew <- as.data.frame(scale(data_2023, SF.pca$center, SF.pca$scale) %*% SF.pca$rotation)

pcsnew <- pcsnew[,1:15]

pcsnew %<>% 
  mutate(PC1 = ntile(PC1,10))
pcsnew %<>% 
  mutate(PC2 = ntile(PC2,10))
pcsnew %<>% 
  mutate(PC3 = ntile(PC3,10))
pcsnew %<>% 
  mutate(PC4 = ntile(PC4,10))
pcsnew %<>% 
  mutate(PC5 = ntile(PC5,10))
pcsnew %<>% 
  mutate(PC6 = ntile(PC6,10))
pcsnew %<>%
  mutate(PC7 = ntile(PC7,10))
pcsnew %<>% 
  mutate(PC8 = ntile(PC8,10))
pcsnew %<>% 
  mutate(PC9 = ntile(PC9,10))
pcsnew %<>% 
  mutate(PC10 = ntile(PC10,10))
pcsnew %<>% 
  mutate(PC11 = ntile(PC11,10))
pcsnew %<>% 
  mutate(PC12 = ntile(PC12,10))
pcsnew %<>% 
  mutate(PC13 = ntile(PC13,10))
pcsnew %<>% 
  mutate(PC14 = ntile(PC14,10))
pcsnew %<>% 
  mutate(PC15 = ntile(PC15,10))

index <- 1:ncol(pcsnew)
pcsnew[ , index] <- lapply(pcsnew[ , index], as.factor)
str(pcsnew)

data_2023 <- cbind(data_2023, player_name = Data_SF_2023$player_name)

pred.2023_tan = predict(tancl, node = "drafted", pcsnew, method = "bayes-lw")

head(pred.2023_tan)

data_2023 <- cbind(data_2023, drafted_tancl = pred.2023_tan)

pred.2023_nb = predict(nbcl.trained, node = "drafted", pcsnew, method = "bayes-lw")

head(pred.2023_nb)

data_2023 <- cbind(data_2023, drafted_nbcl = pred.2023_nb)

drafted_nb_cl <- subset(data_2023, drafted_nbcl == 1)
drafted_tan_cl <- subset(data_2023, drafted_tancl == 1)

drafted_nb_cl[,"player_name"]
drafted_tan_cl[,"player_name"]

draft_nb <- rbind(draft_nb,drafted_nb_cl)
draft_tan <- rbind(draft_tan,drafted_tan_cl)


#####
# BN - PF
#####

PF_pc <- as.data.frame(PF.pca$x)

PF_pc <- cbind(Data_PF$drafted)


pc.pf <- as.data.frame(PF.pca$x)
pc.pf <- pc.pf[,1:15]
pc.pf <- cbind(pc.pf, drafted = Data_PF$drafted)

#library(magrittr)
pc.pf %<>% 
  mutate(PC1 = ntile(PC1,10))
pc.pf %<>% 
  mutate(PC2 = ntile(PC2,10))
pc.pf %<>% 
  mutate(PC3 = ntile(PC3,10))
pc.pf %<>% 
  mutate(PC4 = ntile(PC4,10))
pc.pf %<>% 
  mutate(PC5 = ntile(PC5,10))
pc.pf %<>% 
  mutate(PC6 = ntile(PC6,10))
pc.pf %<>%
  mutate(PC7 = ntile(PC7,10))
pc.pf %<>% 
  mutate(PC8 = ntile(PC8,10))
pc.pf %<>% 
  mutate(PC9 = ntile(PC9,10))
pc.pf %<>% 
  mutate(PC10 = ntile(PC10,10))
pc.pf %<>% 
  mutate(PC11 = ntile(PC11,10))
pc.pf %<>% 
  mutate(PC12 = ntile(PC12,10))
pc.pf %<>% 
  mutate(PC13 = ntile(PC13,10))
pc.pf %<>% 
  mutate(PC14 = ntile(PC14,10))
pc.pf %<>% 
  mutate(PC15 = ntile(PC15,10))

index <- 1:ncol(pc.pf)
pc.pf[ , index] <- lapply(pc.pf[ , index], as.factor)
str(pc.pf)


#make this example reproducible
set.seed(1)

#Use 70% of our dataset as training set and remaining 30% as testing set
sample <- sample(c(TRUE, FALSE), nrow(pc.pf), replace=TRUE, prob=c(0.7,0.3))
train  <- pc.pf[sample, ]
test   <- pc.pf[!sample, ]


tancl = tree.bayes(pc.pf, training = "drafted")
graphviz.plot(tancl)

nbcl = naive.bayes(pc.pf,training = "drafted")
graphviz.plot(nbcl, layout = "fdp")


tancl.trained = bn.fit(tancl, pc.pf)
nbcl.trained = bn.fit(nbcl, pc.pf)


coef(tancl.trained$drafted)
coef(nbcl.trained$drafted)


coef(tancl.trained$PC5)

#cross validation

cv.tan = bn.cv(tancl, data = pc.pf, runs = 10, method = "k-fold",
               folds = 10, algorithm.args = list(training = "drafted"))
cv.tan

cv.nb = bn.cv(nbcl, data = pc.pf, runs = 10, method = "k-fold",
              folds = 10, algorithm.args = list(training = "drafted"))

cv.nb

#avg loss ; nbcl : 0.10 ; tancl : 0.163

plot(cv.tan, xlab = "TAN")

###TAN PERFORMANCE

# SIMPLE FIT - THIS ONE !

pred.maxlik = predict(tancl.trained, node = "drafted", test, method = "parents")
pred.bayes = predict(tancl.trained, node = "drafted", test, method = "bayes-lw")

mean(abs(as.numeric(test$drafted) - as.numeric(pred.maxlik)))
#0.05387205
mean(abs(as.numeric(test$drafted) - as.numeric(pred.bayes)))
#0.04377104

# CROSS VAIDATED

pred.maxlik = predict(tancl, node = "drafted", test, method = "parents")
pred.bayes = predict(tancl, node = "drafted", test, method = "bayes-lw")

mean(abs(as.numeric(test$drafted) - as.numeric(pred.maxlik)))
#0.2794613
mean(abs(as.numeric(test$drafted) - as.numeric(pred.bayes)))
#0.2794613

table <- confusionMatrix(data= pred.maxlik, reference = test$drafted ,mode = "everything")
table

# P-Value [Acc > NIR] : 0.0004671
# Accuracy : 0.9461
# F1 : 0.9688 
# Sensitivity : 0.9394  
# Specificity : 1.0000 
# Balanced Accuracy : 0.9697    

# computing model performance metrics
data.frame( R2 = R2(as.numeric(pred.maxlik), as.numeric(test$drafted)))

### 0.6326531

###NAIVE BAYES PERFORMANCE

# SIMPLE FIT

pred.maxlik = predict(nbcl.trained, node = "drafted", test, method = "parents")
pred.bayes = predict(nbcl.trained, node = "drafted", test, method = "bayes-lw")

mean(abs(as.numeric(test$drafted) - as.numeric(pred.maxlik)))
#0.08080808
mean(abs(as.numeric(test$drafted) - as.numeric(pred.bayes)))
#0.08080808

# CROSS VALIDATED - THIS ONE !

pred.maxlik = predict(nbcl, node = "drafted", test, method = "parents")
pred.bayes = predict(nbcl, node = "drafted", test, method = "bayes-lw")

mean(abs(as.numeric(test$drafted) - as.numeric(pred.maxlik)))
#0.04377104
mean(abs(as.numeric(test$drafted) - as.numeric(pred.bayes)))
#0.04377104

table <- confusionMatrix(data = pred.maxlik, reference = test$drafted ,mode = "everything")
table

# P-Value [Acc > NIR] : 3.094e-05 
# Accuracy : 0.9562
# F1 : 0.9756 
# Sensitivity : 0.9848       
# Specificity : 0.7273    
# Balanced Accuracy : 0.8561 

# computing model performance metrics
data.frame( R2 = R2(as.numeric(pred.maxlik), as.numeric(test$drafted)))

  ### R2 :  0.586564

# Prediction 2023 :

data_2023 <- subset(Data_PF_2023,select = -c(player_name)) %>% mutate_if(is.integer, as.numeric)

# Compute PCA coordinates of the new data
pcsnew <- as.data.frame(scale(data_2023, PF.pca$center, PF.pca$scale) %*% PF.pca$rotation)

pcsnew <- pcsnew[,1:15]

pcsnew %<>% 
  mutate(PC1 = ntile(PC1,10))
pcsnew %<>% 
  mutate(PC2 = ntile(PC2,10))
pcsnew %<>% 
  mutate(PC3 = ntile(PC3,10))
pcsnew %<>% 
  mutate(PC4 = ntile(PC4,10))
pcsnew %<>% 
  mutate(PC5 = ntile(PC5,10))
pcsnew %<>% 
  mutate(PC6 = ntile(PC6,10))
pcsnew %<>%
  mutate(PC7 = ntile(PC7,10))
pcsnew %<>% 
  mutate(PC8 = ntile(PC8,10))
pcsnew %<>% 
  mutate(PC9 = ntile(PC9,10))
pcsnew %<>% 
  mutate(PC10 = ntile(PC10,10))
pcsnew %<>% 
  mutate(PC11 = ntile(PC11,10))
pcsnew %<>% 
  mutate(PC12 = ntile(PC12,10))
pcsnew %<>% 
  mutate(PC13 = ntile(PC13,10))
pcsnew %<>% 
  mutate(PC14 = ntile(PC14,10))
pcsnew %<>% 
  mutate(PC15 = ntile(PC15,10))

index <- 1:ncol(pcsnew)
pcsnew[ , index] <- lapply(pcsnew[ , index], as.factor)
str(pcsnew)

data_2023 <- cbind(data_2023, player_name = Data_PF_2023$player_name)

pred.2023_tan = predict(tancl.trained, node = "drafted", pcsnew, method = "bayes-lw")

head(pred.2023_tan)

data_2023 <- cbind(data_2023, drafted_tancl = pred.2023_tan)

pred.2023_nb = predict(nbcl.trained, node = "drafted", pcsnew, method = "bayes-lw")

head(pred.2023_nb)

data_2023 <- cbind(data_2023, drafted_nbcl = pred.2023_nb)

drafted_nb_cl <- subset(data_2023, drafted_nbcl == 1)
drafted_tan_cl <- subset(data_2023, drafted_tancl == 1)

drafted_nb_cl[,"player_name"]
drafted_tan_cl[,"player_name"]

draft_nb <- rbind(draft_nb,drafted_nb_cl)
draft_tan <- rbind(draft_tan,drafted_tan_cl)


#####
# BN - C
#####

C_pc <- as.data.frame(C.pca$x)

C_pc <- cbind(Data_C$drafted)


pc.c <- as.data.frame(C.pca$x)
pc.c <- pc.c[,1:15]
pc.c <- cbind(pc.c, drafted = Data_C$drafted)

#library(magrittr)
pc.c %<>% 
  mutate(PC1 = ntile(PC1,10))
pc.c %<>% 
  mutate(PC2 = ntile(PC2,10))
pc.c %<>% 
  mutate(PC3 = ntile(PC3,10))
pc.c %<>% 
  mutate(PC4 = ntile(PC4,10))
pc.c %<>% 
  mutate(PC5 = ntile(PC5,10))
pc.c %<>% 
  mutate(PC6 = ntile(PC6,10))
pc.c %<>%
  mutate(PC7 = ntile(PC7,10))
pc.c %<>% 
  mutate(PC8 = ntile(PC8,10))
pc.c %<>% 
  mutate(PC9 = ntile(PC9,10))
pc.c %<>% 
  mutate(PC10 = ntile(PC10,10))
pc.c %<>% 
  mutate(PC11 = ntile(PC11,10))
pc.c %<>% 
  mutate(PC12 = ntile(PC12,10))
pc.c %<>% 
  mutate(PC13 = ntile(PC13,10))
pc.c %<>% 
  mutate(PC14 = ntile(PC14,10))
pc.c %<>% 
  mutate(PC15 = ntile(PC15,10))

index <- 1:ncol(pc.c)
pc.c[ , index] <- lapply(pc.c[ , index], as.factor)
str(pc.c)


#make this example reproducible
set.seed(1)

#Use 70% of our dataset as training set and remaining 30% as testing set
sample <- sample(c(TRUE, FALSE), nrow(pc.c), replace=TRUE, prob=c(0.7,0.3))
train  <- pc.c[sample, ]
test   <- pc.c[!sample, ]


tancl = tree.bayes(pc.c, training = "drafted")
graphviz.plot(tancl)

nbcl = naive.bayes(pc.c,training = "drafted")
graphviz.plot(nbcl, layout = "fdp")


tancl.trained = bn.fit(tancl, pc.c)
nbcl.trained = bn.fit(nbcl, pc.c)


coef(tancl.trained$drafted)
coef(nbcl.trained$drafted)


coef(tancl.trained$PC5)

#cross validation

cv.tan = bn.cv(tancl, data = pc.c, runs = 10, method = "k-fold",
               folds = 10, algorithm.args = list(training = "drafted"))
cv.tan

cv.nb = bn.cv(nbcl, data = pc.c, runs = 10, method = "k-fold",
              folds = 10, algorithm.args = list(training = "drafted"))

cv.nb

#avg loss ; nbcl : 0.10 ; tancl : 0.21

plot(cv.tan, xlab = "TAN")

###TAN PERFORMANCE

# SIMPLE FIT - THIS ONE ! 

pred.maxlik = predict(tancl.trained, node = "drafted", test, method = "parents")
pred.bayes = predict(tancl.trained, node = "drafted", test, method = "bayes-lw")

mean(abs(as.numeric(test$drafted) - as.numeric(pred.maxlik)))
#0.1080139
mean(abs(as.numeric(test$drafted) - as.numeric(pred.bayes)))
#0.08362369

# CROSS VALIDATED

pred.maxlik = predict(tancl, node = "drafted", test, method = "parents")
pred.bayes = predict(tancl, node = "drafted", test, method = "bayes-lw")

mean(abs(as.numeric(test$drafted) - as.numeric(pred.maxlik)))
#0.2334495
mean(abs(as.numeric(test$drafted) - as.numeric(pred.bayes)))
#0.2473868

table <- confusionMatrix(data= pred.maxlik, reference = test$drafted ,mode = "everything")
table

# P-Value [Acc > NIR] : 0.1665 
  # Accuracy : 0.892  
  # F1 : 0.9339 
  # Sensitivity : 0.8760            
  # Specificity : 1.0000 
# Balanced Accuracy : 0.9380

# computing model performance metrics
data.frame( R2 = R2(as.numeric(pred.maxlik), as.numeric(test$drafted)))

### 0.4766

###NAIVE BAYES PERFORMANCE

# SIMPLE FIT 

pred.maxlik = predict(nbcl.trained, node = "drafted", test, method = "parents")
pred.bayes = predict(nbcl.trained, node = "drafted", test, method = "bayes-lw")

mean(abs(as.numeric(test$drafted) - as.numeric(pred.maxlik)))
#0.07317073
mean(abs(as.numeric(test$drafted) - as.numeric(pred.bayes)))
#0.07317073

# CROSS VALIDATED - THIS ONE !

pred.maxlik = predict(nbcl, node = "drafted", test, method = "parents")
pred.bayes = predict(nbcl, node = "drafted", test, method = "bayes-lw")

mean(abs(as.numeric(test$drafted) - as.numeric(pred.maxlik)))
#0.05574913
mean(abs(as.numeric(test$drafted) - as.numeric(pred.bayes)))
#0.05574913

table <- confusionMatrix(data = pred.maxlik, reference = test$drafted ,mode = "everything")
table

# P-Value [Acc > NIR] : 3.553e-05   
# Accuracy : 0.9443  
# F1 : 0.9681 
# Sensitivity : 0.9720     
# Specificity : 0.7568   
# Balanced Accuracy : 0.8644 

# computing model performance metrics
data.frame( R2 = R2(as.numeric(pred.maxlik), as.numeric(test$drafted)))

### R2 :  0.5569784

# Prediction 2023 :

data_2023 <- subset(Data_C_2023,select = -c(player_name)) %>% mutate_if(is.integer, as.numeric)

# Compute PCA coordinates of the new data
pcsnew <- as.data.frame(scale(data_2023, C.pca$center, C.pca$scale) %*% C.pca$rotation)

pcsnew <- pcsnew[,1:15]

pcsnew %<>% 
  mutate(PC1 = ntile(PC1,10))
pcsnew %<>% 
  mutate(PC2 = ntile(PC2,10))
pcsnew %<>% 
  mutate(PC3 = ntile(PC3,10))
pcsnew %<>% 
  mutate(PC4 = ntile(PC4,10))
pcsnew %<>% 
  mutate(PC5 = ntile(PC5,10))
pcsnew %<>% 
  mutate(PC6 = ntile(PC6,10))
pcsnew %<>%
  mutate(PC7 = ntile(PC7,10))
pcsnew %<>% 
  mutate(PC8 = ntile(PC8,10))
pcsnew %<>% 
  mutate(PC9 = ntile(PC9,10))
pcsnew %<>% 
  mutate(PC10 = ntile(PC10,10))
pcsnew %<>% 
  mutate(PC11 = ntile(PC11,10))
pcsnew %<>% 
  mutate(PC12 = ntile(PC12,10))
pcsnew %<>% 
  mutate(PC13 = ntile(PC13,10))
pcsnew %<>% 
  mutate(PC14 = ntile(PC14,10))
pcsnew %<>% 
  mutate(PC15 = ntile(PC15,10))

index <- 1:ncol(pcsnew)
pcsnew[ , index] <- lapply(pcsnew[ , index], as.factor)
str(pcsnew)

data_2023 <- cbind(data_2023, player_name = Data_C_2023$player_name)

pred.2023_tan = predict(tancl.trained, node = "drafted", pcsnew, method = "bayes-lw")

head(pred.2023_tan)

data_2023 <- cbind(data_2023, drafted_tancl = pred.2023_tan)

pred.2023_nb = predict(nbcl.trained, node = "drafted", pcsnew, method = "bayes-lw")

head(pred.2023_nb)

data_2023 <- cbind(data_2023, drafted_nbcl = pred.2023_nb)

drafted_nb_cl <- subset(data_2023, drafted_nbcl == 1)
drafted_tan_cl <- subset(data_2023, drafted_tancl == 1)

drafted_nb_cl[,"player_name"]
drafted_tan_cl[,"player_name"]

draft_nb <- rbind(draft_nb,drafted_nb_cl)
draft_tan <- rbind(draft_tan,drafted_tan_cl)

### Takeaways : 

draft_nb[,"player_name"]
draft_tan[,"player_name"]

draft_nb <- draft_nb[!duplicated(draft_nb$player_name),]
draft_tan <- draft_tan[!duplicated(draft_tan$player_name),]

draft_nb[,"player_name"]
draft_tan[,"player_name"]

common_players <- subset(draft_tan, drafted_nbcl == 1)

common_players[,"player_name"]