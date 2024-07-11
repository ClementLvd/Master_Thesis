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

Data_PG <- subset(Data,(Pure.PG == 1 | Scoring.PG == 1), select = -c(Pure.PG,Scoring.PG,Combo.G,Wing.G,Wing.F,Stretch.4,PF.C,C)) ;

Data_PG_2023 <- subset(Data_2023,(Pure.PG == 1 | Scoring.PG == 1 | Combo.G == 1), select = -c(Pure.PG,Scoring.PG,Combo.G,Wing.G,Wing.F,Stretch.4,PF.C,C)) ;

Data_SG <- subset(Data,(Wing.G == 1 | Scoring.PG == 1 | Combo.G == 1), select = -c(Pure.PG,Scoring.PG,Combo.G,Wing.G,Wing.F,Stretch.4,PF.C,C)); 

Data_SG_2023 <- subset(Data_2023,(Wing.G == 1 | Combo.G == 1), select = -c(Pure.PG,Scoring.PG,Combo.G,Wing.G,Wing.F,Stretch.4,PF.C,C));

Data_SF <- subset(Data, (Wing.G == 1 | Wing.F == 1), select = -c(Pure.PG,Scoring.PG,Combo.G,Wing.G,Wing.F,Stretch.4,PF.C,C)) ;

Data_SF_2023 <- subset(Data_2023,(Wing.G == 1 | Wing.F == 1), select = -c(Pure.PG,Scoring.PG,Combo.G,Wing.G,Wing.F,Stretch.4,PF.C,C)) ;

Data_PF <- subset(Data, (Stretch.4 == 1 | PF.C == 1), select = -c(Pure.PG,Scoring.PG,Combo.G,Wing.G,Wing.F,Stretch.4,PF.C,C)) ;

Data_PF_2023 <- subset(Data_2023,(Stretch.4 == 1 | PF.C == 1), select = -c(Pure.PG,Scoring.PG,Combo.G,Wing.G,Wing.F,Stretch.4,PF.C,C)) ;

Data_C <- subset(Data,(C == 1 | PF.C == 1), select = -c(Pure.PG,Scoring.PG,Combo.G,Wing.G,Wing.F,Stretch.4,PF.C,C)) ;

Data_C_2023 <- subset(Data_2023,(C == 1 | PF.C == 1), select = -c(Pure.PG,Scoring.PG,Combo.G,Wing.G,Wing.F,Stretch.4,PF.C,C))

#####

## Let's perform a PCA with varimax rotation

#####
# GOOD PCA - PG
#####

data <- subset(Data_PG, select = -c(player_name,drafted))

library(dplyr)
data <- data %>% mutate_if(is.integer, as.numeric)

PG.pca <- prcomp(data, scale = TRUE)

fviz_eig(PG.pca)

x <- PG.pca$rotation

var_names <- colnames(data)


#Rotate the PCA according to varimax

ncomp <- 15

rawLoadings     <- x[,1:ncomp] %*% diag(PG.pca$sdev, ncomp, ncomp)

rotatedLoadings <- varimax(rawLoadings)$loadings
invLoadings     <- t(pracma::pinv(rotatedLoadings))
scores          <- scale(data[,1:58]) %*% invLoadings

print(scores[1:5,])  

scores <- as.data.frame(scores)

## Now cluster analysis


# Normalize

z <- as.data.frame(PG.pca$x)
z <- z[,1:2]
means <- apply(z,2,mean)
sds <- apply(z,2,sd)
nor <- as.data.frame(scale(z,center=means,scale=sds))

# distance matrix

distance = dist(nor)

# Hierarchical agglomerative clustering !

mydata.hclust = hclust(distance)
plot(mydata.hclust)

clusters <- hclust(distance, method = 'average')
plot(clusters)

# Both show the same thing : 3 clusters ! 

clusterCut <- cutree(clusters, 3)
table(clusterCut, Data_C$drafted)

ggplot(z, aes(x=PC1, y = PC2, color = factor(clusterCut))) + geom_point()

# screeplot

wss <- (nrow(nor)-1)*sum(apply(nor,2,var))
for (i in 2:20) wss[i] <- sum(kmeans(nor, centers=i)$withinss)
plot(1:20, wss, type="b", xlab="Number of Clusters", ylab="Within groups sum of squares")

# 5 would seem like a good nb of clusters


# Now let's try a k-means

set.seed(123)
kc <- kmeans(nor,3)
kc

library(cluster)
ot <-nor
datadistshortset<-dist(ot,method = "euclidean")
hc1 <- hclust(datadistshortset, method = "complete" )
pamvshortset <- pam(datadistshortset,3, diss = FALSE)
clusplot(pamvshortset, shade = FALSE,labels=2,col.clus="blue",col.p="red",span=FALSE,main="Cluster Mapping",cex=1.2)

library(factoextra) 
k2 <- kmeans(nor, centers = 3, nstart = 25)

str(k2)

fviz_cluster(k2, data = nor)

fviz_nbclust(nor, kmeans, method = "silhouette")

k2 <- kmeans(nor, centers = 6, nstart = 25)

str(k2)

fviz_cluster(k2, data = nor)

## Now let's perform DB scan

# Installing Packages
#install.packages("fpc")

# Loading package
library(fpc)

# Fitting DBScan clustering Model 
# to training dataset
set.seed(220)  # Setting seed
Dbscan_cl <- dbscan(nor, eps = 0.45, MinPts = 5)
Dbscan_cl

# Checking cluster
Dbscan_cl$cluster

# Table
table(Dbscan_cl$cluster, Data_PG$drafted)

# Plotting Cluster
plot(Dbscan_cl, nor, main = "DBScan")
plot(Dbscan_cl, nor, main = "PC1 vs PC2")

# Very bad result....let's try with another z , on more than 2 PCs for example

z <- as.data.frame(PG.pca$x)
z <- z[,1:4]

# Fitting DBScan clustering Model 
# to training dataset
set.seed(220)  # Setting seed
Dbscan_cl <- dbscan(z, eps = 0.5, MinPts = 3)
Dbscan_cl

# Checking cluster
Dbscan_cl$cluster

# Table
table(Dbscan_cl$cluster, Data_PG$drafted)

# Plotting Cluster
plot(Dbscan_cl, z, main = "DBScan")
plot(Dbscan_cl, z, main = "PC1 vs PC2")

# Keep only best contributions ! 

best_cont <- scores %>%mutate(contrib = sqrt(V1*V1 + V2*V2))

best_cont <- cbind(best_cont,drafted = Data_PG$drafted)

best_cont <- subset(best_cont, contrib > 1.5)

z <- best_cont[,1:2]

#RUN DBSCAN

set.seed(220)  # Setting seed
Dbscan_cl <- dbscan(as.data.frame(scale(z)), eps = 0.3, MinPts = 3)
Dbscan_cl

# Checking cluster
Dbscan_cl$cluster

# Table
table(Dbscan_cl$cluster, best_cont$drafted)

# Plotting Cluster
plot(Dbscan_cl, z, main = "DBScan")
plot(Dbscan_cl, z, main = "V1 vs V2")

# Find optimal cluster ! 
#install.packages("dbscan")
library(dbscan)
# to plot the eps values
eps_plot = kNNdistplot(as.data.frame(scale(z)), k=6)

# to draw an optimum line
eps_plot %>% abline(h = 0.5, lty = 2)

d <- dbscan::dbscan(z, eps = 0.19, minPts =  6)
d

# Table
table(d$cluster, best_cont$drafted)

# cluster visualisation
fviz_cluster(d, z, geom = "point")





