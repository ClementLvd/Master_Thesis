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

#install.packages('tidyr')
#install.packages("factoextra")

library(factoextra)
library(tidyr)

#####
# GOOD PCA ANALYSIS - PG
#####

#install.packages("ggcorrplot")
library(ggcorrplot)

data <- subset(Data_PG, select = -c(player_name,drafted))

corr_matrix <- cor(scale(data))
ggcorrplot(corr_matrix)

ggcorrplot(corr_matrix, hc.order = TRUE, outline.col = "white", insig = "blank")



PG.pca <- prcomp(data, scale = TRUE)

fviz_eig(PG.pca)

x <- PG.pca$rotation

var_names <- colnames(data)


# Select the top 200 contributing individuals
fviz_pca_ind(PG.pca,label="var", habillage=Data_PG$drafted,
             addEllipses=TRUE, ellipse.level=0.95, select.ind = list(contrib = 500))

# Select the top 5 contributing variables
fviz_pca_var(PG.pca, select.var = list(contrib = 10))

# 50 best indiv + 5 best variables
fviz_pca_biplot(PG.pca, label="var", habillage=Data_PG$drafted,
                addEllipses=TRUE, ellipse.level=0.95, select.ind = list(contrib = 200), select.var = list(contrib =10))

drafted_pc <- subset(PG.pca$x, Data_PG$drafted == 1)
undrafted_pc <- subset(PG.pca$x, Data_PG$drafted == 0)

drafted_pc <- drafted_pc[,1:2]
undrafted_pc <- undrafted_pc[,1:2]

# Find points within the ellipse of confidence for drafted

plot(drafted_pc[,2] ~ drafted_pc[,1], type = "n", asp = 1)

# add an ellipse, in this case a 95% ellipse
mu <- colMeans(drafted_pc) # center of the ellipse
Sigma <- cov(drafted_pc) # covariance matrix of the ellipse

# percentile of the ellipse
p <- 0.70 

# draw the ellipse for drafted players
#install.packages("SIBER")
library(SIBER)
tmp <- addEllipse(mu, Sigma, p.interval = p, col = "red", lty = 2)

# Determine which of the samples are inside the ellipse
Z_samp <- pointsToEllipsoid(drafted_pc[,1:2], Sigma, mu) # convert to circle space
inside_samp <- ellipseInOut(Z_samp, p = p) # test if inside

# inside points are marked TRUE which corresponds to 1 in numeric terms, and 
# outside marked FALSE which corresponds to 0. 
# So below I calculate (1 + !inside_test) which makes 1 correspond to inside 
# and coloured black, and 2 correspond to outside and coloured red.
# and plot them with colour coding for whether they are inside or outside

points(drafted_pc[,2] ~ drafted_pc[,1], col = 1 + !inside_samp)

#Now let's make the same test with the same ellipse but with undrafted players !

plot(undrafted_pc[,2] ~ undrafted_pc[,1], type = "n", asp = 1)
tmp <- addEllipse(mu, Sigma, p.interval = p, col = "red", lty = 2)

# Determine which of the samples are inside the ellipse
Z_samp <- pointsToEllipsoid(undrafted_pc[,1:2], Sigma, mu) # convert to circle space
inside_samp <- ellipseInOut(Z_samp, p = p) # test if inside

points(undrafted_pc[,2] ~ undrafted_pc[,1], col = 1 + !inside_samp)



#Rotate the PCA according to varimax

ncomp <- 15

rawLoadings     <- x[,1:ncomp] %*% diag(PG.pca$sdev, ncomp, ncomp)

rotatedLoadings <- varimax(rawLoadings)$loadings
invLoadings     <- t(pracma::pinv(rotatedLoadings))
scores          <- scale(data[,1:58]) %*% invLoadings

print(scores[1:5,])  

scores <- as.data.frame(scores)


# Visualize !

drafted_vari <- subset(scores,Data_PG$drafted == 1)
drafted_PG <- subset(Data_PG, Data_PG$drafted == 1)

undrafted_vari <- subset(scores,Data_PG$drafted == 0)
undrafted_PG <- subset(Data_PG, Data_PG$drafted == 0)

data_d <- drafted_vari[,1:2]
data_u <- undrafted_vari[,1:2]

plot(scores[,2] ~ scores[,1],type = "n", asp = 1)

mu_d <- colMeans(data_d) # center of the ellipse
Sigma_d <- cov(data_d) # covariance matrix of the ellipse
p <- 0.95

tmp_d <- addEllipse(mu_d, Sigma_d, p.interval = p, col = "red", lty = 2)

mu_u <- colMeans(data_u) # center of the ellipse
Sigma_u <- cov(data_u) # covariance matrix of the ellipse
p <- 0.95

tmp_u <- addEllipse(mu_u, Sigma_u, p.interval = p, col = "blue", lty = 2)


# Too much data points, let's order them by contribution, then build ellipses
# We compute the contributions as the norm of the vector from the origin to the points ...

data_d <- data_d %>%mutate(contrib = sqrt(V1*V1 + V2*V2))
data_u <- data_u %>%mutate(contrib = sqrt(V1*V1 + V2*V2))

data_d <- cbind(data_d,player_name = drafted_PG$player_name)
data_u <- cbind(data_u,player_name = undrafted_PG$player_name)

best_cont_d <- subset(data_d, contrib > 1.5)
best_cont_u <- subset(data_u, contrib > 1.5)

# Determine which of the samples are inside the ellipse

# Plot the best contributing points and the ellipses ! 

plot(scores[,2] ~ scores[,1],type = "n", asp = 1)

mu_d <- colMeans(best_cont_d[,1:2]) # center of the ellipse
Sigma_d <- cov(best_cont_d[,1:2]) # covariance matrix of the ellipse
p <- 0.7

tmp_d <- addEllipse(mu_d, Sigma_d, p.interval = p, col = "red", lty = 2)

mu_u <- colMeans(best_cont_u[,1:2]) # center of the ellipse
Sigma_u <- cov(best_cont_u[,1:2]) # covariance matrix of the ellipse
p <- 0.95

tmp_u <- addEllipse(mu_u, Sigma_u, p.interval = p, col = "blue", lty = 2)

points(best_cont_u[,2] ~ best_cont_u[,1], col = "blue")
points(best_cont_d[,2] ~ best_cont_d[,1], col = "red")

d_samp <- pointsToEllipsoid(best_cont_d[,1:2], Sigma_d, mu_d) # convert to circle space
inside_samp <- ellipseInOut(d_samp, p = 0.7) # test if inside

best_cont_d <- cbind(best_cont_d, inside_samp)

best_cont_d <- mutate(best_cont_d, drafted = 1)
best_cont_u <- mutate(best_cont_u, drafted = 0)

u_samp <- pointsToEllipsoid(best_cont_u[,1:2], Sigma_d, mu_d) # convert to circle space
inside_samp <- ellipseInOut(u_samp, p = 0.95) # test if inside

best_cont_u <- cbind(best_cont_u, inside_samp)

# Now let's put all the points inside the ellipse in a dataset and check the group differences !
library("dplyr") 
draftable_group <- bind_rows(subset(best_cont_d, inside_samp == TRUE),subset(best_cont_u, inside_samp == TRUE))

draftable_group <- subset(draftable_group, select = -c(inside_samp))

plot(scores[,2] ~ scores[,1],type = "n", asp = 1)

tmp_d <- addEllipse(mu_d, Sigma_d, p.interval = p, col = "red", lty = 2)
tmp_u <- addEllipse(mu_u, Sigma_u, p.interval = p, col = "blue", lty = 2)

points(draftable_group[,2] ~ draftable_group[,1])

# We have isolated a "draftable" Group using the first two Principal Components
# Now we will perform statistical tests to analyze the differences between
# The players that actually got drafted and undrafted players !


# Let's start by applying anova/manova on the rotated PCs... 

library(ggpubr)

ggboxplot(
  draftable_group, x = "drafted", y = c("V1","V2"), 
  merge = TRUE, palette = "jco"
)

# Significant difference in variance - due to the lower number of drafted players perhaps
# But the means seem really close !

rownames(draftable_group)

#MANOVA
model <- lm(cbind(V1, V2) ~ drafted, draftable_group)
Manova(model, test.statistic = "Pillai")

mav <- manova(model, test.statistic = "Pillai")
mav
summary.aov(mav)

#install.packages('effectsize')

library(effectsize)

eta_squared(mav)

#

#ANOVA
one.way <- aov(V1 ~ drafted, data = draftable_group)
summary(one.way)
one.way$coefficients

# p value : 5.9e-06
# P value is enough to reject Null hypothesis : to be expected
# F value : 21.11
# Mean sq


one.way <- aov(V2 ~ drafted, data = draftable_group)
summary(one.way)

# p value : 0.225
# P value is NOT enough to reject Null hypothesis : to be expected
# F value : 1.475
# Mean sq


# very low 0.7

one.way <- aov(V1 + V2 ~ drafted, data = draftable_group)
summary(one.way)

# decent F value


# Here we subset the original PC's from the PCA 

draftables_data <- as.data.frame(subset(PG.pca$x , rownames(Data_PG) %in% rownames(draftable_group)))

draftables_data <- draftables_data[,1:15]

draftables_data_2 <- subset(Data_PG , rownames(Data_PG) %in% rownames(draftable_group))

draftables_data <- cbind(draftables_data, drafted = draftables_data_2$drafted)

library(ggpubr)

ggboxplot(
  draftables_data, x = "drafted", y = c("PC9", "PC10", "PC11", "PC12", "PC13", "PC14", "PC15"), 
  merge = TRUE, palette = "jco"
)

ggboxplot(
  draftables_data, x = "drafted", y = c("PC1","PC15"), 
  merge = TRUE, palette = "jco"
)

# Assumptions : check mcshapiro


#MANOVA
model <- lm(cbind(PC2, PC3, PC4, PC5, PC6, PC7, PC8, PC9, PC10, PC11, PC12, PC13, PC14, PC15) ~ drafted, draftables_data)
Manova(model, test.statistic = "Pillai")

mav <- manova(model, test.statistic = "Pillai")
mav

summary.aov(mav)

#ANOVA
one.way <- aov(PC1 ~ drafted, data = draftables_data)
summary(one.way)

# Very High F, PC1 : 122.6 ; PC2 : 5.5 ; PC3 : 2 ; PC10 : 3 ; PC11 : 5 

two.way <- aov(drafted ~ PC1 + PC2, data = draftables_data)

summary(two.way)

# Non negligeable F = 25.48
# Now on the actual variables

# We figured out that the first Principal component explains most of the difference
# Between the two groups, let's see the main loadings of that component !

fviz_eig(PG.pca)
# Select the top 5 contributing variables
fviz_pca_var(PG.pca, select.var = list(contrib = 10))

ncomp <- 15

rawLoadings <- x[,1:ncomp] %*% diag(PG.pca$sdev, ncomp, ncomp)

rawLoadings <-as.data.frame(rawLoadings)

rawLoadings <-cbind(rawLoadings, var = var_names)

rawLoadings[,1]

ordered_load <- rawLoadings[order(rawLoadings$V1),]

ordered_load[,1]

# We therefore can extract the 10 variables with the biggest loadings! 

vars <- ordered_load[48:58,16]


draftables_data <- subset(Data_PG , rownames(Data_PG) %in% rownames(draftable_group))

vars

#ANOVA
one.way <- aov(X.gbpm ~ drafted, data = draftables_data)
summary(one.way)

# F : 114.3 - p :0

one.way <- aov(porpag ~ drafted, data = draftables_data)
summary(one.way)

# F : 87.8 - p : 0.

one.way <- aov(adjoe ~ drafted, data = draftables_data)
summary(one.way)

# F : 84.66 - p : 0.

one.way <- aov(X.bpm ~ drafted, data = draftables_data)
summary(one.way)

# F : 98.08 - p : 0.

one.way <- aov(ogbpm ~ drafted, data = draftables_data)
summary(one.way)

# F : 94.25 - p : 0.

one.way <- aov(X.obpm ~ drafted, data = draftables_data)
summary(one.way)

# F : 86.44 - p : 0.

one.way <- aov(twoPM ~ drafted, data = draftables_data)
summary(one.way)

# F : 45.93 - p : 0.

one.way <- aov(X.stops ~ drafted, data = draftables_data)
summary(one.way)

# F : 44.45  - p : 0.

one.way <- aov(TPM ~ drafted, data = draftables_data)
summary(one.way)

# F : 5.925 - p : 0.

one.way <- aov(ORtg ~ drafted, data = draftables_data)
summary(one.way)

# F : 22.61 - p : 0.

one.way <- aov(pts ~ drafted, data = draftables_data)
summary(one.way)

# F : 22.23 - p : 0.

one.way <- aov(X.rimmade ~ drafted, data = draftables_data)
summary(one.way)

# F : 28.57  - p : 0.

# Conclusion : BPM - adjoe - porpag ----- STOPS - twoPM ----- PTS - ORtg....

# Let's now try to run a MANOVA model on these variables

#MANOVA
model <- lm(cbind(X.bpm, adjoe, porpag, X.stops, twoPM, ORtg, pts) ~ drafted, draftables_data)
Manova(model, test.statistic = "Pillai")

# F : 20.325

model <- lm(cbind(X.bpm, adjoe, porpag, X.stops, twoPM) ~ drafted, draftables_data)
Manova(model, test.statistic = "Pillai")

#F : 26.97

model <- lm(cbind(X.bpm, adjoe, porpag) ~ drafted, draftables_data)
Manova(model, test.statistic = "Pillai")

#F : 39.152

### We have an interpretation of the main differences between the draftable players
### Things to do now : Establish the "draftable" players group in the 2023 class
### See which players would have the tendency to get drafted according to our study
### Build the NBA predictor model to have a projection of the impact players will have in the NBA


#####
#### GOOD PCA - SG 
#####
data <- subset(Data_SG, select = -c(player_name,drafted))

PG.pca <- prcomp(data, scale = TRUE)

fviz_eig(PG.pca)

x <- PG.pca$rotation

var_names <- colnames(data)


# Select the top 200 contributing individuals
fviz_pca_ind(PG.pca,label="var", habillage=Data_SG$drafted,
             addEllipses=TRUE, ellipse.level=0.95, select.ind = list(contrib = 500))

# Select the top 5 contributing variables
fviz_pca_var(PG.pca, select.var = list(contrib = 10))

# 50 best indiv + 5 best variables
fviz_pca_biplot(PG.pca, label="var", habillage=Data_SG$drafted,
                addEllipses=TRUE, ellipse.level=0.95, select.ind = list(contrib = 200), select.var = list(contrib =10))

drafted_pc <- subset(PG.pca$x, Data_SG$drafted == 1)
undrafted_pc <- subset(PG.pca$x, Data_SG$drafted == 0)

drafted_pc <- drafted_pc[,1:2]
undrafted_pc <- undrafted_pc[,1:2]

# Find points within the ellipse of confidence for drafted

plot(drafted_pc[,2] ~ drafted_pc[,1], type = "n", asp = 1)

# add an ellipse, in this case a 95% ellipse
mu <- colMeans(drafted_pc) # center of the ellipse
Sigma <- cov(drafted_pc) # covariance matrix of the ellipse

# percentile of the ellipse
p <- 0.70 

# draw the ellipse for drafted players
#install.packages("SIBER")
library(SIBER)
tmp <- addEllipse(mu, Sigma, p.interval = p, col = "red", lty = 2)

# Determine which of the samples are inside the ellipse
Z_samp <- pointsToEllipsoid(drafted_pc[,1:2], Sigma, mu) # convert to circle space
inside_samp <- ellipseInOut(Z_samp, p = p) # test if inside

# inside points are marked TRUE which corresponds to 1 in numeric terms, and 
# outside marked FALSE which corresponds to 0. 
# So below I calculate (1 + !inside_test) which makes 1 correspond to inside 
# and coloured black, and 2 correspond to outside and coloured red.
# and plot them with colour coding for whether they are inside or outside

points(drafted_pc[,2] ~ drafted_pc[,1], col = 1 + !inside_samp)

#Now let's make the same test with the same ellipse but with undrafted players !

plot(undrafted_pc[,2] ~ undrafted_pc[,1], type = "n", asp = 1)
tmp <- addEllipse(mu, Sigma, p.interval = p, col = "red", lty = 2)

# Determine which of the samples are inside the ellipse
Z_samp <- pointsToEllipsoid(undrafted_pc[,1:2], Sigma, mu) # convert to circle space
inside_samp <- ellipseInOut(Z_samp, p = p) # test if inside

points(undrafted_pc[,2] ~ undrafted_pc[,1], col = 1 + !inside_samp)



#Rotate the PCA according to varimax

ncomp <- 15

rawLoadings     <- x[,1:ncomp] %*% diag(PG.pca$sdev, ncomp, ncomp)

rotatedLoadings <- varimax(rawLoadings)$loadings
invLoadings     <- t(pracma::pinv(rotatedLoadings))
scores          <- scale(data[,1:58]) %*% invLoadings

print(scores[1:5,])  

scores <- as.data.frame(scores)


# Visualize !

drafted_vari <- subset(scores,Data_SG$drafted == 1)
drafted_PG <- subset(Data_PG, Data_SG$drafted == 1)

undrafted_vari <- subset(scores,Data_SG$drafted == 0)
undrafted_PG <- subset(Data_PG, Data_SG$drafted == 0)

data_d <- drafted_vari[,1:2]
data_u <- undrafted_vari[,1:2]

plot(scores[,2] ~ scores[,1],type = "n", asp = 1)

mu_d <- colMeans(data_d) # center of the ellipse
Sigma_d <- cov(data_d) # covariance matrix of the ellipse
p <- 0.7

tmp_d <- addEllipse(mu_d, Sigma_d, p.interval = p, col = "red", lty = 2)

mu_u <- colMeans(data_u) # center of the ellipse
Sigma_u <- cov(data_u) # covariance matrix of the ellipse
p <- 0.95

tmp_u <- addEllipse(mu_u, Sigma_u, p.interval = p, col = "blue", lty = 2)


# Too much data points, let's order them by contribution, then build ellipses
# We compute the contributions as the norm of the vector from the origin to the points ...

data_d <- data_d %>%mutate(contrib = sqrt(V1*V1 + V2*V2))
data_u <- data_u %>%mutate(contrib = sqrt(V1*V1 + V2*V2))

data_d <- cbind(data_d,player_name = drafted_PG$player_name)
data_u <- cbind(data_u,player_name = undrafted_PG$player_name)

best_cont_d <- subset(data_d, contrib > 1.5)
best_cont_u <- subset(data_u, contrib > 1.5)

# Determine which of the samples are inside the ellipse

# Plot the best contributing points and the ellipses ! 

plot(scores[,2] ~ scores[,1],type = "n", asp = 1)

mu_d <- colMeans(best_cont_d[,1:2]) # center of the ellipse
Sigma_d <- cov(best_cont_d[,1:2]) # covariance matrix of the ellipse
p <- 0.7

tmp_d <- addEllipse(mu_d, Sigma_d, p.interval = p, col = "red", lty = 2)

mu_u <- colMeans(best_cont_u[,1:2]) # center of the ellipse
Sigma_u <- cov(best_cont_u[,1:2]) # covariance matrix of the ellipse
p <- 0.95

tmp_u <- addEllipse(mu_u, Sigma_u, p.interval = p, col = "blue", lty = 2)

points(best_cont_u[,2] ~ best_cont_u[,1], col = "blue")
points(best_cont_d[,2] ~ best_cont_d[,1], col = "red")

d_samp <- pointsToEllipsoid(best_cont_d[,1:2], Sigma_d, mu_d) # convert to circle space
inside_samp <- ellipseInOut(d_samp, p = 0.7) # test if inside

best_cont_d <- cbind(best_cont_d, inside_samp)

best_cont_d <- mutate(best_cont_d, drafted = 1)
best_cont_u <- mutate(best_cont_u, drafted = 0)

u_samp <- pointsToEllipsoid(best_cont_u[,1:2], Sigma_d, mu_d) # convert to circle space
inside_samp <- ellipseInOut(u_samp, p = 0.95) # test if inside

best_cont_u <- cbind(best_cont_u, inside_samp)

# Now let's put all the points inside the ellipse in a dataset and check the group differences !
library("dplyr") 
draftable_group <- bind_rows(subset(best_cont_d, inside_samp == TRUE),subset(best_cont_u, inside_samp == TRUE))

draftable_group <- subset(draftable_group, select = -c(inside_samp))

plot(scores[,2] ~ scores[,1],type = "n", asp = 1)

tmp_d <- addEllipse(mu_d, Sigma_d, p.interval = p, col = "red", lty = 2)
tmp_u <- addEllipse(mu_u, Sigma_u, p.interval = p, col = "blue", lty = 2)

points(draftable_group[,2] ~ draftable_group[,1])

# We have isolated a "draftable" Group using the first two Principal Components
# Now we will perform statistical tests to analyze the differences between
# The players that actually got drafted and undrafted players !


# Let's start by applying anova/manova on the rotated PCs... 

library(ggpubr)

ggboxplot(
  draftable_group, x = "drafted", y = c("V1","V2"), 
  merge = TRUE, palette = "jco"
)

# Significant difference in variance - due to the lower number of drafted players perhaps
# But the means seem really close !

rownames(draftable_group)

#MANOVA
model <- lm(cbind(V1, V2) ~ drafted, draftable_group)
Manova(model, test.statistic = "Pillai")

mav <- manova(model, test.statistic = "Pillai")
mav
summary.aov(mav)

#install.packages('effectsize')

library(effectsize)

eta_squared(mav)

# 0.13

#

#ANOVA
one.way <- aov(V1 ~ drafted, data = draftable_group)
summary(one.way)
one.way$coefficients

# p value :3.43e-06
# P value is enough to reject Null hypothesis : to be expected
# F value : 21.89
# Mean sq


one.way <- aov(V2 ~ drafted, data = draftable_group)
summary(one.way)

# p value : 0.000181
# P value is NOT enough to reject Null hypothesis : to be expected
# F value : 14.1
# Mean sq


# very low 0.7

one.way <- aov(V1 + V2 ~ drafted, data = draftable_group)
summary(one.way)

# worse ! 

# Here we subset the original PC's from the PCA 

draftables_data <- as.data.frame(subset(PG.pca$x , rownames(Data_SG) %in% rownames(draftable_group)))

draftables_data <- draftables_data[,1:15]

draftables_data_2 <- subset(Data_SG , rownames(Data_SG) %in% rownames(draftable_group))

draftables_data <- cbind(draftables_data, drafted = draftables_data_2$drafted)

library(ggpubr)

ggboxplot(
  draftables_data, x = "drafted", y = c("PC9", "PC10", "PC11", "PC12", "PC13", "PC14", "PC15"), 
  merge = TRUE, palette = "jco"
)

ggboxplot(
  draftables_data, x = "drafted", y = c("PC1","PC15"), 
  merge = TRUE, palette = "jco"
)

# Assumptions : check mcshapiro


#MANOVA
model <- lm(cbind(PC1,PC2, PC3, PC4, PC5, PC6, PC7, PC8, PC9, PC10, PC11, PC12, PC13, PC14, PC15) ~ drafted, draftables_data)
Manova(model, test.statistic = "Pillai")

mav <- manova(model, test.statistic = "Pillai")
mav

summary.aov(mav)

#ANOVA
one.way <- aov(PC10 ~ drafted, data = draftables_data)
summary(one.way)

# Very High F, PC1 : 153.2 ; PC10 : 35.16; 

two.way <- aov(drafted ~ PC1 + PC2, data = draftables_data)

summary(two.way)

# Non negligeable F = 25.48
# Now on the actual variables

# We figured out that the first Principal component explains most of the difference
# Between the two groups, let's see the main loadings of that component !

fviz_eig(PG.pca)
# Select the top 5 contributing variables
fviz_pca_var(PG.pca, select.var = list(contrib = 10))

ncomp <- 15

rawLoadings <- x[,1:ncomp] %*% diag(PG.pca$sdev, ncomp, ncomp)

rawLoadings <-as.data.frame(rawLoadings)

rawLoadings <-cbind(rawLoadings, var = var_names)

rawLoadings[,1]

ordered_load <- rawLoadings[order(rawLoadings$V1),]

ordered_load[,1]

# We therefore can extract the 10 variables with the biggest loadings! 

vars <- ordered_load[48:58,16]


draftables_data <- subset(Data_SG , rownames(Data_SG) %in% rownames(draftable_group))

vars

#ANOVA
one.way <- aov(X.gbpm ~ drafted, data = draftables_data)
summary(one.way)

# F : 150.3 - p :0

one.way <- aov(porpag ~ drafted, data = draftables_data)
summary(one.way)

# F : 61.66- p : 0.

one.way <- aov(adjoe ~ drafted, data = draftables_data)
summary(one.way)

# F : 61.77 - p : 0.

one.way <- aov(X.bpm ~ drafted, data = draftables_data)
summary(one.way)

# F : 149.2 - p : 0.

one.way <- aov(X.stops ~ drafted, data = draftables_data)
summary(one.way)

# F :78.8 - p : 0.

one.way <- aov(ogbpm ~ drafted, data = draftables_data)
summary(one.way)

# F : 54.26 - p : 0.

one.way <- aov(twoPM ~ drafted, data = draftables_data)
summary(one.way)

# F : 45.93 - p : 0.


one.way <- aov(X.rimmade ~ drafted, data = draftables_data)
summary(one.way)

# F : 50.18 - p : 0.

# Conclusion : BPM - adjoe - porpag ----- STOPS - twoPM ----- PTS - ORtg....

# Let's now try to run a MANOVA model on these variables

#MANOVA
model <- lm(cbind(X.bpm, adjoe, porpag, X.stops, twoPM, ORtg, pts) ~ drafted, draftables_data)
Manova(model, test.statistic = "Pillai")

# F : 20.325

model <- lm(cbind(X.bpm, adjoe, porpag, X.stops, twoPM) ~ drafted, draftables_data)
Manova(model, test.statistic = "Pillai")

#F : 26.97

model <- lm(cbind(X.bpm, adjoe, porpag) ~ drafted, draftables_data)
Manova(model, test.statistic = "Pillai")

#F : 39.152

### We have an interpretation of the main differences between the draftable players
### Things to do now : Establish the "draftable" players group in the 2023 class
### See which players would have the tendency to get drafted according to our study
### Build the NBA predictor model to have a projection of the impact players will have in the NBA


######
## GOOD PCA - SF
######
data <- subset(Data_SF, select = -c(player_name,drafted))

PG.pca <- prcomp(data, scale = TRUE)

fviz_eig(PG.pca)

x <- PG.pca$rotation

var_names <- colnames(data)


# Select the top 200 contributing individuals
fviz_pca_ind(PG.pca,label="var", habillage=Data_SF$drafted,
             addEllipses=TRUE, ellipse.level=0.95, select.ind = list(contrib = 500))

# Select the top 5 contributing variables
fviz_pca_var(PG.pca, select.var = list(contrib = 10))

# 50 best indiv + 5 best variables
fviz_pca_biplot(PG.pca, label="var", habillage=Data_SF$drafted,
                addEllipses=TRUE, ellipse.level=0.95, select.ind = list(contrib = 200), select.var = list(contrib =10))

drafted_pc <- subset(PG.pca$x, Data_SF$drafted == 1)
undrafted_pc <- subset(PG.pca$x, Data_SF$drafted == 0)

drafted_pc <- drafted_pc[,1:2]
undrafted_pc <- undrafted_pc[,1:2]

# Find points within the ellipse of confidence for drafted

plot(drafted_pc[,2] ~ drafted_pc[,1], type = "n", asp = 1)

# add an ellipse, in this case a 95% ellipse
mu <- colMeans(drafted_pc) # center of the ellipse
Sigma <- cov(drafted_pc) # covariance matrix of the ellipse

# percentile of the ellipse
p <- 0.70 

# draw the ellipse for drafted players
#install.packages("SIBER")
library(SIBER)
tmp <- addEllipse(mu, Sigma, p.interval = p, col = "red", lty = 2)

# Determine which of the samples are inside the ellipse
Z_samp <- pointsToEllipsoid(drafted_pc[,1:2], Sigma, mu) # convert to circle space
inside_samp <- ellipseInOut(Z_samp, p = p) # test if inside

# inside points are marked TRUE which corresponds to 1 in numeric terms, and 
# outside marked FALSE which corresponds to 0. 
# So below I calculate (1 + !inside_test) which makes 1 correspond to inside 
# and coloured black, and 2 correspond to outside and coloured red.
# and plot them with colour coding for whether they are inside or outside

points(drafted_pc[,2] ~ drafted_pc[,1], col = 1 + !inside_samp)

#Now let's make the same test with the same ellipse but with undrafted players !

plot(undrafted_pc[,2] ~ undrafted_pc[,1], type = "n", asp = 1)
tmp <- addEllipse(mu, Sigma, p.interval = p, col = "red", lty = 2)

# Determine which of the samples are inside the ellipse
Z_samp <- pointsToEllipsoid(undrafted_pc[,1:2], Sigma, mu) # convert to circle space
inside_samp <- ellipseInOut(Z_samp, p = p) # test if inside

points(undrafted_pc[,2] ~ undrafted_pc[,1], col = 1 + !inside_samp)



#Rotate the PCA according to varimax

ncomp <- 15

rawLoadings     <- x[,1:ncomp] %*% diag(PG.pca$sdev, ncomp, ncomp)

rotatedLoadings <- varimax(rawLoadings)$loadings
invLoadings     <- t(pracma::pinv(rotatedLoadings))
scores          <- scale(data[,1:58]) %*% invLoadings

print(scores[1:5,])  

scores <- as.data.frame(scores)


# Visualize !

drafted_vari <- subset(scores,Data_SF$drafted == 1)
drafted_PG <- subset(Data_PG, Data_SF$drafted == 1)

undrafted_vari <- subset(scores,Data_SF$drafted == 0)
undrafted_PG <- subset(Data_PG, Data_SF$drafted == 0)

data_d <- drafted_vari[,1:2]
data_u <- undrafted_vari[,1:2]

plot(scores[,2] ~ scores[,1],type = "n", asp = 1)

mu_d <- colMeans(data_d) # center of the ellipse
Sigma_d <- cov(data_d) # covariance matrix of the ellipse
p <- 0.7

tmp_d <- addEllipse(mu_d, Sigma_d, p.interval = p, col = "red", lty = 2)

mu_u <- colMeans(data_u) # center of the ellipse
Sigma_u <- cov(data_u) # covariance matrix of the ellipse
p <- 0.95

tmp_u <- addEllipse(mu_u, Sigma_u, p.interval = p, col = "blue", lty = 2)


# Too much data points, let's order them by contribution, then build ellipses
# We compute the contributions as the norm of the vector from the origin to the points ...

data_d <- data_d %>%mutate(contrib = sqrt(V1*V1 + V2*V2))
data_u <- data_u %>%mutate(contrib = sqrt(V1*V1 + V2*V2))

data_d <- cbind(data_d,player_name = drafted_PG$player_name)
data_u <- cbind(data_u,player_name = undrafted_PG$player_name)

best_cont_d <- subset(data_d, contrib > 1.5)
best_cont_u <- subset(data_u, contrib > 1.5)

# Determine which of the samples are inside the ellipse

# Plot the best contributing points and the ellipses ! 

plot(scores[,2] ~ scores[,1],type = "n", asp = 1)

mu_d <- colMeans(best_cont_d[,1:2]) # center of the ellipse
Sigma_d <- cov(best_cont_d[,1:2]) # covariance matrix of the ellipse
p <- 0.7

tmp_d <- addEllipse(mu_d, Sigma_d, p.interval = p, col = "red", lty = 2)

mu_u <- colMeans(best_cont_u[,1:2]) # center of the ellipse
Sigma_u <- cov(best_cont_u[,1:2]) # covariance matrix of the ellipse
p <- 0.95

tmp_u <- addEllipse(mu_u, Sigma_u, p.interval = p, col = "blue", lty = 2)

points(best_cont_u[,2] ~ best_cont_u[,1], col = "blue")
points(best_cont_d[,2] ~ best_cont_d[,1], col = "red")

d_samp <- pointsToEllipsoid(best_cont_d[,1:2], Sigma_d, mu_d) # convert to circle space
inside_samp <- ellipseInOut(d_samp, p = 0.7) # test if inside

best_cont_d <- cbind(best_cont_d, inside_samp)

best_cont_d <- mutate(best_cont_d, drafted = 1)
best_cont_u <- mutate(best_cont_u, drafted = 0)

u_samp <- pointsToEllipsoid(best_cont_u[,1:2], Sigma_d, mu_d) # convert to circle space
inside_samp <- ellipseInOut(u_samp, p = 0.95) # test if inside

best_cont_u <- cbind(best_cont_u, inside_samp)

# Now let's put all the points inside the ellipse in a dataset and check the group differences !
library("dplyr") 
draftable_group <- bind_rows(subset(best_cont_d, inside_samp == TRUE),subset(best_cont_u, inside_samp == TRUE))

draftable_group <- subset(draftable_group, select = -c(inside_samp))

plot(scores[,2] ~ scores[,1],type = "n", asp = 1)

tmp_d <- addEllipse(mu_d, Sigma_d, p.interval = p, col = "red", lty = 2)
tmp_u <- addEllipse(mu_u, Sigma_u, p.interval = p, col = "blue", lty = 2)

points(draftable_group[,2] ~ draftable_group[,1])

# We have isolated a "draftable" Group using the first two Principal Components
# Now we will perform statistical tests to analyze the differences between
# The players that actually got drafted and undrafted players !


# Let's start by applying anova/manova on the rotated PCs... 

library(ggpubr)

ggboxplot(
  draftable_group, x = "drafted", y = c("V1","V2"), 
  merge = TRUE, palette = "jco"
)

# Significant difference in variance - due to the lower number of drafted players perhaps
# But the means seem really close !

rownames(draftable_group)

#MANOVA
model <- lm(cbind(V1, V2) ~ drafted, draftable_group)
Manova(model, test.statistic = "Pillai")

mav <- manova(model, test.statistic = "Pillai")
mav
summary.aov(mav)

# Here PC2 has more importance !

#install.packages('effectsize')

library(effectsize)

eta_squared(mav)

# 0.12

#

#ANOVA
one.way <- aov(V1 ~ drafted, data = draftable_group)
summary(one.way)
one.way$coefficients

# p value 0.000202
# P value is enough to reject Null hypothesis : to be expected
# F value : 14
# Mean sq


one.way <- aov(V2 ~ drafted, data = draftable_group)
summary(one.way)

# p value : 7.62e-06
# P value is BETTER
# F value : 20.42
# Mean sq


# very low 0.7

one.way <- aov(V1 + V2 ~ drafted, data = draftable_group)
summary(one.way)

# worse ! 

# Here we subset the original PC's from the PCA 

draftables_data <- as.data.frame(subset(PG.pca$x , rownames(Data_SF) %in% rownames(draftable_group)))

draftables_data <- draftables_data[,1:15]

draftables_data_2 <- subset(Data_SF , rownames(Data_SF) %in% rownames(draftable_group))

draftables_data <- cbind(draftables_data, drafted = draftables_data_2$drafted)

library(ggpubr)

ggboxplot(
  draftables_data, x = "drafted", y = c("PC9", "PC10", "PC11", "PC12", "PC13", "PC14", "PC15"), 
  merge = TRUE, palette = "jco"
)

ggboxplot(
  draftables_data, x = "drafted", y = c("PC1","PC15"), 
  merge = TRUE, palette = "jco"
)

# Assumptions : check mcshapiro


#MANOVA
model <- lm(cbind(PC1,PC2, PC3, PC4, PC5, PC6, PC7, PC8, PC9, PC10, PC11, PC12, PC13, PC14, PC15) ~ drafted, draftables_data)
Manova(model, test.statistic = "Pillai")

mav <- manova(model, test.statistic = "Pillai")
mav

summary.aov(mav)

#ANOVA
one.way <- aov(PC2 ~ drafted, data = draftables_data)
summary(one.way)

# Very High F, PC1 : 95.788 ; PC5 : 32.521; 

two.way <- aov(drafted ~ PC1 + PC5, data = draftables_data)

summary(two.way)

# 
# Now on the actual variables

# We figured out that the first Principal component explains most of the difference
# Between the two groups, let's see the main loadings of that component !

fviz_eig(PG.pca)
# Select the top 5 contributing variables
fviz_pca_var(PG.pca, select.var = list(contrib = 10))

ncomp <- 15

rawLoadings <- x[,1:ncomp] %*% diag(PG.pca$sdev, ncomp, ncomp)

rawLoadings <-as.data.frame(rawLoadings)

rawLoadings <-cbind(rawLoadings, var = var_names)

rawLoadings[,1]

ordered_load <- rawLoadings[order(rawLoadings$V1),]

ordered_load[,1]

# We therefore can extract the 10 variables with the biggest loadings! 

vars <- ordered_load[48:58,16]


draftables_data <- subset(Data_SF , rownames(Data_SF) %in% rownames(draftable_group))

vars

#ANOVA
one.way <- aov(adrtg ~ drafted, data = draftables_data)
summary(one.way)

# F : 97.56- p :0

one.way <- aov(X.drtg ~ drafted, data = draftables_data)
summary(one.way)

# F : 39.12 - p :0

one.way <- aov(pid ~ drafted, data = draftables_data)
summary(one.way)

#This is player id so absolutely not relevant...

# F : 0.038 - p :0.7

one.way <- aov(TPA ~ drafted, data = draftables_data)
summary(one.way)

# F : 14.94- p :0.000796

one.way <- aov(conf ~ drafted, data = draftables_data)
summary(one.way)

# F : 26.95- p :0.000796

one.way <- aov(TPM ~ drafted, data = draftables_data)
summary(one.way)

# F : 13.78 - p :0.000796

one.way <- aov(TO_per ~ drafted, data = draftables_data)
summary(one.way)

# F :  0.145 - p :0.000796
one.way <- aov(FT_per ~ drafted, data = draftables_data)
summary(one.way)

# F :1.915 - p :0.000796
one.way <- aov(TP_per ~ drafted, data = draftables_data)
summary(one.way)

# F : 2.50 - p :0.000796

#####
## GOOD PCA - PF
#####


data <- subset(Data_PF, select = -c(player_name,drafted))

PG.pca <- prcomp(data, scale = TRUE)

fviz_eig(PG.pca)

x <- PG.pca$rotation

var_names <- colnames(data)


# Select the top 200 contributing individuals
fviz_pca_ind(PG.pca,label="var", habillage=Data_PF$drafted,
             addEllipses=TRUE, ellipse.level=0.95, select.ind = list(contrib = 500))

# Select the top 5 contributing variables
fviz_pca_var(PG.pca, select.var = list(contrib = 10))

# 50 best indiv + 5 best variables
fviz_pca_biplot(PG.pca, label="var", habillage=Data_PF$drafted,
                addEllipses=TRUE, ellipse.level=0.95, select.ind = list(contrib = 200), select.var = list(contrib =10))

drafted_pc <- subset(PG.pca$x, Data_PF$drafted == 1)
undrafted_pc <- subset(PG.pca$x, Data_PF$drafted == 0)

drafted_pc <- drafted_pc[,1:2]
undrafted_pc <- undrafted_pc[,1:2]

# Find points within the ellipse of confidence for drafted

plot(drafted_pc[,2] ~ drafted_pc[,1], type = "n", asp = 1)

# add an ellipse, in this case a 95% ellipse
mu <- colMeans(drafted_pc) # center of the ellipse
Sigma <- cov(drafted_pc) # covariance matrix of the ellipse

# percentile of the ellipse
p <- 0.70 

# draw the ellipse for drafted players
#install.packages("SIBER")
library(SIBER)
tmp <- addEllipse(mu, Sigma, p.interval = p, col = "red", lty = 2)

# Determine which of the samples are inside the ellipse
Z_samp <- pointsToEllipsoid(drafted_pc[,1:2], Sigma, mu) # convert to circle space
inside_samp <- ellipseInOut(Z_samp, p = p) # test if inside

# inside points are marked TRUE which corresponds to 1 in numeric terms, and 
# outside marked FALSE which corresponds to 0. 
# So below I calculate (1 + !inside_test) which makes 1 correspond to inside 
# and coloured black, and 2 correspond to outside and coloured red.
# and plot them with colour coding for whether they are inside or outside

points(drafted_pc[,2] ~ drafted_pc[,1], col = 1 + !inside_samp)

#Now let's make the same test with the same ellipse but with undrafted players !

plot(undrafted_pc[,2] ~ undrafted_pc[,1], type = "n", asp = 1)
tmp <- addEllipse(mu, Sigma, p.interval = p, col = "red", lty = 2)

# Determine which of the samples are inside the ellipse
Z_samp <- pointsToEllipsoid(undrafted_pc[,1:2], Sigma, mu) # convert to circle space
inside_samp <- ellipseInOut(Z_samp, p = p) # test if inside

points(undrafted_pc[,2] ~ undrafted_pc[,1], col = 1 + !inside_samp)



#Rotate the PCA according to varimax

ncomp <- 15

rawLoadings     <- x[,1:ncomp] %*% diag(PG.pca$sdev, ncomp, ncomp)

rotatedLoadings <- varimax(rawLoadings)$loadings
invLoadings     <- t(pracma::pinv(rotatedLoadings))
scores          <- scale(data[,1:58]) %*% invLoadings

print(scores[1:5,])  

scores <- as.data.frame(scores)


# Visualize !

drafted_vari <- subset(scores,Data_PF$drafted == 1)
drafted_PG <- subset(Data_PG, Data_PF$drafted == 1)

undrafted_vari <- subset(scores,Data_PF$drafted == 0)
undrafted_PG <- subset(Data_PG, Data_PF$drafted == 0)

data_d <- drafted_vari[,1:2]
data_u <- undrafted_vari[,1:2]

plot(scores[,2] ~ scores[,1],type = "n", asp = 1)

mu_d <- colMeans(data_d) # center of the ellipse
Sigma_d <- cov(data_d) # covariance matrix of the ellipse
p <- 0.7

tmp_d <- addEllipse(mu_d, Sigma_d, p.interval = p, col = "red", lty = 2)

mu_u <- colMeans(data_u) # center of the ellipse
Sigma_u <- cov(data_u) # covariance matrix of the ellipse
p <- 0.95

tmp_u <- addEllipse(mu_u, Sigma_u, p.interval = p, col = "blue", lty = 2)


# Too much data points, let's order them by contribution, then build ellipses
# We compute the contributions as the norm of the vector from the origin to the points ...

data_d <- data_d %>%mutate(contrib = sqrt(V1*V1 + V2*V2))
data_u <- data_u %>%mutate(contrib = sqrt(V1*V1 + V2*V2))

data_d <- cbind(data_d,player_name = drafted_PG$player_name)
data_u <- cbind(data_u,player_name = undrafted_PG$player_name)

best_cont_d <- subset(data_d, contrib > 1.5)
best_cont_u <- subset(data_u, contrib > 1.5)

# Determine which of the samples are inside the ellipse

# Plot the best contributing points and the ellipses ! 

plot(scores[,2] ~ scores[,1],type = "n", asp = 1)

mu_d <- colMeans(best_cont_d[,1:2]) # center of the ellipse
Sigma_d <- cov(best_cont_d[,1:2]) # covariance matrix of the ellipse
p <- 0.65

tmp_d <- addEllipse(mu_d, Sigma_d, p.interval = p, col = "red", lty = 2)

mu_u <- colMeans(best_cont_u[,1:2]) # center of the ellipse
Sigma_u <- cov(best_cont_u[,1:2]) # covariance matrix of the ellipse
p <- 0.95

tmp_u <- addEllipse(mu_u, Sigma_u, p.interval = p, col = "blue", lty = 2)

points(best_cont_u[,2] ~ best_cont_u[,1], col = "blue")
points(best_cont_d[,2] ~ best_cont_d[,1], col = "red")

d_samp <- pointsToEllipsoid(best_cont_d[,1:2], Sigma_d, mu_d) # convert to circle space
inside_samp <- ellipseInOut(d_samp, p = 0.7) # test if inside

best_cont_d <- cbind(best_cont_d, inside_samp)

best_cont_d <- mutate(best_cont_d, drafted = 1)
best_cont_u <- mutate(best_cont_u, drafted = 0)

u_samp <- pointsToEllipsoid(best_cont_u[,1:2], Sigma_d, mu_d) # convert to circle space
inside_samp <- ellipseInOut(u_samp, p = 0.95) # test if inside

best_cont_u <- cbind(best_cont_u, inside_samp)

# Now let's put all the points inside the ellipse in a dataset and check the group differences !
library("dplyr") 
draftable_group <- bind_rows(subset(best_cont_d, inside_samp == TRUE),subset(best_cont_u, inside_samp == TRUE))

draftable_group <- subset(draftable_group, select = -c(inside_samp))

plot(scores[,2] ~ scores[,1],type = "n", asp = 1)

tmp_d <- addEllipse(mu_d, Sigma_d, p.interval = p, col = "red", lty = 2)
tmp_u <- addEllipse(mu_u, Sigma_u, p.interval = p, col = "blue", lty = 2)

points(draftable_group[,2] ~ draftable_group[,1])

# We have isolated a "draftable" Group using the first two Principal Components
# Now we will perform statistical tests to analyze the differences between
# The players that actually got drafted and undrafted players !


# Let's start by applying anova/manova on the rotated PCs... 

library(ggpubr)

ggboxplot(
  draftable_group, x = "drafted", y = c("V1","V2"), 
  merge = TRUE, palette = "jco"
)

# Significant difference in variance - due to the lower number of drafted players perhaps
# But the means seem really close !

rownames(draftable_group)

#MANOVA
model <- lm(cbind(V1, V2) ~ drafted, draftable_group)
Manova(model, test.statistic = "Pillai")

mav <- manova(model, test.statistic = "Pillai")
mav
summary.aov(mav)

# Here PC2 has more importance !

#install.packages('effectsize')

library(effectsize)

eta_squared(mav)

# 0.07

#

#ANOVA
one.way <- aov(V1 ~ drafted, data = draftable_group)
summary(one.way)
one.way$coefficients

# p value 0.000202
# P value is enough to reject Null hypothesis : to be expected
# F value : 14
# Mean sq


one.way <- aov(V2 ~ drafted, data = draftable_group)
summary(one.way)

# p value : 7.62e-06
# P value is BETTER
# F value : 5.925
# Mean sq


# very low 0.7

one.way <- aov(V1 + V2 ~ drafted, data = draftable_group)
summary(one.way)

# worse ! 

# Here we subset the original PC's from the PCA 

draftables_data <- as.data.frame(subset(PG.pca$x , rownames(Data_PF) %in% rownames(draftable_group)))

draftables_data <- draftables_data[,1:15]

draftables_data_2 <- subset(Data_PF , rownames(Data_PF) %in% rownames(draftable_group))

draftables_data <- cbind(draftables_data, drafted = draftables_data_2$drafted)

library(ggpubr)

ggboxplot(
  draftables_data, x = "drafted", y = c("PC9", "PC10", "PC11", "PC12", "PC13", "PC14", "PC15"), 
  merge = TRUE, palette = "jco"
)

ggboxplot(
  draftables_data, x = "drafted", y = c("PC1","PC15"), 
  merge = TRUE, palette = "jco"
)

# Assumptions : check mcshapiro


#MANOVA
model <- lm(cbind(PC1,PC2, PC3, PC4, PC5, PC6, PC7, PC8, PC9, PC10, PC11, PC12, PC13, PC14, PC15) ~ drafted, draftables_data)
Manova(model, test.statistic = "Pillai")

mav <- manova(model, test.statistic = "Pillai")
mav

summary.aov(mav)

#ANOVA
one.way <- aov(PC2 ~ drafted, data = draftables_data)
summary(one.way)

# Very LOW F, PC1 : 26.237 ; PC5 : 9.3884; 

two.way <- aov(drafted ~ PC1 + PC5, data = draftables_data)

summary(two.way)

# 
# Now on the actual variables

# We figured out that the first Principal component explains most of the difference
# Between the two groups, let's see the main loadings of that component !

fviz_eig(PG.pca)
# Select the top 5 contributing variables
fviz_pca_var(PG.pca, select.var = list(contrib = 10))

ncomp <- 15

rawLoadings <- x[,1:ncomp] %*% diag(PG.pca$sdev, ncomp, ncomp)

rawLoadings <-as.data.frame(rawLoadings)

rawLoadings <-cbind(rawLoadings, var = var_names)

rawLoadings[,1]

ordered_load <- rawLoadings[order(rawLoadings$V1),]

ordered_load[,1]

# We therefore can extract the 10 variables with the biggest loadings! 

vars <- ordered_load[48:58,16]


draftables_data <- subset(Data_PF , rownames(Data_PF) %in% rownames(draftable_group))

vars

#ANOVA
one.way <- aov(adrtg ~ drafted, data = draftables_data)
summary(one.way)

# F : 17.6- p :0

one.way <- aov(X.drtg ~ drafted, data = draftables_data)
summary(one.way)

# F : 5.556 - p :0

one.way <- aov(TO_per ~ drafted, data = draftables_data)
summary(one.way)

# F : 1.91 - p :0.7

one.way <- aov(pfr ~ drafted, data = draftables_data)
summary(one.way)

# F : 4.712 - p :0.000796

one.way <- aov(conf ~ drafted, data = draftables_data)
summary(one.way)

# F : 0.358 - p :0.000796

one.way <- aov(TPA ~ drafted, data = draftables_data)
summary(one.way)

# F : 5.415 - p :0.000796

one.way <- aov(TPM ~ drafted, data = draftables_data)
summary(one.way)

# F : 6.743- p :0.000796

one.way <- aov(yr ~ drafted, data = draftables_data)
summary(one.way)

# F : 6.868 - p :0.000796
one.way <- aov(X.ast.tov ~ drafted, data = draftables_data)
summary(one.way)

# F :  1.322 - p :0.000796

#####
## GOOD PCA - C
#####


data <- subset(Data_C, select = -c(player_name,drafted))

PG.pca <- prcomp(data, scale = TRUE)

fviz_eig(PG.pca)

x <- PG.pca$rotation

var_names <- colnames(data)


# Select the top 200 contributing individuals
fviz_pca_ind(PG.pca,label="var", habillage=Data_C$drafted,
             addEllipses=TRUE, ellipse.level=0.95, select.ind = list(contrib = 500))

# Select the top 5 contributing variables
fviz_pca_var(PG.pca, select.var = list(contrib = 10))

# 50 best indiv + 5 best variables
fviz_pca_biplot(PG.pca, label="var", habillage=Data_C$drafted,
                addEllipses=TRUE, ellipse.level=0.95, select.ind = list(contrib = 200), select.var = list(contrib =10))

drafted_pc <- subset(PG.pca$x, Data_C$drafted == 1)
undrafted_pc <- subset(PG.pca$x, Data_C$drafted == 0)

drafted_pc <- drafted_pc[,1:2]
undrafted_pc <- undrafted_pc[,1:2]

# Find points within the ellipse of confidence for drafted

plot(drafted_pc[,2] ~ drafted_pc[,1], type = "n", asp = 1)

# add an ellipse, in this case a 95% ellipse
mu <- colMeans(drafted_pc) # center of the ellipse
Sigma <- cov(drafted_pc) # covariance matrix of the ellipse

# percentile of the ellipse
p <- 0.70 

# draw the ellipse for drafted players
#install.packages("SIBER")
library(SIBER)
tmp <- addEllipse(mu, Sigma, p.interval = p, col = "red", lty = 2)

# Determine which of the samples are inside the ellipse
Z_samp <- pointsToEllipsoid(drafted_pc[,1:2], Sigma, mu) # convert to circle space
inside_samp <- ellipseInOut(Z_samp, p = p) # test if inside

# inside points are marked TRUE which corresponds to 1 in numeric terms, and 
# outside marked FALSE which corresponds to 0. 
# So below I calculate (1 + !inside_test) which makes 1 correspond to inside 
# and coloured black, and 2 correspond to outside and coloured red.
# and plot them with colour coding for whether they are inside or outside

points(drafted_pc[,2] ~ drafted_pc[,1], col = 1 + !inside_samp)

#Now let's make the same test with the same ellipse but with undrafted players !

plot(undrafted_pc[,2] ~ undrafted_pc[,1], type = "n", asp = 1)
tmp <- addEllipse(mu, Sigma, p.interval = p, col = "red", lty = 2)

# Determine which of the samples are inside the ellipse
Z_samp <- pointsToEllipsoid(undrafted_pc[,1:2], Sigma, mu) # convert to circle space
inside_samp <- ellipseInOut(Z_samp, p = p) # test if inside

points(undrafted_pc[,2] ~ undrafted_pc[,1], col = 1 + !inside_samp)



#Rotate the PCA according to varimax

ncomp <- 15

rawLoadings     <- x[,1:ncomp] %*% diag(PG.pca$sdev, ncomp, ncomp)

rotatedLoadings <- varimax(rawLoadings)$loadings
invLoadings     <- t(pracma::pinv(rotatedLoadings))
scores          <- scale(data[,1:58]) %*% invLoadings

print(scores[1:5,])  

scores <- as.data.frame(scores)


# Visualize !

drafted_vari <- subset(scores,Data_C$drafted == 1)
drafted_PG <- subset(Data_C, Data_C$drafted == 1)

undrafted_vari <- subset(scores,Data_C$drafted == 0)
undrafted_PG <- subset(Data_C, Data_C$drafted == 0)

data_d <- drafted_vari[,1:2]
data_u <- undrafted_vari[,1:2]

plot(scores[,2] ~ scores[,1],type = "n", asp = 1)

mu_d <- colMeans(data_d) # center of the ellipse
Sigma_d <- cov(data_d) # covariance matrix of the ellipse
p <- 0.7

tmp_d <- addEllipse(mu_d, Sigma_d, p.interval = p, col = "red", lty = 2)

mu_u <- colMeans(data_u) # center of the ellipse
Sigma_u <- cov(data_u) # covariance matrix of the ellipse
p <- 0.95

tmp_u <- addEllipse(mu_u, Sigma_u, p.interval = p, col = "blue", lty = 2)


# Too much data points, let's order them by contribution, then build ellipses
# We compute the contributions as the norm of the vector from the origin to the points ...

data_d <- data_d %>%mutate(contrib = sqrt(V1*V1 + V2*V2))
data_u <- data_u %>%mutate(contrib = sqrt(V1*V1 + V2*V2))

data_d <- cbind(data_d,player_name = drafted_PG$player_name)
data_u <- cbind(data_u,player_name = undrafted_PG$player_name)

best_cont_d <- subset(data_d, contrib > 1.5)
best_cont_u <- subset(data_u, contrib > 1.5)

# Determine which of the samples are inside the ellipse

# Plot the best contributing points and the ellipses ! 

plot(scores[,2] ~ scores[,1],type = "n", asp = 1)

mu_d <- colMeans(best_cont_d[,1:2]) # center of the ellipse
Sigma_d <- cov(best_cont_d[,1:2]) # covariance matrix of the ellipse
p <- 0.65

tmp_d <- addEllipse(mu_d, Sigma_d, p.interval = p, col = "red", lty = 2)

mu_u <- colMeans(best_cont_u[,1:2]) # center of the ellipse
Sigma_u <- cov(best_cont_u[,1:2]) # covariance matrix of the ellipse
p <- 0.95

tmp_u <- addEllipse(mu_u, Sigma_u, p.interval = p, col = "blue", lty = 2)

points(best_cont_u[,2] ~ best_cont_u[,1], col = "blue")
points(best_cont_d[,2] ~ best_cont_d[,1], col = "red")

d_samp <- pointsToEllipsoid(best_cont_d[,1:2], Sigma_d, mu_d) # convert to circle space
inside_samp <- ellipseInOut(d_samp, p = 0.7) # test if inside

best_cont_d <- cbind(best_cont_d, inside_samp)

best_cont_d <- mutate(best_cont_d, drafted = 1)
best_cont_u <- mutate(best_cont_u, drafted = 0)

u_samp <- pointsToEllipsoid(best_cont_u[,1:2], Sigma_d, mu_d) # convert to circle space
inside_samp <- ellipseInOut(u_samp, p = 0.95) # test if inside

best_cont_u <- cbind(best_cont_u, inside_samp)

# Now let's put all the points inside the ellipse in a dataset and check the group differences !
library("dplyr") 
draftable_group <- bind_rows(subset(best_cont_d, inside_samp == TRUE),subset(best_cont_u, inside_samp == TRUE))

draftable_group <- subset(draftable_group, select = -c(inside_samp))

plot(scores[,2] ~ scores[,1],type = "n", asp = 1)

tmp_d <- addEllipse(mu_d, Sigma_d, p.interval = p, col = "red", lty = 2)
tmp_u <- addEllipse(mu_u, Sigma_u, p.interval = p, col = "blue", lty = 2)

points(draftable_group[,2] ~ draftable_group[,1])

# We have isolated a "draftable" Group using the first two Principal Components
# Now we will perform statistical tests to analyze the differences between
# The players that actually got drafted and undrafted players !


# Let's start by applying anova/manova on the rotated PCs... 

library(ggpubr)

ggboxplot(
  draftable_group, x = "drafted", y = c("V1","V2"), 
  merge = TRUE, palette = "jco"
)

# Significant difference in variance - due to the lower number of drafted players perhaps
# But the means seem really close !

rownames(draftable_group)

#MANOVA
model <- lm(cbind(V1, V2) ~ drafted, draftable_group)
Manova(model, test.statistic = "Pillai")

mav <- manova(model, test.statistic = "Pillai")
mav
summary.aov(mav)

# Here PC2 has more importance !

#install.packages('effectsize')

library(effectsize)

eta_squared(mav)

# 0.07

#

#ANOVA
one.way <- aov(V1 ~ drafted, data = draftable_group)
summary(one.way)
one.way$coefficients

# p value  0.598
# P value is NOT enough to reject Null hypothesis : to be expected
# F value : 0.279
# Mean sq


one.way <- aov(V2 ~ drafted, data = draftable_group)
summary(one.way)

# p value : 0.0061 
# P value is BETTER
# F value : 7.817
# Mean sq


# very low 0.7

one.way <- aov(V1 + V2 ~ drafted, data = draftable_group)
summary(one.way)

# worse ! 

# Here we subset the original PC's from the PCA 

draftables_data <- as.data.frame(subset(PG.pca$x , rownames(Data_C) %in% rownames(draftable_group)))

draftables_data <- draftables_data[,1:15]

draftables_data_2 <- subset(Data_C , rownames(Data_C) %in% rownames(draftable_group))

draftables_data <- cbind(draftables_data, drafted = draftables_data_2$drafted)

library(ggpubr)

ggboxplot(
  draftables_data, x = "drafted", y = c("PC9", "PC10", "PC11", "PC12", "PC13", "PC14", "PC15"), 
  merge = TRUE, palette = "jco"
)

ggboxplot(
  draftables_data, x = "drafted", y = c("PC1","PC15"), 
  merge = TRUE, palette = "jco"
)

# Assumptions : check mcshapiro


#MANOVA
model <- lm(cbind(PC1,PC2, PC3, PC4, PC5, PC6, PC7, PC8, PC9, PC10, PC11, PC12, PC13, PC14, PC15) ~ drafted, draftables_data)
Manova(model, test.statistic = "Pillai")

mav <- manova(model, test.statistic = "Pillai")
mav

summary.aov(mav)

#ANOVA
one.way <- aov(PC2 ~ drafted, data = draftables_data)
summary(one.way)

# Very LOW F, PC1 : 31.994 ; PC13 : 10.854; 

two.way <- aov(drafted ~ PC1 + PC13, data = draftables_data)

summary(two.way)

# 
# Now on the actual variables

# We figured out that the first Principal component explains most of the difference
# Between the two groups, let's see the main loadings of that component !

fviz_eig(PG.pca)
# Select the top 5 contributing variables
fviz_pca_var(PG.pca, select.var = list(contrib = 10))

ncomp <- 15

rawLoadings <- x[,1:ncomp] %*% diag(PG.pca$sdev, ncomp, ncomp)

rawLoadings <-as.data.frame(rawLoadings)

rawLoadings <-cbind(rawLoadings, var = var_names)

rawLoadings[,1]

ordered_load <- rawLoadings[order(rawLoadings$V1),]

ordered_load[,1]

# We therefore can extract the 10 variables with the biggest loadings! 

vars <- ordered_load[48:58,16]


draftables_data <- subset(Data_C, rownames(Data_C) %in% rownames(draftable_group))

vars

#ANOVA
one.way <- aov(adrtg ~ drafted, data = draftables_data)
summary(one.way)

# F : 18.97 - p :0

one.way <- aov(X.drtg ~ drafted, data = draftables_data)
summary(one.way)

# F : 4.602 - p :0

one.way <- aov(TO_per ~ drafted, data = draftables_data)
summary(one.way)

# F : 0.219 - p 

one.way <- aov(pfr ~ drafted, data = draftables_data)
summary(one.way)

# F : 0.222 - p 

one.way <- aov(conf ~ drafted, data = draftables_data)
summary(one.way)

# F : 1.188 - p 

one.way <- aov(yr ~ drafted, data = draftables_data)
summary(one.way)

# F :3.077- p :0.0821

one.way <- aov(TPA ~ drafted, data = draftables_data)
summary(one.way)

# F : 0.126 - p 

one.way <- aov(TPM ~ drafted, data = draftables_data)
summary(one.way)

# F : 0.125 - p


one.way <- aov(X.midmade..midmade.midmiss. ~ drafted, data = draftables_data)
summary(one.way)

# F :  2.04 - p :0..155
