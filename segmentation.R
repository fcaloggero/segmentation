## Solo 1 - Market Segmentation ##

setwd("~/Desktop/")

load("Data.RData")

#Library will load the existing loaded package. 
#Require will install or update when the package is not in our repository

library(cluster)
library(useful)
library(Hmisc)
library(HSAUR)
library(MVA)
library(HSAUR2)
library(fpc)
library(mclust)
library(lattice)
library(latticeExtra)
library(car)
library(stats)
library(plyr)
library(ggplot2)
library(NbClust)
library(factoextra)
library(reshape) 
library(scales) 
library(grid) 
library(gridExtra)
library(plot3D)
library(mice)

numdata <- data.num.frame
testingdf <- data.num.frame #for playing with new variables
write.csv(numdata, file = "solo1data.csv")
dev.off()

##################################################################
### EDA ###
##################################################################
str(numdata)
head(numdata)
tail(numdata)
summary(numdata)
describe(numdata)
names(numdata)

#1
barplot(table(numdata$q1))

#reclassify: 1-3, 4-5, 6-7, 8-9, 10-11, 12-13, 14
testingdf$q1[testingdf$q1==2] <- 1

testingdf$q1[testingdf$q1==3] <- 2
testingdf$q1[testingdf$q1==4] <- 2

testingdf$q1[testingdf$q1==5] <- 3
testingdf$q1[testingdf$q1==6] <- 3

testingdf$q1[testingdf$q1==8] <- 4
testingdf$q1[testingdf$q1==7] <- 4

testingdf$q1[testingdf$q1==10] <- 5
testingdf$q1[testingdf$q1==9] <- 5
testingdf$q1[testingdf$q1==11] <- 5

#2
one <- sum(numdata$q2r1)
two <- sum(numdata$q2r2)
three <- sum(numdata$q2r3)
four <- sum(numdata$q2r4)
five <- sum(numdata$q2r5)
six <- sum(numdata$q2r6)
seven <- sum(numdata$q2r7)
eight <- sum(numdata$q2r8)
nine <- sum(numdata$q2r9)
quest2 <- rbind(one, two, three, four, five, six, seven, eight, nine)
barchart(quest2)

#combine r 5,7,9
numdata$q2r9 <- numdata$q2r5+numdata$q2r7+numdata$q2r9
numdata$q2r9[numdata$q2r9>=1] <- 1
summary(numdata$q2r9)
numdata$q2r9

#Drop r5 and r7, combine under r9
drop <- c("q2r5", "q2r7")
numdata <- numdata[,!(names(numdata) %in% drop)]
summary(numdata)

#iPhone and Android
testingdf$q2r1 <- numdata$q2r1+numdata$q2r3
testingdf$q2r1[testingdf$q2r1>=1] <- 1

#4
one <- sum(numdata$q4r1)
two <- sum(numdata$q4r2)
three <- sum(numdata$q4r3)
four <- sum(numdata$q4r4)
five <- sum(numdata$q4r5)
six <- sum(numdata$q4r6)
seven <- sum(numdata$q4r7)
eight <- sum(numdata$q4r8)
nine <- sum(numdata$q4r9)
ten <- sum(numdata$q4r10)
eleven <- sum(numdata$q4r11)
quest4 <- rbind(one, two, three, four, five, six, seven, eight, nine, ten, eleven)
barchart(quest4)

#combine r 2 and 4, show apps
numdata$q4r2 <- numdata$q4r2+numdata$q4r4
numdata$q4r2[numdata$q4r2>=1] <- 1
summary(numdata$q4r2)
numdata$q4r4

#Drop r2 and combine under r4
drop <- c("q4r4")
numdata <- numdata[,!(names(numdata) %in% drop)]

#11
barplot(table(numdata$q11))

#12
barplot(table(numdata$q12))
table(numdata$q12)

#13
one <- mean(numdata$q13r1)
two <- mean(numdata$q13r2)
three <- mean(numdata$q13r3)
four <- mean(numdata$q13r4)
five <- mean(numdata$q13r5)
six <- mean(numdata$q13r6)
seven <- mean(numdata$q13r7)
eight <- mean(numdata$q13r8)
nine <- mean(numdata$q13r9)
ten <- mean(numdata$q13r10)
eleven <- mean(numdata$q13r11)
twelve <- mean(numdata$q13r12)
quest13 <- rbind(one, two, three, four, five, six, seven, eight, nine, ten, eleven, twelve)
barchart(quest13)

#24
one <- mean(numdata$q24r1)
two <- mean(numdata$q24r2)
three <- mean(numdata$q24r3)
four <- mean(numdata$q24r4)
five <- mean(numdata$q24r5)
six <- mean(numdata$q24r6)
seven <- mean(numdata$q24r7)
eight <- mean(numdata$q24r8)
nine <- mean(numdata$q24r9)
ten <- mean(numdata$q24r10)
eleven <- mean(numdata$q24r11)
twelve <- mean(numdata$q24r12)
quest24 <- rbind(one, two, three, four, five, six, seven, eight, nine, ten, eleven, twelve)
barchart(quest24)

#25
one <- mean(numdata$q25r1)
two <- mean(numdata$q25r2)
three <- mean(numdata$q25r3)
four <- mean(numdata$q25r4)
five <- mean(numdata$q25r5)
six <- mean(numdata$q25r6)
seven <- mean(numdata$q25r7)
eight <- mean(numdata$q25r8)
nine <- mean(numdata$q25r9)
ten <- mean(numdata$q25r10)
eleven <- mean(numdata$q25r11)
twelve <- mean(numdata$q25r12)
quest25 <- rbind(one, two, three, four, five, six, seven, eight, nine, ten, eleven, twelve)
barchart(quest25)

#26
three <- mean(numdata$q26r3)
four <- mean(numdata$q26r4)
five <- mean(numdata$q26r5)
six <- mean(numdata$q26r6)
seven <- mean(numdata$q26r7)
eight <- mean(numdata$q26r8)
nine <- mean(numdata$q26r9)
ten <- mean(numdata$q26r10)
eleven <- mean(numdata$q26r11)
twelve <- mean(numdata$q26r12)
thirt <- mean(numdata$q26r13)
fourt <- mean(numdata$q26r14)
fift <- mean(numdata$q26r15)
sixt <- mean(numdata$q26r16)
sevent <- mean(numdata$q26r17)
quest26 <- rbind(three, four, five, six, seven, eight, nine, ten, eleven, twelve, thirt, fourt, fift, sixt, sevent)
barchart(quest26)

#48
barplot(table(numdata$q48))
barplot(table(numdata$q48, numdata$q1))

#49
barplot(table(numdata$q49))

#50
one <- sum(numdata$q50r1)
two <- sum(numdata$q50r2)
three <- sum(numdata$q50r3)
four <- sum(numdata$q50r4)
five <- sum(numdata$q50r5)
quest50 <- rbind(one, two, three, four, five)
barchart(quest50)

#54
barplot(table(numdata$q54))

#combine r 2 and 4, TV show apps
numdata$q54[numdata$q54==5] <- 4
numdata$q54[numdata$q54==6] <- 4
summary(numdata$q54)

#55
barplot(table(numdata$q55))
table(numdata$q55, numdata$q54)

#56
barplot(table(numdata$q56))

#reclassify: 1-3, 4-5, 6-7, 8-9, 10-11, 12-13, 14
numdata$q56[numdata$q56==2] <- 1
numdata$q56[numdata$q56==3] <- 1

numdata$q56[numdata$q56==4] <- 2
numdata$q56[numdata$q56==5] <- 2

numdata$q56[numdata$q56==6] <- 3
numdata$q56[numdata$q56==7] <- 3

numdata$q56[numdata$q56==8] <- 4
numdata$q56[numdata$q56==9] <- 4

numdata$q56[numdata$q56==10] <- 5
numdata$q56[numdata$q56==11] <- 5

numdata$q56[numdata$q56==12] <- 6
numdata$q56[numdata$q56==13] <- 6

numdata$q56[numdata$q56==14] <- 7

summary(numdata$q56)
barplot(table(numdata$q56))

#57
barplot(table(numdata$q57))

boxplot(numdata$q57, numdata$q1, horizontal=TRUE,las=1)

# Age by Gender       
require(lattice)
histogram(~numdata$q1 | numdata$q57, data=numdata)

#  # of apps by Gender       
require(lattice)
histogram(~numdata$q11 | numdata$q57, data=numdata)
histogram(~numdata$q11 | numdata$q57, data=numdata, type="count")

prop.table(table(numdata$q57, numdata$q1), margin=2)


barchart(prop.table(table(numdata$q57, numdata$q1)),
         xlab="Subscriber proportion by Segment", col="green")

a <- table(numdata$q1)
a
barplot(a)

#initial walkthrough of data
table(numdata$q4r11) 
table(numdata$q1, numdata$q4r11) 
table(numdata$q1, numdata$q4r6) 
table(numdata$q1, numdata$q12) 

table(numdata$q24r2)
table(numdata$q24r2, numdata$q25r1)  

table(numdata$q24r6)
table(numdata$q24r6, numdata$q25r12) 

table(numdata$q24r5)
table(numdata$q24r5, numdata$q25r12) 

b <- table(numdata$q1, numdata$q2r1) 
b
barplot(b)

free <- table(numdata$q11, numdata$q12) 

free
barplot(free)

old <- table(numdata$q1r12, numdata$q4) 
old
barplot(old)

table(numdata$q12) 

library(plyr) 
temp <- count(numdata, c('numdata$q1','numdata$q2r1')) 
str(temp)

############################################################################################################
##DATA PREPARATION
############################################################################################################
## MICE for Imputation ##
drop <- c("q5r1")
numdata <- numdata[,!(names(numdata) %in% drop)]
summary(numdata)

imp <- mice(numdata, m=5, method='rf')
df.complete <- complete(imp)
summary(df.complete)
summary(numdata)

#MICE Methods Tests
imp <- mice(numdata, m=5, method='rf')
densityplot(imp, ~q57, main = "Density Plot of Random Forest")

imp <- mice(numdata, m=5, method='sample')
densityplot(imp, ~q57, main = "Density Plot of Random Sampling")

imp <- mice(numdata, m=5, method='cart', main = "Density Plot of CART")
densityplot(imp, ~q57, main = "Density Plot of CART")

imp <- mice(numdata, m=5, method='lda', main = "Density Plot of CART")
densityplot(imp, ~q57, main = "Density Plot of LDA")

summary(numdata$q57)
summary(df.complete$q57)


#Input Imputed Variables Separately
numdata$q57 <- df.complete$q57

#####################################################################
### Creating subsets ###
#####################################################################

#raw version
numsubr <- subset(numdata, select=
                    c("q24r1","q24r2","q24r3","q24r4","q24r5","q24r6","q24r7","q24r8","q24r9",
                      "q24r10","q24r11","q24r12",
                      "q25r1","q25r2","q25r3","q25r4","q25r5","q25r6","q25r7","q25r8","q25r9",
                      "q25r10","q25r11","q25r12",
                      "q26r3","q26r4","q26r5","q26r6","q26r7","q26r8","q26r9","q26r10","q26r11",
                      "q26r12","q26r13","q26r14","q26r15","q26r16","q26r17","q26r18"))

# use this for the setup/analysis
numsub <- subset(numdata, select=
                   c("q24r1","q24r2","q24r3","q24r4","q24r5","q24r6","q24r7","q24r8","q24r9",
                     "q24r10","q24r11","q24r12",
                     "q25r1","q25r2","q25r3","q25r4","q25r5","q25r6","q25r7","q25r8","q25r9",
                     "q25r10","q25r11","q25r12",
                     "q26r3","q26r4","q26r5","q26r6","q26r7","q26r8","q26r9","q26r10","q26r11",
                     "q26r12","q26r13","q26r14","q26r15","q26r16","q26r17","q26r18"))

str(numsub)
summary(numsub)

#####################################################################
### correlation plot ###
#####################################################################
require(corrplot)
mcor <- cor(numsub)
corrplot(mcor, method="shade", shade.col=NA, tl.col="black",tl.cex=0.5)
summary(numsub)

# Ladder Corrplot
corrplot(numsubcorrelation, method="shade", type="lower", shade.col=NA, tl.col="black", 
tl.srt=45, order="AOE")

#####################################################################
### check for peaks & valleys (ie) natural clusters ###
#####################################################################

##  Create cuts:
q24r1_c <- cut(q24r1, 6)
q24r2_c <- cut(q24r2, 6)

##  Calculate joint counts at cut levels:
z <- table(q24r1_c, q24r2_c)
z

##  Plot as a 3D histogram:
hist3D(z=z, border="black")

##  Plot as a 2D heatmap:

image2D(z=z, border="black")

library(latticeExtra)

cloud(z~q24r1_c+q24r2_c, numsub, panel.3d.cloud=panel.3dbars, col.facet='blue', 
      xbase=0.4, ybase=0.4, scales=list(arrows=FALSE, col=1), 
      par.settings = list(axis.line = list(col = "transparent")))

#####################################################################
### PCA Plots ###
#####################################################################
dev.off()
pca <-princomp(numsub)
plot(pca$scores[,1],pca$scores[,2])

names(pca)
str(pca)
summary(pca)
head(pca$scores)

sort(pca$scores[,1])
sort(pca$scores[,1], decreasing = TRUE)
numsub["431",]
numsub["2176",]
numsub["1083",]
numsub["230",]

sort(pca$scores[,1], decreasing = FALSE)
numsub["1946",]
numsub["243",]


##  Create cuts:
pcadf <- as.data.frame(pca$scores)
pca1 <- cut(pcadf$Comp.1, 10)
pca2 <- cut(pcadf$Comp.2, 10)

##  Calculate joint counts at cut levels:
z <- table(pca1, pca2)

##  Plot as a 3D histogram:
hist3D(z=z, border="black")

#####################################################################
### Create a 'scree' plot to determine the num of clusters ###
#####################################################################

dev.off()
wssplot <- function(numsub, nc=10, seed=1234) {
  wss <- (nrow(numsub)-1)*sum(apply(numsub,2,var))
  for (i in 2:nc) {
    set.seed(seed)
    wss[i] <- sum(kmeans(numsub, centers=i)$withinss)}
  plot(1:nc, wss, type="b", xlab="Number of Clusters",
       ylab="Within groups sum of squares")} 

wssplot(numsub)

# this doesn't tell us much about the optimal #. There are no elbows. Let's try a different method:
cluster.model1 <- NbClust(numsub, 
                  min.nc=2, max.nc=8, distance='euclidean', 
                  method='kmeans', index='all')
table(cluster.model1$Best.n[1,])

plot1 <- fviz_nbclust(cluster.model1) + 
  ggtitle('K-means + Euclidean Distance')

#####################################################################
###  k means with raw data with 3 clusters ###
#####################################################################

clusterresults <- kmeans(numsub,3)
clusterresults$size
rsquare <- clusterresults$betweenss/clusterresults$totss
rsquare

clusterresults$centers # each row is a centroid of a cluster

plot(clusterresults, data=numsub)

dissE <- daisy(numsub)

dE2   <- dissE^2
sk2   <- silhouette(clusterresults$cluster, dE2)

plot(sk2, border=NA)

#####################################################################
###  k means with raw data with 4 clusters ###
#####################################################################

clusterresults <- kmeans(numsub,4)
clusterresults$size
rsquare <- clusterresults$betweenss/clusterresults$totss
rsquare

clusterresults$centers # each row is a centroid of a cluster

plot(clusterresults, data=numsub)

dissE <- daisy(numsub)

dE2   <- dissE^2
sk2   <- silhouette(clusterresults$cluster, dE2)

plot(sk2, border=NA)

#####################################################################
###  k means with raw data with 5 clusters ###
#####################################################################

clusterresults <- kmeans(numsub,5)
clusterresults$size
rsquare <- clusterresults$betweenss/clusterresults$totss
rsquare

clusterresults$centers # each row is a centroid of a cluster

plot(clusterresults, data=numsub)

dissE <- daisy(numsub)

dE2   <- dissE^2
sk2   <- silhouette(clusterresults$cluster, dE2)

plot(sk2, border=NA)

############################################################
### Clustering for Likert scale - 'ordinal' data
### 40 variables with 6 values (ie) 240 binary columns
### For each pair of people, compute % match on 240 columns
### Similarity metric is this % & do clustering

#####################################################################
### create 'derived' variables - means of similar variables ###
#####################################################################

attach(numsub)
numsub$q24a <- (q24r1+q24r2+q24r3+q24r5+q24r6)/5
numsub$q24b <- (q24r7+q24r8)/2
numsub$q24c <- (q24r10+q24r11)/2
numsub$q24d <- (q24r4+q24r9+q24r12)/3

numsub$q25a <- (q25r1+q25r2+q25r3+q25r4+q25r5)/5
numsub$q25b <- (q25r7+q25r8)/2
numsub$q25c <- (q25r9+q25r10+q25r11)/3
numsub$q25d <- (q25r6+q25r12)/2

numsub$q26a <- (q26r3+q26r4+q26r5+q26r6+q26r7)/5
numsub$q26b <- (q26r8+q26r9+q26r10)/3
numsub$q26c <- q26r11
numsub$q26d <- (q26r12+q26r13+q26r14)/3
numsub$q26e <- (q26r15+q26r16+q26r17+q26r18)/4

numsubX <- subset(numsub, select=
                    c("q24a","q24b","q24c","q24d",
                      "q25a","q25b","q25c","q25d",
                      "q26a","q26b","q26d","q26e"))

pca <-princomp(numsubX)
plot(pca$scores[,1],pca$scores[,2])
names(pca)
head(pca$scores)
str(pca$scores)
summary(pca)

pcadf <- as.data.frame(pca$scores)
pca1 <- cut(pcadf$Comp.1, 10)
pca2 <- cut(pcadf$Comp.2, 10)

##  Calculate joint counts at cut levels:
z <- table(pca1, pca2)

##  Plot as a 3D histogram:
hist3D(z=z, border="black")

numsub$control <- (q24r5+q25r7+q25r4)/3
numsub$time <- (q24r6+q25r12+q25r11)/2
numsub$new <- (q24r1+q24r3+q25r5)/3

numsub$q24b <- (q24r7+q24r8)/2
numsub$q24c <- (q24r10+q24r11)/2
numsub$q24d <- (q24r4+q24r9+q24r12)/3

numsub$advice <- (q25r1+q25r3+q24r2)/3
numsub$q25a <- q25r2
numsub$q25b <- q25r8
numsub$q25c <- (q25r9+q25r10)/2
numsub$q25d <- q25r6

numsub$q26a <- (q26r3+q26r4+q26r5+q26r6+q26r7)/5
numsub$q26b <- (q26r8+q26r9+q26r10)/3
numsub$q26c <- q26r11
numsub$q26d <- (q26r12+q26r13+q26r14)/3
numsub$q26e <- (q26r15+q26r16+q26r17+q26r18)/4

numsubtest <- subset(numsub, select=
                    c("control","time","new","q24b","q24c",
                      "advice","q25a","q25b","q25c",
                      "q26a","q26b","q26d","q26e"))

pca <-princomp(numsubtest)
plot(pca$scores[,1],pca$scores[,2])
names(pca)
head(pca$scores)
str(pca$scores)
summary(pca)

pcadf <- as.data.frame(pca$scores)
pca1 <- cut(pcadf$Comp.1, 10)
pca2 <- cut(pcadf$Comp.2, 10)

##  Calculate joint counts at cut levels:
z <- table(pca1, pca2)

##  Plot as a 3D histogram:
hist3D(z=z, border="black")

require(corrplot)
mcor <- cor(numsubX)
corrplot(mcor, method="shade", shade.col=NA, tl.col="black",tl.cex=0.5)

numsub$q24a <- (q24r1+q24r2+q24r3+q24r5+q24r6)/5
numsub$q24b <- (q24r7+q24r8)/2
numsub$q24c <- (q24r10+q24r11)/2
numsub$q24d <- (q24r4+q24r9+q24r12)/3

numsub$q25a <- (q25r1+q25r2+q25r3+q25r4+q25r5)/5
numsub$q25b <- (q25r7+q25r8)/2
numsub$q25c <- (q25r9+q25r10+q25r11)/3
numsub$q25d <- (q25r6+q25r12)/2

numsub$q26a <- (q26r3+q26r4+q26r5+q26r6+q26r7)/5
numsub$q26b <- (q26r8+q26r9+q26r10)/3
numsub$q26c <- q26r11
numsub$q26d <- (q26r12+q26r13+q26r14)/3
numsub$q26e <- (q26r15+q26r16+q26r17+q26r18)/4

numsub2 <- subset(numsub, select=
                    c("q24a","q24b","q24c",
                      "q25a","q25b",
                      "q26a","q26b","q26d","q26e"))

pca <-princomp(numsub2)
plot(pca$scores[,1],pca$scores[,2])
names(pca)
head(pca$scores)
str(pca$scores)
summary(pca)

pcadf <- as.data.frame(pca$scores)
pca1 <- cut(pcadf$Comp.1, 10)
pca2 <- cut(pcadf$Comp.2, 10)

##  Calculate joint counts at cut levels:
z <- table(pca1, pca2)

##  Plot as a 3D histogram:
hist3D(z=z, border="black")

require(corrplot)
mcor <- cor(numsub2)
corrplot(mcor, method="shade", shade.col=NA, tl.col="black",tl.cex=0.5)

#####################################################################
### Create a 'scree' plot to determine the num of clusters ###
#####################################################################

dev.off()
wssplot <- function(numsub2, nc=10, seed=1234) {
  wss <- (nrow(numsub2)-1)*sum(apply(numsub2,2,var))
  for (i in 2:nc) {
    set.seed(seed)
    wss[i] <- sum(kmeans(numsub2, centers=i)$withinss)}
  plot(1:nc, wss, type="b", xlab="Number of Clusters",
       ylab="Within groups sum of squares")} 

wssplot(numsub2)

# this doesn't tell us much about the optimal #. There are no elbows. Let's try a different method:
cluster.model1 <- NbClust(numsub2, 
                          min.nc=3, max.nc=6, distance='euclidean', 
                          method='kmeans', index='all')
table(cluster.model1$Best.n[1,])

plot1 <- fviz_nbclust(cluster.model1) + 
  ggtitle('K-means - Euclidean Distance')

#####################################################################
### Create a Kmeans with 3 clusters with derived data ###
#####################################################################
clusterresults <- kmeans(numsub2,3)
names(clusterresults)
clusterresults$centers
rsquare <- clusterresults$betweenss/clusterresults$totss
rsquare

plot(clusterresults, data=numsub2)

dissE <- daisy(numsub2)
names(dissE)
dE2   <- dissE^2
sk2   <- silhouette(clusterresults$cluster, dE2)
str(sk2)
plot(sk2, border=NA)

write.csv(clusterresults, file = "cut3results.csv")

#####################################################################
### Create a Kmeans with 4 clusters with derived data ###
#####################################################################
clusterresults <- kmeans(numsub2,4)
names(clusterresults)
clusterresults$centers
rsquare <- clusterresults$betweenss/clusterresults$totss
rsquare

plot(clusterresults, data=numsub2)

dissE <- daisy(numsub2)
names(dissE)
dE2   <- dissE^2
sk2   <- silhouette(clusterresults$cluster, dE2)
str(sk2)
plot(sk2, border=NA)

write.csv(clusterresults, file = "cut5results.csv")

##### another way to do the same thing ################

newdf <- as.data.frame(clusterresults$cluster)
pcadf <- as.data.frame(pca$scores)

write.csv(newdf, file = "clusterresults.csv")
write.csv(pcadf, file = "pca.csv")
combdata <- cbind(newdf,pcadf)
head(combdata)

xyplot(Comp.2 ~ Comp.1, combdata, groups = clusterresults$cluster, pch= 20)

write.csv(numsub, file = "numsub.csv")

#####################################################################
### Create a Kmeans with 5 clusters with derived data ###
#####################################################################
clusterresults <- kmeans(numsub2,5)
names(clusterresults)
clusterresults$centers
rsquare <- clusterresults$betweenss/clusterresults$totss
rsquare

plot(clusterresults, data=numsub2)

dissE <- daisy(numsub2)
names(dissE)
dE2   <- dissE^2
sk2   <- silhouette(clusterresults$cluster, dE2)
str(sk2)
plot(sk2, border=NA)

write.csv(clusterresults, file = "cut5results.csv")

#####################################################################
### PAM method ###
#####################################################################
clusterresultsPAM <-pam(numsub2,4)
summary(clusterresultsPAM)
str(clusterresultsPAM$silinfo)
plot(clusterresultsPAM, which.plots=1)
plot(clusterresultsPAM, which.plots=2, border=NA)

anova(aov(q24a~cluster, data=numsub2))
anova(aov(q24b~cluster, data=numsub2))
anova(aov(q24c~cluster, data=numsub2))
anova(aov(q25a~cluster, data=numsub2))
anova(aov(q25b~cluster, data=numsub2))
anova(aov(q26a~cluster, data=numsub2))
anova(aov(q26b~cluster, data=numsub2))
anova(aov(q26d~cluster, data=numsub2))
anova(aov(q26e~cluster, data=numsub2))

#####################################################################
### Create a dataset with the original data with the cluster info ###
### This will be useful for creating profiles for the clusters ###
#####################################################################

newdf <- read.csv("clusterresults.csv")
combdata <- cbind(numsub2,newdf,numdata$q1)
head(combdata)

require(reshape)
combdata <- plyr::rename(combdata, c(clusterresults.cluster="cluster"))
head(combdata)

aggregate(combdata,by=list(byvar=combdata$cluster), mean)

## Done with K Means, do the profile  ###
#####################################################################
### Hierarchical Clustering with derived data ###
#####################################################################
numsub.dist <- dist(numsub2)
require(maptree)
hclustmodel <- hclust(dist(numsub2), method = 'ward.D2')
names(hclustmodel)
plot(hclustmodel)

# make it pretty
op = par(bg = "#DDE3CA")
plot(hclustmodel, col = "#487AA1", col.main = "#45ADA8", col.lab = "#7C8071", 
     col.axis = "#F38630", lwd = 3, lty = 3, sub = "", hang = -1, axes = FALSE)
# add axis
axis(side = 2, at = seq(0, 400, 100), col = "#F38630", labels = FALSE, 
     lwd = 2)

cut.4 <- cutree(hclustmodel, k=4)
plot(silhouette(cut.4,numsub.dist), border=NA)
head(cut.4)

#####################################################################
### for hclust how to calculate BSS & TSS ###
#####################################################################
require(proxy)

# Derived
numsubmat <- as.matrix(numsub2)
overallmean <- matrix(apply(numsubmat,2,mean),nrow=1)
overallmean
TSS <- sum(dist(numsubmat,overallmean)^2)
TSS

#####################################################################
### Compute WSS based on 4 clusters ###
#####################################################################
combcutdata <- cbind(numsub2,cut.4)
head(combcutdata)

require(reshape)
combcutdata <- plyr::rename(combcutdata, c(cut.4="cluster"))
head(combcutdata)

clust1 <- subset(combcutdata, cluster == 1)
clust1 <- subset(clust1, select=c("q24a","q24b","q24c","q25a","q25b",
                                  "q26a","q26b","q26d","q26e"))
clust1 <- as.matrix(clust1,rowby=T)
dim(clust1)
clust1mean <- matrix(apply(clust1,2,mean),nrow=1)
dim(clust1mean)
dis1 <- sum(dist(clust1mean,clust1)^2)

clust2 <- subset(combcutdata, cluster == 2)
clust2 <- subset(clust2, select=c("q24a","q24b","q24c","q25a","q25b",
                                  "q26a","q26b","q26d","q26e"))
clust2 <- as.matrix(clust2,rowby=T)
clust2mean <- matrix(apply(clust2,2,mean),nrow=1)
dis2 <- sum(dist(clust2mean,clust2)^2)

clust3 <- subset(combcutdata, cluster == 3)
clust3 <- subset(clust3, select=c("q24a","q24b","q24c","q25a","q25b",
                                  "q26a","q26b","q26d","q26e"))
clust3 <- as.matrix(clust3,rowby=T)
clust3mean <- matrix(apply(clust3,2,mean),nrow=1)
dis3 <- sum(dist(clust3mean,clust3)^2)

clust4 <- subset(combcutdata, cluster == 4)
clust4 <- subset(clust4, select=c("q24a","q24b","q24c","q25a","q25b",
                                  "q26a","q26b","q26d","q26e"))
clust4 <- as.matrix(clust4,rowby=T)
clust4mean <- matrix(apply(clust4,2,mean),nrow=1)
dis4 <- sum(dist(clust4mean,clust4)^2)

WSS <- sum(dis1,dis2,dis3,dis4)
WSS

BSS <- TSS - WSS
BSS

## calculating the % of Between SS/ Total SS
rsquare <- BSS/TSS
rsquare

#####################################################################
### Hierarchical Clustering ###
#####################################################################

# Method: ward.D, Distance: euclidean
hier1 <- NbClust(numsub2, diss=NULL,
                  min.nc=2, max.nc=6, distance='euclidean',
                  method='ward.D2', index='all')
table(hier1$Best.n[1,])

# Method: ward.D, Distance: manhattan
hier2 <- NbClust(numsub2, diss=NULL,
                   min.nc=2, max.nc=8, distance='manhattan',
                   method='ward.D2', index='all')
table(hier2$Best.n[1,])

# Method: ward.D, Distance: euclidean
set.seed(1)
hiercut1 <- hcut(numsub2, k=4, isdiss=FALSE, 
                  hc_func='hclust', hc_metric='euclidean',
                  hc_method='ward.D2', stand=FALSE)

plot1a <- fviz_silhouette(hiercut1)

plot1b <- fviz_cluster(hiercut1, numsub2, 
                       show.clust.cent=TRUE, geom='point',
                       main='Clusters: Ward & Euclidean Distance')

# Method: ward.D, Distance: manhattan
set.seed(1)
dist.daisy <- daisy(numsub2, metric='manhattan', stand=FALSE)
hiercut2 <- hcut(dist.daisy, k=4, isdiss=TRUE,
                  hc_func='hclust', hc_metric='manhattan',
                  hc_method='ward.D2', stand=FALSE)

plot2a <- fviz_silhouette(hiercut2)

plot2b <- fviz_cluster(hiercut2, numsub2, 
                       show.clust.cent=TRUE, geom='point',
                       main='Clusters: Ward & Manhattan Distance')

fviz_dend(hiercut1, k=4, 
          cex=0.5, show_labels=FALSE, rect=TRUE,
          main='Dendrogram: Ward & Euclidean Distance')

fviz_dend(hiercut2, k=4,
          cex=0.5, show_labels=FALSE, rect=TRUE,
          main='Dendrogram: Ward & Manhattan Distance')

#####################################################################
### Model based clustering ###
#####################################################################
library(mclust)
fit <- Mclust(numsub2,4)
plot(fit,data=numsub2, what="density") # plot results
plot(fit,data=numsub2, what="BIC") # plot results

summary(fit) # display the best model

dev.off()
dissE <- daisy(numsub2)
names(dissE)
dE2   <- dissE^2
sk2   <- silhouette(fit$classification, dE2)
str(sk2)
plot(sk2, border=NA)

#####################################################################
### Checking the models ###
#####################################################################
# K-Means
glm(q24a~cluster, data=numsub2)$aic
glm(q24b~cluster, data=numsub2)$aic
glm(q24c~cluster, data=numsub2)$aic
glm(q25a~cluster, data=numsub2)$aic
glm(q25b~cluster, data=numsub2)$aic
glm(q26a~cluster, data=numsub2)$aic
glm(q26b~cluster, data=numsub2)$aic
glm(q26d~cluster, data=numsub2)$aic
glm(q26e~cluster, data=numsub2)$aic

# PAM
glm(q24a~clusterresultsPAM$cluster, data=numsub2)$aic
glm(q24b~clusterresultsPAM$cluster, data=numsub2)$aic
glm(q24c~clusterresultsPAM$cluster, data=numsub2)$aic
glm(q25a~clusterresultsPAM$cluster, data=numsub2)$aic
glm(q25b~clusterresultsPAM$cluster, data=numsub2)$aic
glm(q26a~clusterresultsPAM$cluster, data=numsub2)$aic
glm(q26b~clusterresultsPAM$cluster, data=numsub2)$aic
glm(q26d~clusterresultsPAM$cluster, data=numsub2)$aic
glm(q26e~clusterresultsPAM$cluster, data=numsub2)$aic

# HCut 4
glm(q24a~cut.4, data=numsub2)$aic
glm(q24b~cut.4, data=numsub2)$aic
glm(q24c~cut.4, data=numsub2)$aic
glm(q25a~cut.4, data=numsub2)$aic
glm(q25b~cut.4, data=numsub2)$aic
glm(q26a~cut.4, data=numsub2)$aic
glm(q26b~cut.4, data=numsub2)$aic
glm(q26d~cut.4, data=numsub2)$aic
glm(q26e~cut.4, data=numsub2)$aic

#####################################################################
### A little function to calculate the average silhouette width ###
### for a variety of choices of k: ###
#####################################################################
my.k.choices <- 2:6
avg.sil.width <- rep(0, times=length(my.k.choices))
for (ii in (1:length(my.k.choices)) ){
  avg.sil.width[ii] <- pam(numsub2, k=my.k.choices[ii])$silinfo$avg.width
}
print( cbind(my.k.choices, avg.sil.width) )


#####################################################################
### comparison of cluster results  ###
#####################################################################
clstat <- cluster.stats(numsub.dist, clusterresults$cluster, clusterresultsPAM$cluster)
names(clstat)
clstat$corrected.rand #the % of the pairs that show up in the same clusters

##corrected or adjusted rand index lies between 0 & 1
## perfect match between 2 clustering methods means 1, no match means 0
## any number in between represents 'kind of' % of matched pairs 

clstat <- cluster.stats(numsub.dist, clusterresults$cluster, cut.3) # take hierarchical cluster with kmeans
clstat$corrected.rand

#####################################################################
### Run the model, combine the data for profiling  ###
#####################################################################
clusterresults <- kmeans(numsub2,4)
pca <-princomp(numsub2)

newdf <- as.data.frame(clusterresults$cluster)
pcadf <- as.data.frame(pca$scores)

write.csv(newdf, file = "clusterresults.csv")
write.csv(pcadf, file = "pca.csv")

combdata <- cbind(newdf,pcadf)
head(combdata)

################################################################
### Create a dataset with the original data with the cluster info
### This will be useful for creating profiles for the clusters
###############################################################

newdf <- read.csv("clusterresults.csv")
combdata <- cbind(numsub2,newdf,numdata$q1,numdata$q57)
head(combdata)

require(reshape)
combdata <- rename(combdata, c(clusterresults.cluster="cluster"))
head(combdata)

aggregate(combdata,by=list(byvar=combdata$cluster), mean)

df_means <- ddply(numdata, 
                  .(numdata$cluster), colwise(mean))
df_median <- ddply(numdata, 
                  .(numdata$cluster), colwise(median))

################################################################
### PROFILING: Analyze Each Question per Cluster ###
###############################################################

# Q1
par(mfrow=c(1,2))
par(oma=c(0,0,2,0))
plotq1 <- table(clusterresults$cluster, numdata$q1)
barplot(plotq1, beside = TRUE, col=c("grey","cyan","light green","dark blue"), xlab="Age Category")

plotq1a <- prop.table(table(numdata$q1, clusterresults$cluster), margin=2)
barplot(plotq1a, col=brewer.pal(n = 11, name = "RdBu"), xlab="Cluster")
title("Which of the following best describes your age?", outer=TRUE)

# Q11
plotq11 <- table(clusterresults$cluster, numdata$q11)
barplot(plotq11, beside = TRUE, col=c("grey","cyan","light green","dark blue"))

plotq11a <- prop.table(table(numdata$q1, clusterresults$cluster), margin=2)
barplot(plotq11a, col=brewer.pal(n = 6, name = "RdBu"))

# Q12
plotq12 <- table(clusterresults$cluster, numdata$q12)
barplot(plotq12, beside = TRUE, col=c("grey","cyan","light green","dark blue"))

plotq12a <- prop.table(table(numdata$q12, clusterresults$cluster), margin=2)
barplot(plotq12a, col=brewer.pal(n = 6, name = "RdBu"))

# Q48
plotq48 <- table(clusterresults$cluster, numdata$q48)
barplot(plotq48, beside = TRUE, col=c("grey","cyan","light green","dark blue"))

plotq48a <- prop.table(table(numdata$q48, clusterresults$cluster), margin=2)
barplot(plotq48a, col=brewer.pal(n = 6, name = "RdBu"))

# Q49
plotq49 <- table(clusterresults$cluster, numdata$q49)
barplot(plotq49, beside = TRUE, col=c("grey","cyan","light green","dark blue"))

plotq49a <- prop.table(table(numdata$q49, clusterresults$cluster), margin=2)
barplot(plotq49a, col=brewer.pal(n = 6, name = "RdBu"))

# Q54
plotq54 <- table(clusterresults$cluster, numdata$q54)
barplot(plotq54, beside = TRUE, col=c("grey","cyan","light green","dark blue"))

plotq54a <- prop.table(table(numdata$q54, clusterresults$cluster), margin=2)
barplot(plotq54a, col=brewer.pal(n = 6, name = "RdBu"))

# Q55
plotq55 <- table(clusterresults$cluster, numdata$q55)
barplot(plotq55, beside = TRUE, col=c("grey","cyan","light green","dark blue"))

plotq55a <- prop.table(table(numdata$q55, clusterresults$cluster), margin=2)
barplot(plotq55a, col=brewer.pal(n = 6, name = "RdBu"))

# Q56
plotq56 <- table(clusterresults$cluster, numdata$q56)
barplot(plotq56, beside = TRUE, col=c("grey","cyan","light green","dark blue"))

plotq56a <- prop.table(table(numdata$q56, clusterresults$cluster), margin=2)
barplot(plotq56a, col=brewer.pal(n = 11, name = "RdBu"))

#reclassify: 1-3, 4-5, 6-7, 8-9, 10-11, 12-13, 14
numdata$q56[numdata$q56==2] <- 1
numdata$q56[numdata$q56==3] <- 1

numdata$q56[numdata$q56==4] <- 2
numdata$q56[numdata$q56==5] <- 2

numdata$q56[numdata$q56==6] <- 3
numdata$q56[numdata$q56==7] <- 3

numdata$q56[numdata$q56==8] <- 4
numdata$q56[numdata$q56==9] <- 4

numdata$q56[numdata$q56==10] <- 5
numdata$q56[numdata$q56==11] <- 5

numdata$q56[numdata$q56==12] <- 6
numdata$q56[numdata$q56==13] <- 6

numdata$q56[numdata$q56==14] <- 7

# Q57
plotq57 <- table(clusterresults$cluster, numdata$q57)
barplot(plotq57, beside = TRUE, col=c("grey","cyan","light green","dark blue"))

plotq57a <- prop.table(table(numdata$q57, clusterresults$cluster), margin=2)
barplot(plotq57a, col=brewer.pal(n = 6, name = "RdBu"))

# Q2r1
plotq2r1 <- table(clusterresults$cluster, numdata$q2r1)
barplot(plotq2r1, beside = TRUE, col=c("grey","cyan","light green","dark blue"))

plotq2r1a <- prop.table(table(numdata$q2r1, clusterresults$cluster), margin=2)
barplot(plotq2r1a, col=brewer.pal(n = 6, name = "RdBu"))

# TESTING Q2r1: iPhone OR Android
plotq2r1 <- table(clusterresults$cluster, testingdf$q2r1)
barplot(plotq2r1, beside = TRUE, col=c("grey","cyan","light green","dark blue"), xlab="No, Yes")

plotq2r1a <- prop.table(table(testingdf$q2r1, clusterresults$cluster), margin=2)
barplot(plotq2r1a, col=brewer.pal(n = 6, name = "RdBu"), xlab="Cluster")
title("Do You Own an iPhone OR Android?", outer=TRUE)

# Q2r2
plotq2r2 <- table(clusterresults$cluster, numdata$q2r2)
barplot(plotq2r2, beside = TRUE, col=c("grey","cyan","light green","dark blue"))

plotq2r2a <- prop.table(table(numdata$q2r2, clusterresults$cluster), margin=2)
barplot(plotq2r2a, col=brewer.pal(n = 6, name = "RdBu"))

# Q2r3
plotq2r3 <- table(clusterresults$cluster, numdata$q2r3)
barplot(plotq2r3, beside = TRUE, col=c("grey","cyan","light green","dark blue"))

plotq2r3a <- prop.table(table(numdata$q2r3, clusterresults$cluster), margin=2)
barplot(plotq2r3a, col=brewer.pal(n = 6, name = "RdBu"))

# Q2r4
plotq2r4 <- table(clusterresults$cluster, numdata$q2r4)
barplot(plotq2r4, beside = TRUE, col=c("grey","cyan","light green","dark blue"))

plotq2r4a <- prop.table(table(numdata$q2r4, clusterresults$cluster), margin=2)
barplot(plotq2r4a, col=brewer.pal(n = 6, name = "RdBu"))

# Q2r6
plotq2r6 <- table(clusterresults$cluster, numdata$q2r6)
barplot(plotq2r6, beside = TRUE, col=c("grey","cyan","light green","dark blue"))

plotq2r6a <- prop.table(table(numdata$q2r6, clusterresults$cluster), margin=2)
barplot(plotq2r6a, col=brewer.pal(n = 6, name = "RdBu"))

# Q2r8
plotq2r8 <- table(clusterresults$cluster, numdata$q2r8)
barplot(plotq2r8, beside = TRUE, col=c("grey","cyan","light green","dark blue"))

plotq2r8a <- prop.table(table(numdata$q2r8, clusterresults$cluster), margin=2)
barplot(plotq2r8a, col=brewer.pal(n = 6, name = "RdBu"))

# Q2r9
plotq2r9 <- table(clusterresults$cluster, numdata$q2r9)
barplot(plotq2r9, beside = TRUE, col=c("grey","cyan","light green","dark blue"))

plotq2r9a <- prop.table(table(numdata$q2r9, clusterresults$cluster), margin=2)
barplot(plotq2r9a, col=brewer.pal(n = 6, name = "RdBu"))

# Q4r1
plotq4r1 <- table(clusterresults$cluster, numdata$q4r1)
barplot(plotq4r1, beside = TRUE, col=c("grey","cyan","light green","dark blue"))

plotq4r1a <- prop.table(table(numdata$q4r1, clusterresults$cluster), margin=2)
barplot(plotq4r1a, col=brewer.pal(n = 6, name = "RdBu"))

# Q4r2
plotq4r2 <- table(clusterresults$cluster, numdata$q4r2)
barplot(plotq4r2, beside = TRUE, col=c("grey","cyan","light green","dark blue"), xlab="No, Yes")

plotq4r2a <- prop.table(table(numdata$q4r2, clusterresults$cluster), margin=2)
barplot(plotq4r2a, col=brewer.pal(n = 6, name = "RdBu"), xlab="Cluster")
title("Do You Use TV-related Apps?", outer=TRUE)

# Q4r3
plotq4r3 <- table(clusterresults$cluster, numdata$q4r3)
barplot(plotq4r3, beside = TRUE, col=c("grey","cyan","light green","dark blue"))

plotq4r3a <- prop.table(table(numdata$q4r3, clusterresults$cluster), margin=2)
barplot(plotq4r3a, col=brewer.pal(n = 6, name = "RdBu"))

# Q4r5
plotq4r5 <- table(clusterresults$cluster, numdata$q4r5)
barplot(plotq4r5, beside = TRUE, col=c("grey","cyan","light green","dark blue"), xlab="No, Yes")

plotq4r5a <- prop.table(table(numdata$q4r5, clusterresults$cluster), margin=2)
barplot(plotq4r5a, col=brewer.pal(n = 6, name = "RdBu"), xlab="Cluster")
title("Do You Use Gaming Apps?", outer=TRUE)

# Q4r6
plotq4r6 <- table(clusterresults$cluster, numdata$q4r6)
barplot(plotq4r6, beside = TRUE, col=c("grey","cyan","light green","dark blue"))

plotq4r6a <- prop.table(table(numdata$q4r6, clusterresults$cluster), margin=2)
barplot(plotq4r6a, col=brewer.pal(n = 6, name = "RdBu"))

# Q4r7
plotq4r7 <- table(clusterresults$cluster, numdata$q4r7)
barplot(plotq4r7, beside = TRUE, col=c("grey","cyan","light green","dark blue"))

plotq4r7a <- prop.table(table(numdata$q4r7, clusterresults$cluster), margin=2)
barplot(plotq4r7a, col=brewer.pal(n = 6, name = "RdBu"))

# Q4r8
plotq4r8 <- table(clusterresults$cluster, numdata$q4r8)
barplot(plotq4r8, beside = TRUE, col=c("grey","cyan","light green","dark blue"))

plotq4r8a <- prop.table(table(numdata$q4r8, clusterresults$cluster), margin=2)
barplot(plotq4r8a, col=brewer.pal(n = 6, name = "RdBu"))

# Q4r9
plotq4r9 <- table(clusterresults$cluster, numdata$q4r9)
barplot(plotq4r9, beside = TRUE, col=c("grey","cyan","light green","dark blue"))

plotq4r9a <- prop.table(table(numdata$q4r9, clusterresults$cluster), margin=2)
barplot(plotq4r9a, col=brewer.pal(n = 6, name = "RdBu"))

# TESTING Q4r9: News Apps
testingdf$q4r9 <- numdata$q4r9+numdata$q4r7
testingdf$q4r9[testingdf$q4r9>=1] <- 1

Q4r9 <- table(clusterresults$cluster, testingdf$q4r9)
barplot(Q4r9, beside = TRUE, col=c("grey","cyan","light green","dark blue"), xlab="No, Yes")

Q4r9 <- prop.table(table(testingdf$q4r9, clusterresults$cluster), margin=2)
barplot(Q4r9, col=brewer.pal(n = 6, name = "RdBu"), xlab="Cluster")
title("Do You Own an iPhone OR Android?", outer=TRUE)

# Q4r10
plotq4r10 <- table(clusterresults$cluster, numdata$q4r10)
barplot(plotq4r10, beside = TRUE, col=c("grey","cyan","light green","dark blue"))

plotq4r10a <- prop.table(table(numdata$q4r10, clusterresults$cluster), margin=2)
barplot(plotq4r10a, col=brewer.pal(n = 6, name = "RdBu"))

# Q4r11
plotq4r11 <- table(clusterresults$cluster, numdata$q4r11)
barplot(plotq4r11, beside = TRUE, col=c("grey","cyan","light green","dark blue"))

plotq4r11a <- prop.table(table(numdata$q4r11, clusterresults$cluster), margin=2)
barplot(plotq4r11a, col=brewer.pal(n = 6, name = "RdBu"))

# Q13r1
plotq13r1 <- table(clusterresults$cluster, numdata$q13r1)
barplot(plotq13r1, beside = TRUE, col=c("grey","cyan","light green","dark blue"))

plotq13r1a <- prop.table(table(numdata$q13r1, clusterresults$cluster), margin=2)
barplot(plotq13r1a, col=brewer.pal(n = 6, name = "RdBu"))

# Q13r2
plotq13r2 <- table(clusterresults$cluster, numdata$q13r2)
barplot(plotq13r2, beside = TRUE, col=c("grey","cyan","light green","dark blue"))

plotq13r2a <- prop.table(table(numdata$q13r2, clusterresults$cluster), margin=2)
barplot(plotq13r2a, col=brewer.pal(n = 6, name = "RdBu"))

# Q13r3
plotq13r3 <- table(clusterresults$cluster, numdata$q13r3)
barplot(plotq13r3, beside = TRUE, col=c("grey","cyan","light green","dark blue"))

plotq13r3a <- prop.table(table(numdata$q13r3, clusterresults$cluster), margin=2)
barplot(plotq13r3a, col=brewer.pal(n = 6, name = "RdBu"))

# Q13r4
plotq13r4 <- table(clusterresults$cluster, numdata$q13r4)
barplot(plotq13r4, beside = TRUE, col=c("grey","cyan","light green","dark blue"))

plotq13r4a <- prop.table(table(numdata$q13r4, clusterresults$cluster), margin=2)
barplot(plotq13r4a, col=brewer.pal(n = 6, name = "RdBu"))

# Q13r5
plotq13r5 <- table(clusterresults$cluster, numdata$q13r5)
barplot(plotq13r5, beside = TRUE, col=c("grey","cyan","light green","dark blue"))

plotq13r5a <- prop.table(table(numdata$q13r5, clusterresults$cluster), margin=2)
barplot(plotq13r5a, col=brewer.pal(n = 6, name = "RdBu"))

# Q13r6
plotq13r6 <- table(clusterresults$cluster, numdata$q13r6)
barplot(plotq13r6, beside = TRUE, col=c("grey","cyan","light green","dark blue"))

plotq13r6a <- prop.table(table(numdata$q13r6, clusterresults$cluster), margin=2)
barplot(plotq13r6a, col=brewer.pal(n = 6, name = "RdBu"))

# Q13r7
plotq13r7 <- table(clusterresults$cluster, numdata$q13r7)
barplot(plotq13r7, beside = TRUE, col=c("grey","cyan","light green","dark blue"))

plotq13r7a <- prop.table(table(numdata$q13r7, clusterresults$cluster), margin=2)
barplot(plotq13r7a, col=brewer.pal(n = 6, name = "RdBu"))

# Q13r8
plotq13r8 <- table(clusterresults$cluster, numdata$q13r8)
barplot(plotq13r8, beside = TRUE, col=c("grey","cyan","light green","dark blue"))

plotq13r8a <- prop.table(table(numdata$q13r8, clusterresults$cluster), margin=2)
barplot(plotq13r8a, col=brewer.pal(n = 6, name = "RdBu"))

# Q13r9
plotq13r9 <- table(clusterresults$cluster, numdata$q13r9)
barplot(plotq13r9, beside = TRUE, col=c("grey","cyan","light green","dark blue"))

plotq13r9a <- prop.table(table(numdata$q13r9, clusterresults$cluster), margin=2)
barplot(plotq13r9a, col=brewer.pal(n = 6, name = "RdBu"))

# Q13r10
plotq13r10 <- table(clusterresults$cluster, numdata$q13r10)
barplot(plotq13r10, beside = TRUE, col=c("grey","cyan","light green","dark blue"))

plotq13r10a <- prop.table(table(numdata$q13r10, clusterresults$cluster), margin=2)
barplot(plotq13r10a, col=brewer.pal(n = 6, name = "RdBu"))

# Q13r11
plotq13r11 <- table(clusterresults$cluster, numdata$q13r11)
barplot(plotq13r11, beside = TRUE, col=c("grey","cyan","light green","dark blue"))

plotq4r11a <- prop.table(table(numdata$q13r11, clusterresults$cluster), margin=2)
barplot(plotq4r11a, col=brewer.pal(n = 6, name = "RdBu"))

# Q13r12
plotq13r12 <- table(clusterresults$cluster, numdata$q13r12)
barplot(plotq13r12, beside = TRUE, col=c("grey","cyan","light green","dark blue"))

plotq13r12a <- prop.table(table(numdata$q13r12, clusterresults$cluster), margin=2)
barplot(plotq13r12a, col=brewer.pal(n = 6, name = "RdBu"))

# Q24r1
plotq24r1 <- table(clusterresults$cluster, numdata$q24r1)
barplot(plotq24r1, beside = TRUE, col=c("grey","cyan","light green","dark blue"))

plotq24r1a <- prop.table(table(numdata$q24r1, clusterresults$cluster), margin=2)
barplot(plotq24r1a, col=brewer.pal(n = 6, name = "RdBu"))

# Q24r2
plotq24r2 <- table(clusterresults$cluster, numdata$q24r2)
barplot(plotq24r2, beside = TRUE, col=c("grey","cyan","light green","dark blue"))

plotq24r2a <- prop.table(table(numdata$q24r2, clusterresults$cluster), margin=2)
barplot(plotq24r2a, col=brewer.pal(n = 6, name = "RdBu"))

# Q24r3
plotq24r3 <- table(clusterresults$cluster, numdata$q24r3)
barplot(plotq24r3, beside = TRUE, col=c("grey","cyan","light green","dark blue"))

plotq24r3a <- prop.table(table(numdata$q24r3, clusterresults$cluster), margin=2)
barplot(plotq24r3a, col=brewer.pal(n = 6, name = "RdBu"))

# Q24r4
plotq24r4 <- table(clusterresults$cluster, numdata$q24r4)
barplot(plotq24r4, beside = TRUE, col=c("grey","cyan","light green","dark blue"))

plotq24r4a <- prop.table(table(numdata$q24r4, clusterresults$cluster), margin=2)
barplot(plotq24r4a, col=brewer.pal(n = 6, name = "RdBu"))

# Q24r5
plotq24r5 <- table(clusterresults$cluster, numdata$q24r5)
barplot(plotq24r5, beside = TRUE, col=c("grey","cyan","light green","dark blue"))

plotq24r5a <- prop.table(table(numdata$q24r5, clusterresults$cluster), margin=2)
barplot(plotq24r5a, col=brewer.pal(n = 6, name = "RdBu"))

# Q24r6
plotq24r6 <- table(clusterresults$cluster, numdata$q24r6)
barplot(plotq24r6, beside = TRUE, col=c("grey","cyan","light green","dark blue"))

plotq24r6a <- prop.table(table(numdata$q24r6, clusterresults$cluster), margin=2)
barplot(plotq24r6a, col=brewer.pal(n = 6, name = "RdBu"))

# Q24r7
plotq24r7 <- table(clusterresults$cluster, numdata$q24r7)
barplot(plotq24r7, beside = TRUE, col=c("grey","cyan","light green","dark blue"))

plotq24r7a <- prop.table(table(numdata$q24r7, clusterresults$cluster), margin=2)
barplot(plotq24r7a, col=brewer.pal(n = 6, name = "RdBu"))

# Q24r8
plotq24r8 <- table(clusterresults$cluster, numdata$q24r8)
barplot(plotq24r8, beside = TRUE, col=c("grey","cyan","light green","dark blue"))

plotq24r8a <- prop.table(table(numdata$q24r8, clusterresults$cluster), margin=2)
barplot(plotq24r8a, col=brewer.pal(n = 6, name = "RdBu"))

# Q24r9
plotq24r9 <- table(clusterresults$cluster, numdata$q24r9)
barplot(plotq24r9, beside = TRUE, col=c("grey","cyan","light green","dark blue"))

plotq24r9a <- prop.table(table(numdata$q24r9, clusterresults$cluster), margin=2)
barplot(plotq24r9a, col=brewer.pal(n = 6, name = "RdBu"))

# Q24r10
plotq24r10 <- table(clusterresults$cluster, numdata$q24r10)
barplot(plotq24r10, beside = TRUE, col=c("grey","cyan","light green","dark blue"))

plotq24r10a <- prop.table(table(numdata$q24r10, clusterresults$cluster), margin=2)
barplot(plotq24r10a, col=brewer.pal(n = 6, name = "RdBu"))

# Q24r11
plotq24r11 <- table(clusterresults$cluster, numdata$q24r11)
barplot(plotq24r11, beside = TRUE, col=c("grey","cyan","light green","dark blue"))

plotq4r11a <- prop.table(table(numdata$q24r11, clusterresults$cluster), margin=2)
barplot(plotq4r11a, col=brewer.pal(n = 6, name = "RdBu"))

# Q24r12
plotq24r12 <- table(clusterresults$cluster, numdata$q24r12)
barplot(plotq24r12, beside = TRUE, col=c("grey","cyan","light green","dark blue"))

plotq24r12a <- prop.table(table(numdata$q24r12, clusterresults$cluster), margin=2)
barplot(plotq24r12a, col=brewer.pal(n = 6, name = "RdBu"))

# Q25r1
plotq25r1 <- table(clusterresults$cluster, numdata$q25r1)
barplot(plotq25r1, beside = TRUE, col=c("grey","cyan","light green","dark blue"))

plotq25r1a <- prop.table(table(numdata$q25r1, clusterresults$cluster), margin=2)
barplot(plotq25r1a, col=brewer.pal(n = 6, name = "RdBu"))

# Q25r2
plotq25r2 <- table(clusterresults$cluster, numdata$q25r2)
barplot(plotq25r2, beside = TRUE, col=c("grey","cyan","light green","dark blue"))

plotq25r2a <- prop.table(table(numdata$q25r2, clusterresults$cluster), margin=2)
barplot(plotq25r2a, col=brewer.pal(n = 6, name = "RdBu"))

# Q25r3
plotq25r3 <- table(clusterresults$cluster, numdata$q25r3)
barplot(plotq25r3, beside = TRUE, col=c("grey","cyan","light green","dark blue"))

plotq25r3a <- prop.table(table(numdata$q25r3, clusterresults$cluster), margin=2)
barplot(plotq25r3a, col=brewer.pal(n = 6, name = "RdBu"))

# Q25r4
plotq25r4 <- table(clusterresults$cluster, numdata$q25r4)
barplot(plotq25r4, beside = TRUE, col=c("grey","cyan","light green","dark blue"))

plotq25r4a <- prop.table(table(numdata$q25r4, clusterresults$cluster), margin=2)
barplot(plotq25r4a, col=brewer.pal(n = 6, name = "RdBu"))

# Q25r5
plotq25r5 <- table(clusterresults$cluster, numdata$q25r5)
barplot(plotq25r5, beside = TRUE, col=c("grey","cyan","light green","dark blue"))

plotq25r5a <- prop.table(table(numdata$q25r5, clusterresults$cluster), margin=2)
barplot(plotq25r5a, col=brewer.pal(n = 6, name = "RdBu"))

# Q25r6
plotq25r6 <- table(clusterresults$cluster, numdata$q25r6)
barplot(plotq25r6, beside = TRUE, col=c("grey","cyan","light green","dark blue"))

plotq25r6a <- prop.table(table(numdata$q25r6, clusterresults$cluster), margin=2)
barplot(plotq25r6a, col=brewer.pal(n = 6, name = "RdBu"))

# Q25r7
plotq25r7 <- table(clusterresults$cluster, numdata$q25r7)
barplot(plotq25r7, beside = TRUE, col=c("grey","cyan","light green","dark blue"))

plotq25r7a <- prop.table(table(numdata$q25r7, clusterresults$cluster), margin=2)
barplot(plotq25r7a, col=brewer.pal(n = 6, name = "RdBu"))

# Q25r8
plotq25r8 <- table(clusterresults$cluster, numdata$q25r8)
barplot(plotq25r8, beside = TRUE, col=c("grey","cyan","light green","dark blue"))

plotq25r8a <- prop.table(table(numdata$q25r8, clusterresults$cluster), margin=2)
barplot(plotq25r8a, col=brewer.pal(n = 6, name = "RdBu"))

# Q26r3
plotq26r3 <- table(clusterresults$cluster, numdata$q26r3)
barplot(plotq26r3, beside = TRUE, col=c("grey","cyan","light green","dark blue"))

plotq26r3a <- prop.table(table(numdata$q26r3, clusterresults$cluster), margin=2)
barplot(plotq26r3a, col=brewer.pal(n = 6, name = "RdBu"))

# Q26r4
plotq26r4 <- table(clusterresults$cluster, numdata$q26r4)
barplot(plotq26r4, beside = TRUE, col=c("grey","cyan","light green","dark blue"))

plotq26r4a <- prop.table(table(numdata$q26r4, clusterresults$cluster), margin=2)
barplot(plotq26r4a, col=brewer.pal(n = 6, name = "RdBu"))

# Q26r5
plotq26r5 <- table(clusterresults$cluster, numdata$q26r5)
barplot(plotq26r5, beside = TRUE, col=c("grey","cyan","light green","dark blue"))

plotq26r5a <- prop.table(table(numdata$q26r5, clusterresults$cluster), margin=2)
barplot(plotq26r5a, col=brewer.pal(n = 6, name = "RdBu"))

# Q26r6
plotq26r6 <- table(clusterresults$cluster, numdata$q26r6)
barplot(plotq26r6, beside = TRUE, col=c("grey","cyan","light green","dark blue"))

plotq26r6a <- prop.table(table(numdata$q26r6, clusterresults$cluster), margin=2)
barplot(plotq26r6a, col=brewer.pal(n = 6, name = "RdBu"))

# Q26r7
plotq26r7 <- table(clusterresults$cluster, numdata$q26r7)
barplot(plotq26r7, beside = TRUE, col=c("grey","cyan","light green","dark blue"))

plotq26r7a <- prop.table(table(numdata$q26r7, clusterresults$cluster), margin=2)
barplot(plotq26r7a, col=brewer.pal(n = 6, name = "RdBu"))

# Q26r8
plotq26r8 <- table(clusterresults$cluster, numdata$q26r8)
barplot(plotq26r8, beside = TRUE, col=c("grey","cyan","light green","dark blue"))

plotq26r8a <- prop.table(table(numdata$q26r8, clusterresults$cluster), margin=2)
barplot(plotq26r8a, col=brewer.pal(n = 6, name = "RdBu"))

# Q26r9
plotq26r9 <- table(clusterresults$cluster, numdata$q26r9)
barplot(plotq26r9, beside = TRUE, col=c("grey","cyan","light green","dark blue"))

plotq26r9a <- prop.table(table(numdata$q26r9, clusterresults$cluster), margin=2)
barplot(plotq26r9a, col=brewer.pal(n = 6, name = "RdBu"))

# Q26r10
plotq26r10 <- table(clusterresults$cluster, numdata$q26r10)
barplot(plotq26r10, beside = TRUE, col=c("grey","cyan","light green","dark blue"))

plotq26r10a <- prop.table(table(numdata$q26r10, clusterresults$cluster), margin=2)
barplot(plotq26r10a, col=brewer.pal(n = 6, name = "RdBu"))

# Q26r11
plotq26r11 <- table(clusterresults$cluster, numdata$q26r11)
barplot(plotq26r11, beside = TRUE, col=c("grey","cyan","light green","dark blue"))

plotq4r11a <- prop.table(table(numdata$q26r11, clusterresults$cluster), margin=2)
barplot(plotq4r11a, col=brewer.pal(n = 6, name = "RdBu"))

# Q26r12
plotq26r12 <- table(clusterresults$cluster, numdata$q26r12)
barplot(plotq26r12, beside = TRUE, col=c("grey","cyan","light green","dark blue"))

plotq26r12a <- prop.table(table(numdata$q26r12, clusterresults$cluster), margin=2)
barplot(plotq26r12a, col=brewer.pal(n = 6, name = "RdBu"))

# Q26r13
plotq26r13 <- table(clusterresults$cluster, numdata$q26r13)
barplot(plotq26r13, beside = TRUE, col=c("grey","cyan","light green","dark blue"))

plotq26r13a <- prop.table(table(numdata$q26r13, clusterresults$cluster), margin=2)
barplot(plotq26r13a, col=brewer.pal(n = 6, name = "RdBu"))

# Q26r14
plotq26r14 <- table(clusterresults$cluster, numdata$q26r14)
barplot(plotq26r14, beside = TRUE, col=c("grey","cyan","light green","dark blue"))

plotq26r14a <- prop.table(table(numdata$q26r14, clusterresults$cluster), margin=2)
barplot(plotq26r14a, col=brewer.pal(n = 6, name = "RdBu"))

# Q26r15
plotq26r15 <- table(clusterresults$cluster, numdata$q26r15)
barplot(plotq26r15, beside = TRUE, col=c("grey","cyan","light green","dark blue"))

plotq26r15a <- prop.table(table(numdata$q26r15, clusterresults$cluster), margin=2)
barplot(plotq26r15a, col=brewer.pal(n = 6, name = "RdBu"))

# Q26r16
plotq26r16 <- table(clusterresults$cluster, numdata$q26r16)
barplot(plotq26r16, beside = TRUE, col=c("grey","cyan","light green","dark blue"))

plotq26r16a <- prop.table(table(numdata$q26r16, clusterresults$cluster), margin=2)
barplot(plotq26r16a, col=brewer.pal(n = 6, name = "RdBu"))

# Q26r17
plotq26r17 <- table(clusterresults$cluster, numdata$q26r17)
barplot(plotq26r17, beside = TRUE, col=c("grey","cyan","light green","dark blue"))

plotq26r17a <- prop.table(table(numdata$q26r17, clusterresults$cluster), margin=2)
barplot(plotq26r17a, col=brewer.pal(n = 6, name = "RdBu"))

# Q26r18
plotq26r18 <- table(clusterresults$cluster, numdata$q26r18)
barplot(plotq26r18, beside = TRUE, col=c("grey","cyan","light green","dark blue"))

plotq26r18a <- prop.table(table(numdata$q26r18, clusterresults$cluster), margin=2)
barplot(plotq26r18a, col=brewer.pal(n = 6, name = "RdBu"))

# Q50r1
plotq50r1 <- table(clusterresults$cluster, numdata$q50r1)
barplot(plotq50r1, beside = TRUE, col=c("grey","cyan","light green","dark blue"))

plotq50r1a <- prop.table(table(numdata$q50r1, clusterresults$cluster), margin=2)
barplot(plotq50r1a, col=brewer.pal(n = 6, name = "RdBu"))

# Q50r2
plotq50r2 <- table(clusterresults$cluster, numdata$q50r2)
barplot(plotq50r2, beside = TRUE, col=c("grey","cyan","light green","dark blue"))

plotq50r2a <- prop.table(table(numdata$q50r2, clusterresults$cluster), margin=2)
barplot(plotq50r2a, col=brewer.pal(n = 6, name = "RdBu"))

# Q50r3
plotq50r3 <- table(clusterresults$cluster, numdata$q50r3)
barplot(plotq50r3, beside = TRUE, col=c("grey","cyan","light green","dark blue"))

plotq50r3a <- prop.table(table(numdata$q50r3, clusterresults$cluster), margin=2)
barplot(plotq50r3a, col=brewer.pal(n = 6, name = "RdBu"))

# Q50r4
plotq50r4 <- table(clusterresults$cluster, numdata$q50r4)
barplot(plotq50r4, beside = TRUE, col=c("grey","cyan","light green","dark blue"))

plotq50r4a <- prop.table(table(numdata$q50r4, clusterresults$cluster), margin=2)
barplot(plotq50r4a, col=brewer.pal(n = 6, name = "RdBu"))

# Q50r5
plotq50r5 <- table(clusterresults$cluster, numdata$q50r5)
barplot(plotq50r5, beside = TRUE, col=c("grey","cyan","light green","dark blue"))

plotq50r5a <- prop.table(table(numdata$q50r5, clusterresults$cluster), margin=2)
barplot(plotq50r5a, col=brewer.pal(n = 6, name = "RdBu"))


