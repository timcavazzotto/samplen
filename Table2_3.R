# Function in R to estimate the confidence interval of the admissible error (E).
# Created by: Daiana Novello, Adilson dos Anjosb
# Paper: Number of children needed to evaluate products made in cooking workshops
# TABLE 2/3

library(Rmisc)
library(bootstrap)


# Load data
load("bread.RData")
load("cookie.RData")
load("muffin.RData")
load("pancake.RData")
load("sfiha.RData")

##### TABLE 2 ####
#gender
fbread<-ftable(bread$gender)
frbread<-ftable(bread$gender)/length(bread$gender)
fcookie<-ftable(cookie$gender)
frcookie<-ftable(cookie$gender)/length(cookie$gender)
fmuffin<-ftable(muffin$gender)
frmuffin<-ftable(muffin$gender)/length(muffin$gender)
fpancake<-ftable(pancake$gender)
frpancake<-ftable(pancake$gender)/length(pancake$gender)
fsfiha<-ftable(sfiha$gender)
frsfiha<-ftable(sfiha$gender)/length(sfiha$gender)

#nutstatus
fbread<-ftable(bread$nutstatus)
frbread<-ftable(bread$nutstatus)/length(bread$nutstatus)
fcookie<-ftable(cookie$nutstatus)
frcookie<-ftable(cookie$nutstatus)/length(cookie$nutstatus)
fmuffin<-ftable(muffin$nutstatus)
frmuffin<-ftable(muffin$nutstatus)/length(muffin$nutstatus)
fpancake<-ftable(pancake$nutstatus)
frpancake<-ftable(pancake$nutstatus)/length(pancake$nutstatus)
fsfiha<-ftable(sfiha$nutstatus)
frsfiha<-ftable(sfiha$nutstatus)/length(sfiha$nutstatus)


#### TABLE 3 ####
#mean and sd
summary(bread)
lapply(bread,sd)

summary(cookie)
lapply(cookie,sd)

summary(muffin)
lapply(muffin,sd)

summary(pancake)
lapply(pancake,sd)

summary(sfiha)
lapply(sfiha,sd)


# acceptability index (AI)

mean((bread$acceptance)*100)/max(bread$acceptance)
mean((muffin$acceptance)*100)/max(muffin$acceptance)
mean((cookie$acceptance)*100)/max(cookie$acceptance)
mean((pancake$acceptance)*100)/max(pancake$acceptance)
mean((sfiha$acceptance)*100)/max(sfiha$acceptance)


