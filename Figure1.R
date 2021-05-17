# Function in R to estimate the confidence interval of the admissible error (E).
# Created by: Daiana Novello, Adilson dos Anjosb
# Paper: Number of children needed to evaluate products made in cooking workshops
# Figure 1

library(Rmisc)
library(bootstrap)
library(ggplot2)
library(gmodels)
library(dplyr)
library(tidyverse)
library(grid)
library(gridExtra)

# Load data
load("EciTable4.RData")
load("EciTable5_female.RData")
load("EciTable5_male.RData")
load("EciTable5_female.RData")
load("EciTable6_ExcessWeight")
load("EciTable6_Eutrophy")

#data figure (a) 
E.ci.table4$n<-as.numeric(E.ci.table4$n)
E.ci.table4$mean<-as.numeric(E.ci.table4$mean)
E.ci.table4$food<-as.factor(E.ci.table4$food)
E.ci.table4$variable<-as.factor(E.ci.table4$variable)
str(E.ci.table4)

dataT4<-as.data.frame(subset(E.ci.table4, select = -c(lower, upper)))
str(dataT4)

dataT4<-subset(dataT4, food=="cookie")
dataT4<-subset(dataT4, n>3)

#data figure (b) 
E.ci.Table5_male$n<-as.numeric(E.ci.Table5_male$n)
E.ci.Table5_male$mean<-as.numeric(E.ci.Table5_male$mean)
E.ci.Table5_male$food<-as.factor(E.ci.Table5_male$food)
E.ci.Table5_male$variable<-as.factor(E.ci.Table5_male$variable)
str(E.ci.Table5_male)

dataT5.M<-as.data.frame(subset(E.ci.Table5_male, select = -c(lower, upper)))
str(dataT5.M)

dataT5.M<-subset(dataT5.M, food=="cookie")
dataT5.M<-subset(dataT5.M, n>3)


#data figure (c) 
E.ci.Table5_female$n<-as.numeric(E.ci.Table5_female$n)
E.ci.Table5_female$mean<-as.numeric(E.ci.Table5_female$mean)
E.ci.Table5_female$food<-as.factor(E.ci.Table5_female$food)
E.ci.Table5_female$variable<-as.factor(E.ci.Table5_female$variable)
str(E.ci.Table5_female)

dataT5.F<-as.data.frame(subset(E.ci.Table5_female, select = -c(lower, upper)))
str(dataT5.F)

dataT5.F<-subset(dataT5.F, food=="cookie")
dataT5.F<-subset(dataT5.F, n>3)


#data figure (d) 
E.ci.Table6_Eutrophy$n<-as.numeric(E.ci.Table6_Eutrophy$n)
E.ci.Table6_Eutrophy$mean<-as.numeric(E.ci.Table6_Eutrophy$mean)
E.ci.Table6_Eutrophy$food<-as.factor(E.ci.Table6_Eutrophy$food)
E.ci.Table6_Eutrophy$variable<-as.factor(E.ci.Table6_Eutrophy$variable)
str(E.ci.Table6_Eutrophy)

dataT6.E<-as.data.frame(subset(E.ci.Table6_Eutrophy, select = -c(lower, upper)))
str(dataT6.E)

dataT6.E<-subset(dataT6.E, food=="cookie")
dataT6.E<-subset(dataT6.E, n>3)

#data figure (e) 
E.ci.Table6_ExcessWeight$n<-as.numeric(E.ci.Table6_ExcessWeight$n)
E.ci.Table6_ExcessWeight$mean<-as.numeric(E.ci.Table6_ExcessWeight$mean)
E.ci.Table6_ExcessWeight$food<-as.factor(E.ci.Table6_ExcessWeight$food)
E.ci.Table6_ExcessWeight$variable<-as.factor(E.ci.Table6_ExcessWeight$variable)
str(E.ci.Table6_ExcessWeight)

dataT6.EW<-as.data.frame(subset(E.ci.Table6_ExcessWeight, select = -c(lower, upper)))
str(dataT6.EW)

dataT6.EW<-subset(dataT6.EW, food=="cookie")
dataT6.EW<-subset(dataT6.EW, n>3)


#plots

P1<-ggplot(dataT4, aes(x=n, y=mean, group=variable, color=variable)) +
  geom_line() +
  labs(y="Error (E)",
       x="Sample \n (General evaluation)")

P2<-ggplot(dataT5.M, aes(x=n, y=mean, group=variable, color=variable)) +
  geom_line() +
  labs(y="Error (E)",
       x="Sample \n (General evaluation)")

P3<-ggplot(dataT5.F, aes(x=n, y=mean, group=variable, color=variable)) +
  geom_line() +
  labs(y="Error (E)",
       x="Sample \n (General evaluation)")

P4<-ggplot(dataT6.E, aes(x=n, y=mean, group=variable, color=variable)) +
  geom_line() +
  labs(y="Error (E)",
       x="Sample \n (General evaluation)")


P5<-ggplot(dataT6.EW, aes(x=n, y=mean, group=variable, color=variable)) +
  geom_line() +
  labs(y="Error (E)",
       x="Sample \n (General evaluation)")


grid.arrange(P1, P2, P3, P4, P5, nrow = 3)
