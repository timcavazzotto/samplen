# Function in R to estimate the confidence interval of the admissible error (E).
# Created by: Daiana Novello, Adilson dos Anjosb
# Paper: Number of children needed to evaluate products made in cooking workshops
# TABLE 6

library(Rmisc)
library(bootstrap)


# Load data
load("bread.RData")
load("cookie.RData")
load("muffin.RData")
load("pancake.RData")
load("sfiha.RData")


# subgroups nutstatus==2 (eutrophy)

#### BREAD ####
## BREAD==appearance
E.ci.T6.appearance<-data.frame(n=integer(),
                               food=numeric(),
                               nutstatus=numeric(),
                               variable=numeric(),
                               lower=numeric(),
                               mean=numeric(),
                               upper=numeric())

for (i in 1:160) {
  
  samplen<-function(n){
    E<-c()
    for(i in 1:500){
      x<-sample(bread$appearance[bread$nutstatus==2], n, replace = F) # change the parameters for others subgroups
      theta <- function(x){mean(x)}
      results <- bootstrap(x, 1000, theta)
      E[i]<-(quantile(results$thetastar, probs=.975 ,type=3, names=F)-quantile(results$thetastar, probs=.025 ,type=3, names=F))/2
    }
    return(CI(E))
  }
  
  E.ci.T6.appearance[i,]<-c(i,"bread","eutrophy","appearance",samplen(i))
  
  i=i+1
  if (i > length(bread$appearance[bread$nutstatus==2])) break()
  
}

## BREAD==aroma
E.ci.T6.aroma<-data.frame(n=integer(),
                          food=numeric(),
                          nutstatus=numeric(),
                          variable=numeric(),
                          lower=numeric(),
                          mean=numeric(),
                          upper=numeric())

for (i in 1:160) {
  
  samplen<-function(n){
    E<-c()
    for(i in 1:500){
      x<-sample(bread$aroma[bread$nutstatus==2], n, replace = F) # change the parameters for others subgroups
      theta <- function(x){mean(x)}
      results <- bootstrap(x, 1000, theta)
      E[i]<-(quantile(results$thetastar, probs=.975 ,type=3, names=F)-quantile(results$thetastar, probs=.025 ,type=3, names=F))/2
    }
    return(CI(E))
  }
  
  E.ci.T6.aroma[i,]<-c(i,"bread","eutrophy","aroma",samplen(i))
  
  i=i+1
  if (i > length(bread$aroma[bread$nutstatus==2])) break()
  
}

## BREAD==taste
E.ci.T6.taste<-data.frame(n=integer(),
                          food=numeric(),
                          nutstatus=numeric(),
                          variable=numeric(),
                          lower=numeric(),
                          mean=numeric(),
                          upper=numeric())

for (i in 1:160) {
  
  samplen<-function(n){
    E<-c()
    for(i in 1:500){
      x<-sample(bread$taste[bread$nutstatus==2], n, replace = F) # change the parameters for others subgroups
      theta <- function(x){mean(x)}
      results <- bootstrap(x, 1000, theta)
      E[i]<-(quantile(results$thetastar, probs=.975 ,type=3, names=F)-quantile(results$thetastar, probs=.025 ,type=3, names=F))/2
    }
    return(CI(E))
  }
  
  E.ci.T6.taste[i,]<-c(i,"bread","eutrophy","taste",samplen(i))
  
  i=i+1
  if (i > length(bread$taste[bread$nutstatus==2])) break()
  
}

## BREAD==texture
E.ci.T6.texture<-data.frame(n=integer(),
                            food=numeric(),
                            nutstatus=numeric(),
                            variable=numeric(),
                            lower=numeric(),
                            mean=numeric(),
                            upper=numeric())

for (i in 1:160) {
  
  samplen<-function(n){
    E<-c()
    for(i in 1:500){
      x<-sample(bread$texture[bread$nutstatus==2], n, replace = F) # change the parameters for others subgroups
      theta <- function(x){mean(x)}
      results <- bootstrap(x, 1000, theta)
      E[i]<-(quantile(results$thetastar, probs=.975 ,type=3, names=F)-quantile(results$thetastar, probs=.025 ,type=3, names=F))/2
    }
    return(CI(E))
  }
  
  E.ci.T6.texture[i,]<-c(i,"bread","eutrophy","texture",samplen(i))
  
  i=i+1
  if (i > length(bread$texture[bread$nutstatus==2])) break()
  
}

## BREAD==color
E.ci.T6.color<-data.frame(n=integer(),
                          food=numeric(),
                          nutstatus=numeric(),
                          variable=numeric(),
                          lower=numeric(),
                          mean=numeric(),
                          upper=numeric())

for (i in 1:160) {
  
  samplen<-function(n){
    E<-c()
    for(i in 1:500){
      x<-sample(bread$color[bread$nutstatus==2], n, replace = F) # change the parameters for others subgroups
      theta <- function(x){mean(x)}
      results <- bootstrap(x, 1000, theta)
      E[i]<-(quantile(results$thetastar, probs=.975 ,type=3, names=F)-quantile(results$thetastar, probs=.025 ,type=3, names=F))/2
    }
    return(CI(E))
  }
  
  E.ci.T6.color[i,]<-c(i,"bread","eutrophy","color",samplen(i))
  
  i=i+1
  if (i > length(bread$color[bread$nutstatus==2])) break()
  
}

## BREAD==acceptance
E.ci.T6.acceptance<-data.frame(n=integer(),
                               food=numeric(),
                               nutstatus=numeric(),
                               variable=numeric(),
                               lower=numeric(),
                               mean=numeric(),
                               upper=numeric())

for (i in 1:160) {
  
  samplen<-function(n){
    E<-c()
    for(i in 1:500){
      x<-sample(bread$acceptance[bread$nutstatus==2], n, replace = F) # change the parameters for others subgroups
      theta <- function(x){mean(x)}
      results <- bootstrap(x, 1000, theta)
      E[i]<-(quantile(results$thetastar, probs=.975 ,type=3, names=F)-quantile(results$thetastar, probs=.025 ,type=3, names=F))/2
    }
    return(CI(E))
  }
  
  E.ci.T6.acceptance[i,]<-c(i,"bread","eutrophy","acceptance",samplen(i))
  
  i=i+1
  if (i > length(bread$acceptance[bread$nutstatus==2])) break()
  
}


#### combining dataframes bread
E.ci.table6.bread.M<-rbind(E.ci.T6.appearance,E.ci.T6.aroma,E.ci.T6.taste,E.ci.T6.texture,E.ci.T6.color)



####COOKIE####
## cookie==appearance
E.ci.T6.appearance<-data.frame(n=integer(),
                               food=numeric(),
                               nutstatus=numeric(),
                               variable=numeric(),
                               lower=numeric(),
                               mean=numeric(),
                               upper=numeric())

for (i in 1:160) {
  
  samplen<-function(n){
    E<-c()
    for(i in 1:500){
      x<-sample(cookie$appearance[cookie$nutstatus==2], n, replace = F) # change the parameters for others subgroups
      theta <- function(x){mean(x)}
      results <- bootstrap(x, 1000, theta)
      E[i]<-(quantile(results$thetastar, probs=.975 ,type=3, names=F)-quantile(results$thetastar, probs=.025 ,type=3, names=F))/2
    }
    return(CI(E))
  }
  
  E.ci.T6.appearance[i,]<-c(i,"cookie","eutrophy","appearance",samplen(i))
  
  i=i+1
  if (i > length(cookie$appearance[cookie$nutstatus==2])) break()
  
}

## cookie==aroma
E.ci.T6.aroma<-data.frame(n=integer(),
                          food=numeric(),
                          nutstatus=numeric(),
                          variable=numeric(),
                          lower=numeric(),
                          mean=numeric(),
                          upper=numeric())

for (i in 1:160) {
  
  samplen<-function(n){
    E<-c()
    for(i in 1:500){
      x<-sample(cookie$aroma[cookie$nutstatus==2], n, replace = F) # change the parameters for others subgroups
      theta <- function(x){mean(x)}
      results <- bootstrap(x, 1000, theta)
      E[i]<-(quantile(results$thetastar, probs=.975 ,type=3, names=F)-quantile(results$thetastar, probs=.025 ,type=3, names=F))/2
    }
    return(CI(E))
  }
  
  E.ci.T6.aroma[i,]<-c(i,"cookie","eutrophy","aroma",samplen(i))
  
  i=i+1
  if (i > length(cookie$aroma[cookie$nutstatus==2])) break()
  
}

## cookie==taste
E.ci.T6.taste<-data.frame(n=integer(),
                          food=numeric(),
                          nutstatus=numeric(),
                          variable=numeric(),
                          lower=numeric(),
                          mean=numeric(),
                          upper=numeric())

for (i in 1:160) {
  
  samplen<-function(n){
    E<-c()
    for(i in 1:500){
      x<-sample(cookie$taste[cookie$nutstatus==2], n, replace = F) # change the parameters for others subgroups
      theta <- function(x){mean(x)}
      results <- bootstrap(x, 1000, theta)
      E[i]<-(quantile(results$thetastar, probs=.975 ,type=3, names=F)-quantile(results$thetastar, probs=.025 ,type=3, names=F))/2
    }
    return(CI(E))
  }
  
  E.ci.T6.taste[i,]<-c(i,"cookie","eutrophy","taste",samplen(i))
  
  i=i+1
  if (i > length(cookie$taste[cookie$nutstatus==2])) break()
  
}

## cookie==texture
E.ci.T6.texture<-data.frame(n=integer(),
                            food=numeric(),
                            nutstatus=numeric(),
                            variable=numeric(),
                            lower=numeric(),
                            mean=numeric(),
                            upper=numeric())

for (i in 1:160) {
  
  samplen<-function(n){
    E<-c()
    for(i in 1:500){
      x<-sample(cookie$texture[cookie$nutstatus==2], n, replace = F) # change the parameters for others subgroups
      theta <- function(x){mean(x)}
      results <- bootstrap(x, 1000, theta)
      E[i]<-(quantile(results$thetastar, probs=.975 ,type=3, names=F)-quantile(results$thetastar, probs=.025 ,type=3, names=F))/2
    }
    return(CI(E))
  }
  
  E.ci.T6.texture[i,]<-c(i,"cookie","eutrophy","texture",samplen(i))
  
  i=i+1
  if (i > length(cookie$texture[cookie$nutstatus==2])) break()
  
}

## cookie==color
E.ci.T6.color<-data.frame(n=integer(),
                          food=numeric(),
                          nutstatus=numeric(),
                          variable=numeric(),
                          lower=numeric(),
                          mean=numeric(),
                          upper=numeric())

for (i in 1:160) {
  
  samplen<-function(n){
    E<-c()
    for(i in 1:500){
      x<-sample(cookie$color[cookie$nutstatus==2], n, replace = F) # change the parameters for others subgroups
      theta <- function(x){mean(x)}
      results <- bootstrap(x, 1000, theta)
      E[i]<-(quantile(results$thetastar, probs=.975 ,type=3, names=F)-quantile(results$thetastar, probs=.025 ,type=3, names=F))/2
    }
    return(CI(E))
  }
  
  E.ci.T6.color[i,]<-c(i,"cookie","eutrophy","color",samplen(i))
  
  i=i+1
  if (i > length(cookie$color[cookie$nutstatus==2])) break()
  
}

## cookie==acceptance
E.ci.T6.acceptance<-data.frame(n=integer(),
                               food=numeric(),
                               nutstatus=numeric(),
                               variable=numeric(),
                               lower=numeric(),
                               mean=numeric(),
                               upper=numeric())

for (i in 1:160) {
  
  samplen<-function(n){
    E<-c()
    for(i in 1:500){
      x<-sample(cookie$acceptance[cookie$nutstatus==2], n, replace = F) # change the parameters for others subgroups
      theta <- function(x){mean(x)}
      results <- bootstrap(x, 1000, theta)
      E[i]<-(quantile(results$thetastar, probs=.975 ,type=3, names=F)-quantile(results$thetastar, probs=.025 ,type=3, names=F))/2
    }
    return(CI(E))
  }
  
  E.ci.T6.acceptance[i,]<-c(i,"cookie","eutrophy","acceptance",samplen(i))
  
  i=i+1
  if (i > length(cookie$acceptance[cookie$nutstatus==2])) break()
  
}


#### combining dataframes cookie
E.ci.table6.cookie.M<-rbind(E.ci.T6.appearance,E.ci.T6.aroma,E.ci.T6.taste,E.ci.T6.texture,E.ci.T6.color)

#### MUFFIN ####
## muffin==appearance
E.ci.T6.appearance<-data.frame(n=integer(),
                               food=numeric(),
                               nutstatus=numeric(),
                               variable=numeric(),
                               lower=numeric(),
                               mean=numeric(),
                               upper=numeric())

for (i in 1:160) {
  
  samplen<-function(n){
    E<-c()
    for(i in 1:500){
      x<-sample(muffin$appearance[muffin$nutstatus==2], n, replace = F) # change the parameters for others subgroups
      theta <- function(x){mean(x)}
      results <- bootstrap(x, 1000, theta)
      E[i]<-(quantile(results$thetastar, probs=.975 ,type=3, names=F)-quantile(results$thetastar, probs=.025 ,type=3, names=F))/2
    }
    return(CI(E))
  }
  
  E.ci.T6.appearance[i,]<-c(i,"muffin","eutrophy","appearance",samplen(i))
  
  i=i+1
  if (i > length(muffin$appearance[muffin$nutstatus==2])) break()
  
}

## muffin==aroma
E.ci.T6.aroma<-data.frame(n=integer(),
                          food=numeric(),
                          nutstatus=numeric(),
                          variable=numeric(),
                          lower=numeric(),
                          mean=numeric(),
                          upper=numeric())

for (i in 1:160) {
  
  samplen<-function(n){
    E<-c()
    for(i in 1:500){
      x<-sample(muffin$aroma[muffin$nutstatus==2], n, replace = F) # change the parameters for others subgroups
      theta <- function(x){mean(x)}
      results <- bootstrap(x, 1000, theta)
      E[i]<-(quantile(results$thetastar, probs=.975 ,type=3, names=F)-quantile(results$thetastar, probs=.025 ,type=3, names=F))/2
    }
    return(CI(E))
  }
  
  E.ci.T6.aroma[i,]<-c(i,"muffin","eutrophy","aroma",samplen(i))
  
  i=i+1
  if (i > length(muffin$aroma[muffin$nutstatus==2])) break()
  
}

## muffin==taste
E.ci.T6.taste<-data.frame(n=integer(),
                          food=numeric(),
                          nutstatus=numeric(),
                          variable=numeric(),
                          lower=numeric(),
                          mean=numeric(),
                          upper=numeric())

for (i in 1:160) {
  
  samplen<-function(n){
    E<-c()
    for(i in 1:500){
      x<-sample(muffin$taste[muffin$nutstatus==2], n, replace = F) # change the parameters for others subgroups
      theta <- function(x){mean(x)}
      results <- bootstrap(x, 1000, theta)
      E[i]<-(quantile(results$thetastar, probs=.975 ,type=3, names=F)-quantile(results$thetastar, probs=.025 ,type=3, names=F))/2
    }
    return(CI(E))
  }
  
  E.ci.T6.taste[i,]<-c(i,"muffin","eutrophy","taste",samplen(i))
  
  i=i+1
  if (i > length(muffin$taste[muffin$nutstatus==2])) break()
  
}

## muffin==texture
E.ci.T6.texture<-data.frame(n=integer(),
                            food=numeric(),
                            nutstatus=numeric(),
                            variable=numeric(),
                            lower=numeric(),
                            mean=numeric(),
                            upper=numeric())

for (i in 1:160) {
  
  samplen<-function(n){
    E<-c()
    for(i in 1:500){
      x<-sample(muffin$texture[muffin$nutstatus==2], n, replace = F) # change the parameters for others subgroups
      theta <- function(x){mean(x)}
      results <- bootstrap(x, 1000, theta)
      E[i]<-(quantile(results$thetastar, probs=.975 ,type=3, names=F)-quantile(results$thetastar, probs=.025 ,type=3, names=F))/2
    }
    return(CI(E))
  }
  
  E.ci.T6.texture[i,]<-c(i,"muffin","eutrophy","texture",samplen(i))
  
  i=i+1
  if (i > length(muffin$texture[muffin$nutstatus==2])) break()
  
}

## muffin==color
E.ci.T6.color<-data.frame(n=integer(),
                          food=numeric(),
                          nutstatus=numeric(),
                          variable=numeric(),
                          lower=numeric(),
                          mean=numeric(),
                          upper=numeric())

for (i in 1:160) {
  
  samplen<-function(n){
    E<-c()
    for(i in 1:500){
      x<-sample(muffin$color[muffin$nutstatus==2], n, replace = F) # change the parameters for others subgroups
      theta <- function(x){mean(x)}
      results <- bootstrap(x, 1000, theta)
      E[i]<-(quantile(results$thetastar, probs=.975 ,type=3, names=F)-quantile(results$thetastar, probs=.025 ,type=3, names=F))/2
    }
    return(CI(E))
  }
  
  E.ci.T6.color[i,]<-c(i,"muffin","eutrophy","color",samplen(i))
  
  i=i+1
  if (i > length(muffin$color[muffin$nutstatus==2])) break()
  
}

## muffin==acceptance
E.ci.T6.acceptance<-data.frame(n=integer(),
                               food=numeric(),
                               nutstatus=numeric(),
                               variable=numeric(),
                               lower=numeric(),
                               mean=numeric(),
                               upper=numeric())

for (i in 1:160) {
  
  samplen<-function(n){
    E<-c()
    for(i in 1:500){
      x<-sample(muffin$acceptance[muffin$nutstatus==2], n, replace = F) # change the parameters for others subgroups
      theta <- function(x){mean(x)}
      results <- bootstrap(x, 1000, theta)
      E[i]<-(quantile(results$thetastar, probs=.975 ,type=3, names=F)-quantile(results$thetastar, probs=.025 ,type=3, names=F))/2
    }
    return(CI(E))
  }
  
  E.ci.T6.acceptance[i,]<-c(i,"muffin","eutrophy","acceptance",samplen(i))
  
  i=i+1
  if (i > length(muffin$acceptance[muffin$nutstatus==2])) break()
  
}


#### combining dataframes muffin
E.ci.table6.muffin.M<-rbind(E.ci.T6.appearance,E.ci.T6.aroma,E.ci.T6.taste,E.ci.T6.texture,E.ci.T6.color)

#### PANCAKE ####

## pancake==appearance
E.ci.T6.appearance<-data.frame(n=integer(),
                               food=numeric(),
                               nutstatus=numeric(),
                               variable=numeric(),
                               lower=numeric(),
                               mean=numeric(),
                               upper=numeric())

for (i in 1:160) {
  
  samplen<-function(n){
    E<-c()
    for(i in 1:500){
      x<-sample(pancake$appearance[pancake$nutstatus==2], n, replace = F) # change the parameters for others subgroups
      theta <- function(x){mean(x)}
      results <- bootstrap(x, 1000, theta)
      E[i]<-(quantile(results$thetastar, probs=.975 ,type=3, names=F)-quantile(results$thetastar, probs=.025 ,type=3, names=F))/2
    }
    return(CI(E))
  }
  
  E.ci.T6.appearance[i,]<-c(i,"pancake","eutrophy","appearance",samplen(i))
  
  i=i+1
  if (i > length(pancake$appearance[pancake$nutstatus==2])) break()
  
}

## pancake==aroma
E.ci.T6.aroma<-data.frame(n=integer(),
                          food=numeric(),
                          nutstatus=numeric(),
                          variable=numeric(),
                          lower=numeric(),
                          mean=numeric(),
                          upper=numeric())

for (i in 1:160) {
  
  samplen<-function(n){
    E<-c()
    for(i in 1:500){
      x<-sample(pancake$aroma[pancake$nutstatus==2], n, replace = F) # change the parameters for others subgroups
      theta <- function(x){mean(x)}
      results <- bootstrap(x, 1000, theta)
      E[i]<-(quantile(results$thetastar, probs=.975 ,type=3, names=F)-quantile(results$thetastar, probs=.025 ,type=3, names=F))/2
    }
    return(CI(E))
  }
  
  E.ci.T6.aroma[i,]<-c(i,"pancake","eutrophy","aroma",samplen(i))
  
  i=i+1
  if (i > length(pancake$aroma[pancake$nutstatus==2])) break()
  
}

## pancake==taste
E.ci.T6.taste<-data.frame(n=integer(),
                          food=numeric(),
                          nutstatus=numeric(),
                          variable=numeric(),
                          lower=numeric(),
                          mean=numeric(),
                          upper=numeric())

for (i in 1:160) {
  
  samplen<-function(n){
    E<-c()
    for(i in 1:500){
      x<-sample(pancake$taste[pancake$nutstatus==2], n, replace = F) # change the parameters for others subgroups
      theta <- function(x){mean(x)}
      results <- bootstrap(x, 1000, theta)
      E[i]<-(quantile(results$thetastar, probs=.975 ,type=3, names=F)-quantile(results$thetastar, probs=.025 ,type=3, names=F))/2
    }
    return(CI(E))
  }
  
  E.ci.T6.taste[i,]<-c(i,"pancake","eutrophy","taste",samplen(i))
  
  i=i+1
  if (i > length(pancake$taste[pancake$nutstatus==2])) break()
  
}

## pancake==texture
E.ci.T6.texture<-data.frame(n=integer(),
                            food=numeric(),
                            nutstatus=numeric(),
                            variable=numeric(),
                            lower=numeric(),
                            mean=numeric(),
                            upper=numeric())

for (i in 1:160) {
  
  samplen<-function(n){
    E<-c()
    for(i in 1:500){
      x<-sample(pancake$texture[pancake$nutstatus==2], n, replace = F) # change the parameters for others subgroups
      theta <- function(x){mean(x)}
      results <- bootstrap(x, 1000, theta)
      E[i]<-(quantile(results$thetastar, probs=.975 ,type=3, names=F)-quantile(results$thetastar, probs=.025 ,type=3, names=F))/2
    }
    return(CI(E))
  }
  
  E.ci.T6.texture[i,]<-c(i,"pancake","eutrophy","texture",samplen(i))
  
  i=i+1
  if (i > length(pancake$texture[pancake$nutstatus==2])) break()
  
}

## pancake==color
E.ci.T6.color<-data.frame(n=integer(),
                          food=numeric(),
                          nutstatus=numeric(),
                          variable=numeric(),
                          lower=numeric(),
                          mean=numeric(),
                          upper=numeric())

for (i in 1:160) {
  
  samplen<-function(n){
    E<-c()
    for(i in 1:500){
      x<-sample(pancake$color[pancake$nutstatus==2], n, replace = F) # change the parameters for others subgroups
      theta <- function(x){mean(x)}
      results <- bootstrap(x, 1000, theta)
      E[i]<-(quantile(results$thetastar, probs=.975 ,type=3, names=F)-quantile(results$thetastar, probs=.025 ,type=3, names=F))/2
    }
    return(CI(E))
  }
  
  E.ci.T6.color[i,]<-c(i,"pancake","eutrophy","color",samplen(i))
  
  i=i+1
  if (i > length(pancake$color[pancake$nutstatus==2])) break()
  
}

## pancake==acceptance
E.ci.T6.acceptance<-data.frame(n=integer(),
                               food=numeric(),
                               nutstatus=numeric(),
                               variable=numeric(),
                               lower=numeric(),
                               mean=numeric(),
                               upper=numeric())

for (i in 1:160) {
  
  samplen<-function(n){
    E<-c()
    for(i in 1:500){
      x<-sample(pancake$acceptance[pancake$nutstatus==2], n, replace = F) # change the parameters for others subgroups
      theta <- function(x){mean(x)}
      results <- bootstrap(x, 1000, theta)
      E[i]<-(quantile(results$thetastar, probs=.975 ,type=3, names=F)-quantile(results$thetastar, probs=.025 ,type=3, names=F))/2
    }
    return(CI(E))
  }
  
  E.ci.T6.acceptance[i,]<-c(i,"pancake","eutrophy","acceptance",samplen(i))
  
  i=i+1
  if (i > length(pancake$acceptance[pancake$nutstatus==2])) break()
  
}


#### combining dataframes pancake
E.ci.table6.pancake.M<-rbind(E.ci.T6.appearance,E.ci.T6.aroma,E.ci.T6.taste,E.ci.T6.texture,E.ci.T6.color)


#### SFIHA ####

## sfiha==appearance
E.ci.T6.appearance<-data.frame(n=integer(),
                               food=numeric(),
                               nutstatus=numeric(),
                               variable=numeric(),
                               lower=numeric(),
                               mean=numeric(),
                               upper=numeric())

for (i in 1:160) {
  
  samplen<-function(n){
    E<-c()
    for(i in 1:500){
      x<-sample(sfiha$appearance[sfiha$nutstatus==2], n, replace = F) # change the parameters for others subgroups
      theta <- function(x){mean(x)}
      results <- bootstrap(x, 1000, theta)
      E[i]<-(quantile(results$thetastar, probs=.975 ,type=3, names=F)-quantile(results$thetastar, probs=.025 ,type=3, names=F))/2
    }
    return(CI(E))
  }
  
  E.ci.T6.appearance[i,]<-c(i,"sfiha","eutrophy","appearance",samplen(i))
  
  i=i+1
  if (i > length(sfiha$appearance[sfiha$nutstatus==2])) break()
  
}

## sfiha==aroma
E.ci.T6.aroma<-data.frame(n=integer(),
                          food=numeric(),
                          nutstatus=numeric(),
                          variable=numeric(),
                          lower=numeric(),
                          mean=numeric(),
                          upper=numeric())

for (i in 1:160) {
  
  samplen<-function(n){
    E<-c()
    for(i in 1:500){
      x<-sample(sfiha$aroma[sfiha$nutstatus==2], n, replace = F) # change the parameters for others subgroups
      theta <- function(x){mean(x)}
      results <- bootstrap(x, 1000, theta)
      E[i]<-(quantile(results$thetastar, probs=.975 ,type=3, names=F)-quantile(results$thetastar, probs=.025 ,type=3, names=F))/2
    }
    return(CI(E))
  }
  
  E.ci.T6.aroma[i,]<-c(i,"sfiha","eutrophy","aroma",samplen(i))
  
  i=i+1
  if (i > length(sfiha$aroma[sfiha$nutstatus==2])) break()
  
}

## sfiha==taste
E.ci.T6.taste<-data.frame(n=integer(),
                          food=numeric(),
                          nutstatus=numeric(),
                          variable=numeric(),
                          lower=numeric(),
                          mean=numeric(),
                          upper=numeric())

for (i in 1:160) {
  
  samplen<-function(n){
    E<-c()
    for(i in 1:500){
      x<-sample(sfiha$taste[sfiha$nutstatus==2], n, replace = F) # change the parameters for others subgroups
      theta <- function(x){mean(x)}
      results <- bootstrap(x, 1000, theta)
      E[i]<-(quantile(results$thetastar, probs=.975 ,type=3, names=F)-quantile(results$thetastar, probs=.025 ,type=3, names=F))/2
    }
    return(CI(E))
  }
  
  E.ci.T6.taste[i,]<-c(i,"sfiha","eutrophy","taste",samplen(i))
  
  i=i+1
  if (i > length(sfiha$taste[sfiha$nutstatus==2])) break()
  
}

## sfiha==texture
E.ci.T6.texture<-data.frame(n=integer(),
                            food=numeric(),
                            nutstatus=numeric(),
                            variable=numeric(),
                            lower=numeric(),
                            mean=numeric(),
                            upper=numeric())

for (i in 1:160) {
  
  samplen<-function(n){
    E<-c()
    for(i in 1:500){
      x<-sample(sfiha$texture[sfiha$nutstatus==2], n, replace = F) # change the parameters for others subgroups
      theta <- function(x){mean(x)}
      results <- bootstrap(x, 1000, theta)
      E[i]<-(quantile(results$thetastar, probs=.975 ,type=3, names=F)-quantile(results$thetastar, probs=.025 ,type=3, names=F))/2
    }
    return(CI(E))
  }
  
  E.ci.T6.texture[i,]<-c(i,"sfiha","eutrophy","texture",samplen(i))
  
  i=i+1
  if (i > length(sfiha$texture[sfiha$nutstatus==2])) break()
  
}

## sfiha==color
E.ci.T6.color<-data.frame(n=integer(),
                          food=numeric(),
                          nutstatus=numeric(),
                          variable=numeric(),
                          lower=numeric(),
                          mean=numeric(),
                          upper=numeric())

for (i in 1:160) {
  
  samplen<-function(n){
    E<-c()
    for(i in 1:500){
      x<-sample(sfiha$color[sfiha$nutstatus==2], n, replace = F) # change the parameters for others subgroups
      theta <- function(x){mean(x)}
      results <- bootstrap(x, 1000, theta)
      E[i]<-(quantile(results$thetastar, probs=.975 ,type=3, names=F)-quantile(results$thetastar, probs=.025 ,type=3, names=F))/2
    }
    return(CI(E))
  }
  
  E.ci.T6.color[i,]<-c(i,"sfiha","eutrophy","color",samplen(i))
  
  i=i+1
  if (i > length(sfiha$color[sfiha$nutstatus==2])) break()
  
}

## sfiha==acceptance
E.ci.T6.acceptance<-data.frame(n=integer(),
                               food=numeric(),
                               nutstatus=numeric(),
                               variable=numeric(),
                               lower=numeric(),
                               mean=numeric(),
                               upper=numeric())

for (i in 1:160) {
  
  samplen<-function(n){
    E<-c()
    for(i in 1:500){
      x<-sample(sfiha$acceptance[sfiha$nutstatus==2], n, replace = F) # change the parameters for others subgroups
      theta <- function(x){mean(x)}
      results <- bootstrap(x, 1000, theta)
      E[i]<-(quantile(results$thetastar, probs=.975 ,type=3, names=F)-quantile(results$thetastar, probs=.025 ,type=3, names=F))/2
    }
    return(CI(E))
  }
  
  E.ci.T6.acceptance[i,]<-c(i,"sfiha","eutrophy","acceptance",samplen(i))
  
  i=i+1
  if (i > length(sfiha$acceptance[sfiha$nutstatus==2])) break()
  
}


#### combining dataframes sfiha
E.ci.table6.sfiha.E<-rbind(E.ci.T6.appearance,E.ci.T6.aroma,E.ci.T6.taste,E.ci.T6.texture,E.ci.T6.color)

E.ci.table6.eutrophy<-rbind(E.ci.table6.bread.M,
                          E.ci.table6.sfiha.M,
                          E.ci.table6.cookie.M,
                          E.ci.table6.muffin.M,
                          E.ci.table6.pancake.M)



save(E.ci.table6.eutrophy, file = "Ecitable6_eutrophy.RData")

rm(list = ls())



#################
####  Excess weight ####
#################

# subgroups nutstatus==3 (excess weight)


#### BREAD ####
## BREAD==appearance
E.ci.T6.appearance<-data.frame(n=integer(),
                               food=numeric(),
                               nutstatus=numeric(),
                               variable=numeric(),
                               lower=numeric(),
                               mean=numeric(),
                               upper=numeric())

for (i in 1:160) {
  
  samplen<-function(n){
    E<-c()
    for(i in 1:500){
      x<-sample(bread$appearance[bread$nutstatus==3], n, replace = F) # change the parameters for others subgroups
      theta <- function(x){mean(x)}
      results <- bootstrap(x, 1000, theta)
      E[i]<-(quantile(results$thetastar, probs=.975 ,type=3, names=F)-quantile(results$thetastar, probs=.025 ,type=3, names=F))/2
    }
    return(CI(E))
  }
  
  E.ci.T6.appearance[i,]<-c(i,"bread","excess_weight","appearance",samplen(i))
  
  i=i+1
  if (i > length(bread$appearance[bread$nutstatus==3])) break()
  
}

## BREAD==aroma
E.ci.T6.aroma<-data.frame(n=integer(),
                          food=numeric(),
                          nutstatus=numeric(),
                          variable=numeric(),
                          lower=numeric(),
                          mean=numeric(),
                          upper=numeric())

for (i in 1:160) {
  
  samplen<-function(n){
    E<-c()
    for(i in 1:500){
      x<-sample(bread$aroma[bread$nutstatus==3], n, replace = F) # change the parameters for others subgroups
      theta <- function(x){mean(x)}
      results <- bootstrap(x, 1000, theta)
      E[i]<-(quantile(results$thetastar, probs=.975 ,type=3, names=F)-quantile(results$thetastar, probs=.025 ,type=3, names=F))/2
    }
    return(CI(E))
  }
  
  E.ci.T6.aroma[i,]<-c(i,"bread","excess_weight","aroma",samplen(i))
  
  i=i+1
  if (i > length(bread$aroma[bread$nutstatus==3])) break()
  
}

## BREAD==taste
E.ci.T6.taste<-data.frame(n=integer(),
                          food=numeric(),
                          nutstatus=numeric(),
                          variable=numeric(),
                          lower=numeric(),
                          mean=numeric(),
                          upper=numeric())

for (i in 1:160) {
  
  samplen<-function(n){
    E<-c()
    for(i in 1:500){
      x<-sample(bread$taste[bread$nutstatus==3], n, replace = F) # change the parameters for others subgroups
      theta <- function(x){mean(x)}
      results <- bootstrap(x, 1000, theta)
      E[i]<-(quantile(results$thetastar, probs=.975 ,type=3, names=F)-quantile(results$thetastar, probs=.025 ,type=3, names=F))/2
    }
    return(CI(E))
  }
  
  E.ci.T6.taste[i,]<-c(i,"bread","excess_weight","taste",samplen(i))
  
  i=i+1
  if (i > length(bread$taste[bread$nutstatus==3])) break()
  
}

## BREAD==texture
E.ci.T6.texture<-data.frame(n=integer(),
                            food=numeric(),
                            nutstatus=numeric(),
                            variable=numeric(),
                            lower=numeric(),
                            mean=numeric(),
                            upper=numeric())

for (i in 1:160) {
  
  samplen<-function(n){
    E<-c()
    for(i in 1:500){
      x<-sample(bread$texture[bread$nutstatus==3], n, replace = F) # change the parameters for others subgroups
      theta <- function(x){mean(x)}
      results <- bootstrap(x, 1000, theta)
      E[i]<-(quantile(results$thetastar, probs=.975 ,type=3, names=F)-quantile(results$thetastar, probs=.025 ,type=3, names=F))/2
    }
    return(CI(E))
  }
  
  E.ci.T6.texture[i,]<-c(i,"bread","excess_weight","texture",samplen(i))
  
  i=i+1
  if (i > length(bread$texture[bread$nutstatus==3])) break()
  
}

## BREAD==color
E.ci.T6.color<-data.frame(n=integer(),
                          food=numeric(),
                          nutstatus=numeric(),
                          variable=numeric(),
                          lower=numeric(),
                          mean=numeric(),
                          upper=numeric())

for (i in 1:160) {
  
  samplen<-function(n){
    E<-c()
    for(i in 1:500){
      x<-sample(bread$color[bread$nutstatus==3], n, replace = F) # change the parameters for others subgroups
      theta <- function(x){mean(x)}
      results <- bootstrap(x, 1000, theta)
      E[i]<-(quantile(results$thetastar, probs=.975 ,type=3, names=F)-quantile(results$thetastar, probs=.025 ,type=3, names=F))/2
    }
    return(CI(E))
  }
  
  E.ci.T6.color[i,]<-c(i,"bread","excess_weight","color",samplen(i))
  
  i=i+1
  if (i > length(bread$color[bread$nutstatus==3])) break()
  
}

## BREAD==acceptance
E.ci.T6.acceptance<-data.frame(n=integer(),
                               food=numeric(),
                               nutstatus=numeric(),
                               variable=numeric(),
                               lower=numeric(),
                               mean=numeric(),
                               upper=numeric())

for (i in 1:160) {
  
  samplen<-function(n){
    E<-c()
    for(i in 1:500){
      x<-sample(bread$acceptance[bread$nutstatus==3], n, replace = F) # change the parameters for others subgroups
      theta <- function(x){mean(x)}
      results <- bootstrap(x, 1000, theta)
      E[i]<-(quantile(results$thetastar, probs=.975 ,type=3, names=F)-quantile(results$thetastar, probs=.025 ,type=3, names=F))/2
    }
    return(CI(E))
  }
  
  E.ci.T6.acceptance[i,]<-c(i,"bread","excess_weight","acceptance",samplen(i))
  
  i=i+1
  if (i > length(bread$acceptance[bread$nutstatus==3])) break()
  
}


#### combining dataframes bread
E.ci.table6.bread.M<-rbind(E.ci.T6.appearance,E.ci.T6.aroma,E.ci.T6.taste,E.ci.T6.texture,E.ci.T6.color)



####COOKIE####
## cookie==appearance
E.ci.T6.appearance<-data.frame(n=integer(),
                               food=numeric(),
                               nutstatus=numeric(),
                               variable=numeric(),
                               lower=numeric(),
                               mean=numeric(),
                               upper=numeric())

for (i in 1:160) {
  
  samplen<-function(n){
    E<-c()
    for(i in 1:500){
      x<-sample(cookie$appearance[cookie$nutstatus==3], n, replace = F) # change the parameters for others subgroups
      theta <- function(x){mean(x)}
      results <- bootstrap(x, 1000, theta)
      E[i]<-(quantile(results$thetastar, probs=.975 ,type=3, names=F)-quantile(results$thetastar, probs=.025 ,type=3, names=F))/2
    }
    return(CI(E))
  }
  
  E.ci.T6.appearance[i,]<-c(i,"cookie","eutrophy","appearance",samplen(i))
  
  i=i+1
  if (i > length(cookie$appearance[cookie$nutstatus==3])) break()
  
}

## cookie==aroma
E.ci.T6.aroma<-data.frame(n=integer(),
                          food=numeric(),
                          nutstatus=numeric(),
                          variable=numeric(),
                          lower=numeric(),
                          mean=numeric(),
                          upper=numeric())

for (i in 1:160) {
  
  samplen<-function(n){
    E<-c()
    for(i in 1:500){
      x<-sample(cookie$aroma[cookie$nutstatus==3], n, replace = F) # change the parameters for others subgroups
      theta <- function(x){mean(x)}
      results <- bootstrap(x, 1000, theta)
      E[i]<-(quantile(results$thetastar, probs=.975 ,type=3, names=F)-quantile(results$thetastar, probs=.025 ,type=3, names=F))/2
    }
    return(CI(E))
  }
  
  E.ci.T6.aroma[i,]<-c(i,"cookie","excess_weight","aroma",samplen(i))
  
  i=i+1
  if (i > length(cookie$aroma[cookie$nutstatus==3])) break()
  
}

## cookie==taste
E.ci.T6.taste<-data.frame(n=integer(),
                          food=numeric(),
                          nutstatus=numeric(),
                          variable=numeric(),
                          lower=numeric(),
                          mean=numeric(),
                          upper=numeric())

for (i in 1:160) {
  
  samplen<-function(n){
    E<-c()
    for(i in 1:500){
      x<-sample(cookie$taste[cookie$nutstatus==3], n, replace = F) # change the parameters for others subgroups
      theta <- function(x){mean(x)}
      results <- bootstrap(x, 1000, theta)
      E[i]<-(quantile(results$thetastar, probs=.975 ,type=3, names=F)-quantile(results$thetastar, probs=.025 ,type=3, names=F))/2
    }
    return(CI(E))
  }
  
  E.ci.T6.taste[i,]<-c(i,"cookie","excess_weight","taste",samplen(i))
  
  i=i+1
  if (i > length(cookie$taste[cookie$nutstatus==3])) break()
  
}

## cookie==texture
E.ci.T6.texture<-data.frame(n=integer(),
                            food=numeric(),
                            nutstatus=numeric(),
                            variable=numeric(),
                            lower=numeric(),
                            mean=numeric(),
                            upper=numeric())

for (i in 1:160) {
  
  samplen<-function(n){
    E<-c()
    for(i in 1:500){
      x<-sample(cookie$texture[cookie$nutstatus==3], n, replace = F) # change the parameters for others subgroups
      theta <- function(x){mean(x)}
      results <- bootstrap(x, 1000, theta)
      E[i]<-(quantile(results$thetastar, probs=.975 ,type=3, names=F)-quantile(results$thetastar, probs=.025 ,type=3, names=F))/2
    }
    return(CI(E))
  }
  
  E.ci.T6.texture[i,]<-c(i,"cookie","excess_weight","texture",samplen(i))
  
  i=i+1
  if (i > length(cookie$texture[cookie$nutstatus==3])) break()
  
}

## cookie==color
E.ci.T6.color<-data.frame(n=integer(),
                          food=numeric(),
                          nutstatus=numeric(),
                          variable=numeric(),
                          lower=numeric(),
                          mean=numeric(),
                          upper=numeric())

for (i in 1:160) {
  
  samplen<-function(n){
    E<-c()
    for(i in 1:500){
      x<-sample(cookie$color[cookie$nutstatus==3], n, replace = F) # change the parameters for others subgroups
      theta <- function(x){mean(x)}
      results <- bootstrap(x, 1000, theta)
      E[i]<-(quantile(results$thetastar, probs=.975 ,type=3, names=F)-quantile(results$thetastar, probs=.025 ,type=3, names=F))/2
    }
    return(CI(E))
  }
  
  E.ci.T6.color[i,]<-c(i,"cookie","excess_weight","color",samplen(i))
  
  i=i+1
  if (i > length(cookie$color[cookie$nutstatus==3])) break()
  
}

## cookie==acceptance
E.ci.T6.acceptance<-data.frame(n=integer(),
                               food=numeric(),
                               nutstatus=numeric(),
                               variable=numeric(),
                               lower=numeric(),
                               mean=numeric(),
                               upper=numeric())

for (i in 1:160) {
  
  samplen<-function(n){
    E<-c()
    for(i in 1:500){
      x<-sample(cookie$acceptance[cookie$nutstatus==3], n, replace = F) # change the parameters for others subgroups
      theta <- function(x){mean(x)}
      results <- bootstrap(x, 1000, theta)
      E[i]<-(quantile(results$thetastar, probs=.975 ,type=3, names=F)-quantile(results$thetastar, probs=.025 ,type=3, names=F))/2
    }
    return(CI(E))
  }
  
  E.ci.T6.acceptance[i,]<-c(i,"cookie","excess_weight","acceptance",samplen(i))
  
  i=i+1
  if (i > length(cookie$acceptance[cookie$nutstatus==3])) break()
  
}


#### combining dataframes cookie
E.ci.table6.cookie.M<-rbind(E.ci.T6.appearance,E.ci.T6.aroma,E.ci.T6.taste,E.ci.T6.texture,E.ci.T6.color)

#### MUFFIN ####
## muffin==appearance
E.ci.T6.appearance<-data.frame(n=integer(),
                               food=numeric(),
                               nutstatus=numeric(),
                               variable=numeric(),
                               lower=numeric(),
                               mean=numeric(),
                               upper=numeric())

for (i in 1:160) {
  
  samplen<-function(n){
    E<-c()
    for(i in 1:500){
      x<-sample(muffin$appearance[muffin$nutstatus==3], n, replace = F) # change the parameters for others subgroups
      theta <- function(x){mean(x)}
      results <- bootstrap(x, 1000, theta)
      E[i]<-(quantile(results$thetastar, probs=.975 ,type=3, names=F)-quantile(results$thetastar, probs=.025 ,type=3, names=F))/2
    }
    return(CI(E))
  }
  
  E.ci.T6.appearance[i,]<-c(i,"muffin","excess_weight","appearance",samplen(i))
  
  i=i+1
  if (i > length(muffin$appearance[muffin$nutstatus==3])) break()
  
}

## muffin==aroma
E.ci.T6.aroma<-data.frame(n=integer(),
                          food=numeric(),
                          nutstatus=numeric(),
                          variable=numeric(),
                          lower=numeric(),
                          mean=numeric(),
                          upper=numeric())

for (i in 1:160) {
  
  samplen<-function(n){
    E<-c()
    for(i in 1:500){
      x<-sample(muffin$aroma[muffin$nutstatus==3], n, replace = F) # change the parameters for others subgroups
      theta <- function(x){mean(x)}
      results <- bootstrap(x, 1000, theta)
      E[i]<-(quantile(results$thetastar, probs=.975 ,type=3, names=F)-quantile(results$thetastar, probs=.025 ,type=3, names=F))/2
    }
    return(CI(E))
  }
  
  E.ci.T6.aroma[i,]<-c(i,"muffin","excess_weight","aroma",samplen(i))
  
  i=i+1
  if (i > length(muffin$aroma[muffin$nutstatus==3])) break()
  
}

## muffin==taste
E.ci.T6.taste<-data.frame(n=integer(),
                          food=numeric(),
                          nutstatus=numeric(),
                          variable=numeric(),
                          lower=numeric(),
                          mean=numeric(),
                          upper=numeric())

for (i in 1:160) {
  
  samplen<-function(n){
    E<-c()
    for(i in 1:500){
      x<-sample(muffin$taste[muffin$nutstatus==3], n, replace = F) # change the parameters for others subgroups
      theta <- function(x){mean(x)}
      results <- bootstrap(x, 1000, theta)
      E[i]<-(quantile(results$thetastar, probs=.975 ,type=3, names=F)-quantile(results$thetastar, probs=.025 ,type=3, names=F))/2
    }
    return(CI(E))
  }
  
  E.ci.T6.taste[i,]<-c(i,"muffin","excess_weight","taste",samplen(i))
  
  i=i+1
  if (i > length(muffin$taste[muffin$nutstatus==3])) break()
  
}

## muffin==texture
E.ci.T6.texture<-data.frame(n=integer(),
                            food=numeric(),
                            nutstatus=numeric(),
                            variable=numeric(),
                            lower=numeric(),
                            mean=numeric(),
                            upper=numeric())

for (i in 1:160) {
  
  samplen<-function(n){
    E<-c()
    for(i in 1:500){
      x<-sample(muffin$texture[muffin$nutstatus==3], n, replace = F) # change the parameters for others subgroups
      theta <- function(x){mean(x)}
      results <- bootstrap(x, 1000, theta)
      E[i]<-(quantile(results$thetastar, probs=.975 ,type=3, names=F)-quantile(results$thetastar, probs=.025 ,type=3, names=F))/2
    }
    return(CI(E))
  }
  
  E.ci.T6.texture[i,]<-c(i,"muffin","excess_weight","texture",samplen(i))
  
  i=i+1
  if (i > length(muffin$texture[muffin$nutstatus==3])) break()
  
}

## muffin==color
E.ci.T6.color<-data.frame(n=integer(),
                          food=numeric(),
                          nutstatus=numeric(),
                          variable=numeric(),
                          lower=numeric(),
                          mean=numeric(),
                          upper=numeric())

for (i in 1:160) {
  
  samplen<-function(n){
    E<-c()
    for(i in 1:500){
      x<-sample(muffin$color[muffin$nutstatus==3], n, replace = F) # change the parameters for others subgroups
      theta <- function(x){mean(x)}
      results <- bootstrap(x, 1000, theta)
      E[i]<-(quantile(results$thetastar, probs=.975 ,type=3, names=F)-quantile(results$thetastar, probs=.025 ,type=3, names=F))/2
    }
    return(CI(E))
  }
  
  E.ci.T6.color[i,]<-c(i,"muffin","excess_weight","color",samplen(i))
  
  i=i+1
  if (i > length(muffin$color[muffin$nutstatus==3])) break()
  
}

## muffin==acceptance
E.ci.T6.acceptance<-data.frame(n=integer(),
                               food=numeric(),
                               nutstatus=numeric(),
                               variable=numeric(),
                               lower=numeric(),
                               mean=numeric(),
                               upper=numeric())

for (i in 1:160) {
  
  samplen<-function(n){
    E<-c()
    for(i in 1:500){
      x<-sample(muffin$acceptance[muffin$nutstatus==3], n, replace = F) # change the parameters for others subgroups
      theta <- function(x){mean(x)}
      results <- bootstrap(x, 1000, theta)
      E[i]<-(quantile(results$thetastar, probs=.975 ,type=3, names=F)-quantile(results$thetastar, probs=.025 ,type=3, names=F))/2
    }
    return(CI(E))
  }
  
  E.ci.T6.acceptance[i,]<-c(i,"muffin","excess_weight","acceptance",samplen(i))
  
  i=i+1
  if (i > length(muffin$acceptance[muffin$nutstatus==3])) break()
  
}


#### combining dataframes muffin
E.ci.table6.muffin.M<-rbind(E.ci.T6.appearance,E.ci.T6.aroma,E.ci.T6.taste,E.ci.T6.texture,E.ci.T6.color)

#### PANCAKE ####

## pancake==appearance
E.ci.T6.appearance<-data.frame(n=integer(),
                               food=numeric(),
                               nutstatus=numeric(),
                               variable=numeric(),
                               lower=numeric(),
                               mean=numeric(),
                               upper=numeric())

for (i in 1:160) {
  
  samplen<-function(n){
    E<-c()
    for(i in 1:500){
      x<-sample(pancake$appearance[pancake$nutstatus==3], n, replace = F) # change the parameters for others subgroups
      theta <- function(x){mean(x)}
      results <- bootstrap(x, 1000, theta)
      E[i]<-(quantile(results$thetastar, probs=.975 ,type=3, names=F)-quantile(results$thetastar, probs=.025 ,type=3, names=F))/2
    }
    return(CI(E))
  }
  
  E.ci.T6.appearance[i,]<-c(i,"pancake","excess_weight","appearance",samplen(i))
  
  i=i+1
  if (i > length(pancake$appearance[pancake$nutstatus==3])) break()
  
}

## pancake==aroma
E.ci.T6.aroma<-data.frame(n=integer(),
                          food=numeric(),
                          nutstatus=numeric(),
                          variable=numeric(),
                          lower=numeric(),
                          mean=numeric(),
                          upper=numeric())

for (i in 1:160) {
  
  samplen<-function(n){
    E<-c()
    for(i in 1:500){
      x<-sample(pancake$aroma[pancake$nutstatus==3], n, replace = F) # change the parameters for others subgroups
      theta <- function(x){mean(x)}
      results <- bootstrap(x, 1000, theta)
      E[i]<-(quantile(results$thetastar, probs=.975 ,type=3, names=F)-quantile(results$thetastar, probs=.025 ,type=3, names=F))/2
    }
    return(CI(E))
  }
  
  E.ci.T6.aroma[i,]<-c(i,"pancake","excess_weight","aroma",samplen(i))
  
  i=i+1
  if (i > length(pancake$aroma[pancake$nutstatus==3])) break()
  
}

## pancake==taste
E.ci.T6.taste<-data.frame(n=integer(),
                          food=numeric(),
                          nutstatus=numeric(),
                          variable=numeric(),
                          lower=numeric(),
                          mean=numeric(),
                          upper=numeric())

for (i in 1:160) {
  
  samplen<-function(n){
    E<-c()
    for(i in 1:500){
      x<-sample(pancake$taste[pancake$nutstatus==3], n, replace = F) # change the parameters for others subgroups
      theta <- function(x){mean(x)}
      results <- bootstrap(x, 1000, theta)
      E[i]<-(quantile(results$thetastar, probs=.975 ,type=3, names=F)-quantile(results$thetastar, probs=.025 ,type=3, names=F))/2
    }
    return(CI(E))
  }
  
  E.ci.T6.taste[i,]<-c(i,"pancake","excess_weight","taste",samplen(i))
  
  i=i+1
  if (i > length(pancake$taste[pancake$nutstatus==3])) break()
  
}

## pancake==texture
E.ci.T6.texture<-data.frame(n=integer(),
                            food=numeric(),
                            nutstatus=numeric(),
                            variable=numeric(),
                            lower=numeric(),
                            mean=numeric(),
                            upper=numeric())

for (i in 1:160) {
  
  samplen<-function(n){
    E<-c()
    for(i in 1:500){
      x<-sample(pancake$texture[pancake$nutstatus==3], n, replace = F) # change the parameters for others subgroups
      theta <- function(x){mean(x)}
      results <- bootstrap(x, 1000, theta)
      E[i]<-(quantile(results$thetastar, probs=.975 ,type=3, names=F)-quantile(results$thetastar, probs=.025 ,type=3, names=F))/2
    }
    return(CI(E))
  }
  
  E.ci.T6.texture[i,]<-c(i,"pancake","excess_weight","texture",samplen(i))
  
  i=i+1
  if (i > length(pancake$texture[pancake$nutstatus==3])) break()
  
}

## pancake==color
E.ci.T6.color<-data.frame(n=integer(),
                          food=numeric(),
                          nutstatus=numeric(),
                          variable=numeric(),
                          lower=numeric(),
                          mean=numeric(),
                          upper=numeric())

for (i in 1:160) {
  
  samplen<-function(n){
    E<-c()
    for(i in 1:500){
      x<-sample(pancake$color[pancake$nutstatus==3], n, replace = F) # change the parameters for others subgroups
      theta <- function(x){mean(x)}
      results <- bootstrap(x, 1000, theta)
      E[i]<-(quantile(results$thetastar, probs=.975 ,type=3, names=F)-quantile(results$thetastar, probs=.025 ,type=3, names=F))/2
    }
    return(CI(E))
  }
  
  E.ci.T6.color[i,]<-c(i,"pancake","excess_weight","color",samplen(i))
  
  i=i+1
  if (i > length(pancake$color[pancake$nutstatus==3])) break()
  
}

## pancake==acceptance
E.ci.T6.acceptance<-data.frame(n=integer(),
                               food=numeric(),
                               nutstatus=numeric(),
                               variable=numeric(),
                               lower=numeric(),
                               mean=numeric(),
                               upper=numeric())

for (i in 1:160) {
  
  samplen<-function(n){
    E<-c()
    for(i in 1:500){
      x<-sample(pancake$acceptance[pancake$nutstatus==3], n, replace = F) # change the parameters for others subgroups
      theta <- function(x){mean(x)}
      results <- bootstrap(x, 1000, theta)
      E[i]<-(quantile(results$thetastar, probs=.975 ,type=3, names=F)-quantile(results$thetastar, probs=.025 ,type=3, names=F))/2
    }
    return(CI(E))
  }
  
  E.ci.T6.acceptance[i,]<-c(i,"pancake","excess_weight","acceptance",samplen(i))
  
  i=i+1
  if (i > length(pancake$acceptance[pancake$nutstatus==3])) break()
  
}


#### combining dataframes pancake
E.ci.table6.pancake.M<-rbind(E.ci.T6.appearance,E.ci.T6.aroma,E.ci.T6.taste,E.ci.T6.texture,E.ci.T6.color)


#### SFIHA ####

## sfiha==appearance
E.ci.T6.appearance<-data.frame(n=integer(),
                               food=numeric(),
                               nutstatus=numeric(),
                               variable=numeric(),
                               lower=numeric(),
                               mean=numeric(),
                               upper=numeric())

for (i in 1:160) {
  
  samplen<-function(n){
    E<-c()
    for(i in 1:500){
      x<-sample(sfiha$appearance[sfiha$nutstatus==3], n, replace = F) # change the parameters for others subgroups
      theta <- function(x){mean(x)}
      results <- bootstrap(x, 1000, theta)
      E[i]<-(quantile(results$thetastar, probs=.975 ,type=3, names=F)-quantile(results$thetastar, probs=.025 ,type=3, names=F))/2
    }
    return(CI(E))
  }
  
  E.ci.T6.appearance[i,]<-c(i,"sfiha","excess_weight","appearance",samplen(i))
  
  i=i+1
  if (i > length(sfiha$appearance[sfiha$nutstatus==3])) break()
  
}

## sfiha==aroma
E.ci.T6.aroma<-data.frame(n=integer(),
                          food=numeric(),
                          nutstatus=numeric(),
                          variable=numeric(),
                          lower=numeric(),
                          mean=numeric(),
                          upper=numeric())

for (i in 1:160) {
  
  samplen<-function(n){
    E<-c()
    for(i in 1:500){
      x<-sample(sfiha$aroma[sfiha$nutstatus==3], n, replace = F) # change the parameters for others subgroups
      theta <- function(x){mean(x)}
      results <- bootstrap(x, 1000, theta)
      E[i]<-(quantile(results$thetastar, probs=.975 ,type=3, names=F)-quantile(results$thetastar, probs=.025 ,type=3, names=F))/2
    }
    return(CI(E))
  }
  
  E.ci.T6.aroma[i,]<-c(i,"sfiha","excess_weight","aroma",samplen(i))
  
  i=i+1
  if (i > length(sfiha$aroma[sfiha$nutstatus==3])) break()
  
}

## sfiha==taste
E.ci.T6.taste<-data.frame(n=integer(),
                          food=numeric(),
                          nutstatus=numeric(),
                          variable=numeric(),
                          lower=numeric(),
                          mean=numeric(),
                          upper=numeric())

for (i in 1:160) {
  
  samplen<-function(n){
    E<-c()
    for(i in 1:500){
      x<-sample(sfiha$taste[sfiha$nutstatus==3], n, replace = F) # change the parameters for others subgroups
      theta <- function(x){mean(x)}
      results <- bootstrap(x, 1000, theta)
      E[i]<-(quantile(results$thetastar, probs=.975 ,type=3, names=F)-quantile(results$thetastar, probs=.025 ,type=3, names=F))/2
    }
    return(CI(E))
  }
  
  E.ci.T6.taste[i,]<-c(i,"sfiha","excess_weight","taste",samplen(i))
  
  i=i+1
  if (i > length(sfiha$taste[sfiha$nutstatus==3])) break()
  
}

## sfiha==texture
E.ci.T6.texture<-data.frame(n=integer(),
                            food=numeric(),
                            nutstatus=numeric(),
                            variable=numeric(),
                            lower=numeric(),
                            mean=numeric(),
                            upper=numeric())

for (i in 1:160) {
  
  samplen<-function(n){
    E<-c()
    for(i in 1:500){
      x<-sample(sfiha$texture[sfiha$nutstatus==3], n, replace = F) # change the parameters for others subgroups
      theta <- function(x){mean(x)}
      results <- bootstrap(x, 1000, theta)
      E[i]<-(quantile(results$thetastar, probs=.975 ,type=3, names=F)-quantile(results$thetastar, probs=.025 ,type=3, names=F))/2
    }
    return(CI(E))
  }
  
  E.ci.T6.texture[i,]<-c(i,"sfiha","excess_weight","texture",samplen(i))
  
  i=i+1
  if (i > length(sfiha$texture[sfiha$nutstatus==3])) break()
  
}

## sfiha==color
E.ci.T6.color<-data.frame(n=integer(),
                          food=numeric(),
                          nutstatus=numeric(),
                          variable=numeric(),
                          lower=numeric(),
                          mean=numeric(),
                          upper=numeric())

for (i in 1:160) {
  
  samplen<-function(n){
    E<-c()
    for(i in 1:500){
      x<-sample(sfiha$color[sfiha$nutstatus==3], n, replace = F) # change the parameters for others subgroups
      theta <- function(x){mean(x)}
      results <- bootstrap(x, 1000, theta)
      E[i]<-(quantile(results$thetastar, probs=.975 ,type=3, names=F)-quantile(results$thetastar, probs=.025 ,type=3, names=F))/2
    }
    return(CI(E))
  }
  
  E.ci.T6.color[i,]<-c(i,"sfiha","excess_weight","color",samplen(i))
  
  i=i+1
  if (i > length(sfiha$color[sfiha$nutstatus==3])) break()
  
}

## sfiha==acceptance
E.ci.T6.acceptance<-data.frame(n=integer(),
                               food=numeric(),
                               nutstatus=numeric(),
                               variable=numeric(),
                               lower=numeric(),
                               mean=numeric(),
                               upper=numeric())

for (i in 1:160) {
  
  samplen<-function(n){
    E<-c()
    for(i in 1:500){
      x<-sample(sfiha$acceptance[sfiha$nutstatus==3], n, replace = F) # change the parameters for others subgroups
      theta <- function(x){mean(x)}
      results <- bootstrap(x, 1000, theta)
      E[i]<-(quantile(results$thetastar, probs=.975 ,type=3, names=F)-quantile(results$thetastar, probs=.025 ,type=3, names=F))/2
    }
    return(CI(E))
  }
  
  E.ci.T6.acceptance[i,]<-c(i,"sfiha","excess_weight","acceptance",samplen(i))
  
  i=i+1
  if (i > length(sfiha$acceptance[sfiha$nutstatus==3])) break()
  
}


#### combining dataframes sfiha
E.ci.table6.sfiha.M<-rbind(E.ci.T6.appearance,E.ci.T6.aroma,E.ci.T6.taste,E.ci.T6.texture,E.ci.T6.color,E.ci.T6.acceptance)

E.ci.table6.ExcessWeight<-rbind(E.ci.table6.bread.M,
                          E.ci.table6.sfiha.M,
                          E.ci.table6.cookie.M,
                          E.ci.table6.muffin.M,
                          E.ci.table6.pancake.M)



save(E.ci.table6.ExcessWeight, file = "EciTable6_ExcessWeight.RData")

rm(list = ls())
