# Function in R to estimate the confidence interval of the admissible error (E).
# Created by: Daiana Novello, Adilson dos Anjosb
# Paper: Number of children needed to evaluate products made in cooking workshops
# TABLE 4

library(Rmisc)
library(bootstrap)


# Load data
load("bread.RData")
load("cookie.RData")
load("muffin.RData")
load("pancake.RData")
load("sfiha.RData")

# subgroups gender==1 (male)
# subgroups gender==2 (female)

#### BREAD ####
## BREAD==appearance
E.ci.T4.appearance<-data.frame(n=integer(),
                          food=numeric(),
                          variable=numeric(),
                          lower=numeric(),
                          mean=numeric(),
                          upper=numeric())

for (i in 1:300) {
  
  samplen<-function(n){
    E<-c()
    for(i in 1:500){
      x<-sample(bread$appearance, n, replace = F) # change the parameters for others subgroups
      theta <- function(x){mean(x)}
      results <- bootstrap(x, 1000, theta)
      E[i]<-(quantile(results$thetastar, probs=.975 ,type=3, names=F)-quantile(results$thetastar, probs=.025 ,type=3, names=F))/2
    }
    return(CI(E))
  }
  
  E.ci.T4.appearance[i,]<-c(i,"bread","appearance",samplen(i))
  
  i=i+1
  if (i > length(bread$appearance)) break()
  
}

## BREAD==aroma
E.ci.T4.aroma<-data.frame(n=integer(),
                               food=numeric(),
                               variable=numeric(),
                               lower=numeric(),
                               mean=numeric(),
                               upper=numeric())

for (i in 1:300) {
  
  samplen<-function(n){
    E<-c()
    for(i in 1:500){
      x<-sample(bread$aroma, n, replace = F) # change the parameters for others subgroups
      theta <- function(x){mean(x)}
      results <- bootstrap(x, 1000, theta)
      E[i]<-(quantile(results$thetastar, probs=.975 ,type=3, names=F)-quantile(results$thetastar, probs=.025 ,type=3, names=F))/2
    }
    return(CI(E))
  }
  
  E.ci.T4.aroma[i,]<-c(i,"bread","aroma",samplen(i))
  
  i=i+1
  if (i > length(bread$aroma)) break()
  
}

## BREAD==taste
E.ci.T4.taste<-data.frame(n=integer(),
                          food=numeric(),
                          variable=numeric(),
                          lower=numeric(),
                          mean=numeric(),
                          upper=numeric())

for (i in 1:300) {
  
  samplen<-function(n){
    E<-c()
    for(i in 1:500){
      x<-sample(bread$taste, n, replace = F) # change the parameters for others subgroups
      theta <- function(x){mean(x)}
      results <- bootstrap(x, 1000, theta)
      E[i]<-(quantile(results$thetastar, probs=.975 ,type=3, names=F)-quantile(results$thetastar, probs=.025 ,type=3, names=F))/2
    }
    return(CI(E))
  }
  
  E.ci.T4.taste[i,]<-c(i,"bread","taste",samplen(i))
  
  i=i+1
  if (i > length(bread$taste)) break()
  
}

## BREAD==texture
E.ci.T4.texture<-data.frame(n=integer(),
                          food=numeric(),
                          variable=numeric(),
                          lower=numeric(),
                          mean=numeric(),
                          upper=numeric())

for (i in 1:300) {
  
  samplen<-function(n){
    E<-c()
    for(i in 1:500){
      x<-sample(bread$texture, n, replace = F) # change the parameters for others subgroups
      theta <- function(x){mean(x)}
      results <- bootstrap(x, 1000, theta)
      E[i]<-(quantile(results$thetastar, probs=.975 ,type=3, names=F)-quantile(results$thetastar, probs=.025 ,type=3, names=F))/2
    }
    return(CI(E))
  }
  
  E.ci.T4.texture[i,]<-c(i,"bread","texture",samplen(i))
  
  i=i+1
  if (i > length(bread$texture)) break()
  
}

## BREAD==color
E.ci.T4.color<-data.frame(n=integer(),
                   food=numeric(),
                   variable=numeric(),
                   lower=numeric(),
                   mean=numeric(),
                   upper=numeric())

for (i in 1:300) {
  
  samplen<-function(n){
    E<-c()
    for(i in 1:500){
      x<-sample(bread$color, n, replace = F) # change the parameters for others subgroups
      theta <- function(x){mean(x)}
      results <- bootstrap(x, 1000, theta)
      E[i]<-(quantile(results$thetastar, probs=.975 ,type=3, names=F)-quantile(results$thetastar, probs=.025 ,type=3, names=F))/2
    }
    return(CI(E))
  }
  
  E.ci.T4.color[i,]<-c(i,"bread","color",samplen(i))
  
  i=i+1
  if (i > length(bread$color)) break()
  
}

## BREAD==acceptance
E.ci.T4.acceptance<-data.frame(n=integer(),
                          food=numeric(),
                          variable=numeric(),
                          lower=numeric(),
                          mean=numeric(),
                          upper=numeric())

for (i in 1:300) {
  
  samplen<-function(n){
    E<-c()
    for(i in 1:500){
      x<-sample(bread$acceptance, n, replace = F) # change the parameters for others subgroups
      theta <- function(x){mean(x)}
      results <- bootstrap(x, 1000, theta)
      E[i]<-(quantile(results$thetastar, probs=.975 ,type=3, names=F)-quantile(results$thetastar, probs=.025 ,type=3, names=F))/2
    }
    return(CI(E))
  }
  
  E.ci.T4.acceptance[i,]<-c(i,"bread","acceptance",samplen(i))
  
  i=i+1
  if (i > length(bread$acceptance)) break()
  
}


#### combining dataframes bread
E.ci.table4.bread<-rbind(E.ci.T4.appearance,E.ci.T4.aroma,E.ci.T4.taste,E.ci.T4.texture,E.ci.T4.color,E.ci.T4.acceptance)


#### COOKIE ####


## cookie==appearance
E.ci.T4.appearance<-data.frame(n=integer(),
                               food=numeric(),
                               variable=numeric(),
                               lower=numeric(),
                               mean=numeric(),
                               upper=numeric())

for (i in 1:300) {
  
  samplen<-function(n){
    E<-c()
    for(i in 1:500){
      x<-sample(cookie$appearance, n, replace = F) # change the parameters for others subgroups
      theta <- function(x){mean(x)}
      results <- bootstrap(x, 1000, theta)
      E[i]<-(quantile(results$thetastar, probs=.975 ,type=3, names=F)-quantile(results$thetastar, probs=.025 ,type=3, names=F))/2
    }
    return(CI(E))
  }
  
  E.ci.T4.appearance[i,]<-c(i,"cookie","appearance",samplen(i))
  
  i=i+1
  if (i > length(cookie$appearance)) break()
  
}

## cookie==aroma
E.ci.T4.aroma<-data.frame(n=integer(),
                          food=numeric(),
                          variable=numeric(),
                          lower=numeric(),
                          mean=numeric(),
                          upper=numeric())

for (i in 1:300) {
  
  samplen<-function(n){
    E<-c()
    for(i in 1:500){
      x<-sample(cookie$aroma, n, replace = F) # change the parameters for others subgroups
      theta <- function(x){mean(x)}
      results <- bootstrap(x, 1000, theta)
      E[i]<-(quantile(results$thetastar, probs=.975 ,type=3, names=F)-quantile(results$thetastar, probs=.025 ,type=3, names=F))/2
    }
    return(CI(E))
  }
  
  E.ci.T4.aroma[i,]<-c(i,"cookie","aroma",samplen(i))
  
  i=i+1
  if (i > length(cookie$aroma)) break()
  
}

## cookie==taste
E.ci.T4.taste<-data.frame(n=integer(),
                          food=numeric(),
                          variable=numeric(),
                          lower=numeric(),
                          mean=numeric(),
                          upper=numeric())

for (i in 1:300) {
  
  samplen<-function(n){
    E<-c()
    for(i in 1:500){
      x<-sample(cookie$taste, n, replace = F) # change the parameters for others subgroups
      theta <- function(x){mean(x)}
      results <- bootstrap(x, 1000, theta)
      E[i]<-(quantile(results$thetastar, probs=.975 ,type=3, names=F)-quantile(results$thetastar, probs=.025 ,type=3, names=F))/2
    }
    return(CI(E))
  }
  
  E.ci.T4.taste[i,]<-c(i,"cookie","taste",samplen(i))
  
  i=i+1
  if (i > length(cookie$taste)) break()
  
}

## cookie==texture
E.ci.T4.texture<-data.frame(n=integer(),
                            food=numeric(),
                            variable=numeric(),
                            lower=numeric(),
                            mean=numeric(),
                            upper=numeric())

for (i in 1:300) {
  
  samplen<-function(n){
    E<-c()
    for(i in 1:500){
      x<-sample(cookie$texture, n, replace = F) # change the parameters for others subgroups
      theta <- function(x){mean(x)}
      results <- bootstrap(x, 1000, theta)
      E[i]<-(quantile(results$thetastar, probs=.975 ,type=3, names=F)-quantile(results$thetastar, probs=.025 ,type=3, names=F))/2
    }
    return(CI(E))
  }
  
  E.ci.T4.texture[i,]<-c(i,"cookie","texture",samplen(i))
  
  i=i+1
  if (i > length(cookie$texture)) break()
  
}

## cookie==color
E.ci.T4.color<-data.frame(n=integer(),
                          food=numeric(),
                          variable=numeric(),
                          lower=numeric(),
                          mean=numeric(),
                          upper=numeric())

for (i in 1:300) {
  
  samplen<-function(n){
    E<-c()
    for(i in 1:500){
      x<-sample(cookie$color, n, replace = F) # change the parameters for others subgroups
      theta <- function(x){mean(x)}
      results <- bootstrap(x, 1000, theta)
      E[i]<-(quantile(results$thetastar, probs=.975 ,type=3, names=F)-quantile(results$thetastar, probs=.025 ,type=3, names=F))/2
    }
    return(CI(E))
  }
  
  E.ci.T4.color[i,]<-c(i,"cookie","color",samplen(i))
  
  i=i+1
  if (i > length(cookie$color)) break()
  
}

## cookie==acceptance
E.ci.T4.acceptance<-data.frame(n=integer(),
                               food=numeric(),
                               variable=numeric(),
                               lower=numeric(),
                               mean=numeric(),
                               upper=numeric())

for (i in 1:300) {
  
  samplen<-function(n){
    E<-c()
    for(i in 1:500){
      x<-sample(cookie$acceptance, n, replace = F) # change the parameters for others subgroups
      theta <- function(x){mean(x)}
      results <- bootstrap(x, 1000, theta)
      E[i]<-(quantile(results$thetastar, probs=.975 ,type=3, names=F)-quantile(results$thetastar, probs=.025 ,type=3, names=F))/2
    }
    return(CI(E))
  }
  
  E.ci.T4.acceptance[i,]<-c(i,"cookie","acceptance",samplen(i))
  
  i=i+1
  if (i > length(cookie$acceptance)) break()
  
}


#### combining dataframes cookie
E.ci.table4.cookie<-rbind(E.ci.T4.appearance,E.ci.T4.aroma,E.ci.T4.taste,E.ci.T4.texture,E.ci.T4.color,E.ci.T4.acceptance)


#### MUFFIN ####
## muffin==appearance
E.ci.T4.appearance<-data.frame(n=integer(),
                               food=numeric(),
                               variable=numeric(),
                               lower=numeric(),
                               mean=numeric(),
                               upper=numeric())

for (i in 1:300) {
  
  samplen<-function(n){
    E<-c()
    for(i in 1:500){
      x<-sample(muffin$appearance, n, replace = F) # change the parameters for others subgroups
      theta <- function(x){mean(x)}
      results <- bootstrap(x, 1000, theta)
      E[i]<-(quantile(results$thetastar, probs=.975 ,type=3, names=F)-quantile(results$thetastar, probs=.025 ,type=3, names=F))/2
    }
    return(CI(E))
  }
  
  E.ci.T4.appearance[i,]<-c(i,"muffin","appearance",samplen(i))
  
  i=i+1
  if (i > length(muffin$appearance)) break()
  
}

## muffin==aroma
E.ci.T4.aroma<-data.frame(n=integer(),
                          food=numeric(),
                          variable=numeric(),
                          lower=numeric(),
                          mean=numeric(),
                          upper=numeric())

for (i in 1:300) {
  
  samplen<-function(n){
    E<-c()
    for(i in 1:500){
      x<-sample(muffin$aroma, n, replace = F) # change the parameters for others subgroups
      theta <- function(x){mean(x)}
      results <- bootstrap(x, 1000, theta)
      E[i]<-(quantile(results$thetastar, probs=.975 ,type=3, names=F)-quantile(results$thetastar, probs=.025 ,type=3, names=F))/2
    }
    return(CI(E))
  }
  
  E.ci.T4.aroma[i,]<-c(i,"muffin","aroma",samplen(i))
  
  i=i+1
  if (i > length(muffin$aroma)) break()
  
}

## muffin==taste
E.ci.T4.taste<-data.frame(n=integer(),
                          food=numeric(),
                          variable=numeric(),
                          lower=numeric(),
                          mean=numeric(),
                          upper=numeric())

for (i in 1:300) {
  
  samplen<-function(n){
    E<-c()
    for(i in 1:500){
      x<-sample(muffin$taste, n, replace = F) # change the parameters for others subgroups
      theta <- function(x){mean(x)}
      results <- bootstrap(x, 1000, theta)
      E[i]<-(quantile(results$thetastar, probs=.975 ,type=3, names=F)-quantile(results$thetastar, probs=.025 ,type=3, names=F))/2
    }
    return(CI(E))
  }
  
  E.ci.T4.taste[i,]<-c(i,"muffin","taste",samplen(i))
  
  i=i+1
  if (i > length(muffin$taste)) break()
  
}

## muffin==texture
E.ci.T4.texture<-data.frame(n=integer(),
                            food=numeric(),
                            variable=numeric(),
                            lower=numeric(),
                            mean=numeric(),
                            upper=numeric())

for (i in 1:300) {
  
  samplen<-function(n){
    E<-c()
    for(i in 1:500){
      x<-sample(muffin$texture, n, replace = F) # change the parameters for others subgroups
      theta <- function(x){mean(x)}
      results <- bootstrap(x, 1000, theta)
      E[i]<-(quantile(results$thetastar, probs=.975 ,type=3, names=F)-quantile(results$thetastar, probs=.025 ,type=3, names=F))/2
    }
    return(CI(E))
  }
  
  E.ci.T4.texture[i,]<-c(i,"muffin","texture",samplen(i))
  
  i=i+1
  if (i > length(muffin$texture)) break()
  
}

## muffin==color
E.ci.T4.color<-data.frame(n=integer(),
                          food=numeric(),
                          variable=numeric(),
                          lower=numeric(),
                          mean=numeric(),
                          upper=numeric())

for (i in 1:300) {
  
  samplen<-function(n){
    E<-c()
    for(i in 1:500){
      x<-sample(muffin$color, n, replace = F) # change the parameters for others subgroups
      theta <- function(x){mean(x)}
      results <- bootstrap(x, 1000, theta)
      E[i]<-(quantile(results$thetastar, probs=.975 ,type=3, names=F)-quantile(results$thetastar, probs=.025 ,type=3, names=F))/2
    }
    return(CI(E))
  }
  
  E.ci.T4.color[i,]<-c(i,"muffin","color",samplen(i))
  
  i=i+1
  if (i > length(muffin$color)) break()
  
}

## muffin==acceptance
E.ci.T4.acceptance<-data.frame(n=integer(),
                               food=numeric(),
                               variable=numeric(),
                               lower=numeric(),
                               mean=numeric(),
                               upper=numeric())

for (i in 1:300) {
  
  samplen<-function(n){
    E<-c()
    for(i in 1:500){
      x<-sample(muffin$acceptance, n, replace = F) # change the parameters for others subgroups
      theta <- function(x){mean(x)}
      results <- bootstrap(x, 1000, theta)
      E[i]<-(quantile(results$thetastar, probs=.975 ,type=3, names=F)-quantile(results$thetastar, probs=.025 ,type=3, names=F))/2
    }
    return(CI(E))
  }
  
  E.ci.T4.acceptance[i,]<-c(i,"muffin","acceptance",samplen(i))
  
  i=i+1
  if (i > length(muffin$acceptance)) break()
  
}



#### combining dataframes muffin
E.ci.table4.muffin<-rbind(E.ci.T4.appearance,E.ci.T4.aroma,E.ci.T4.taste,E.ci.T4.texture,E.ci.T4.color,E.ci.T4.acceptance)

#### PANCAKE ####
E.ci.T4.appearance<-data.frame(n=integer(),
                               food=numeric(),
                               variable=numeric(),
                               lower=numeric(),
                               mean=numeric(),
                               upper=numeric())

for (i in 1:300) {
  
  samplen<-function(n){
    E<-c()
    for(i in 1:500){
      x<-sample(pancake$appearance, n, replace = F) # change the parameters for others subgroups
      theta <- function(x){mean(x)}
      results <- bootstrap(x, 1000, theta)
      E[i]<-(quantile(results$thetastar, probs=.975 ,type=3, names=F)-quantile(results$thetastar, probs=.025 ,type=3, names=F))/2
    }
    return(CI(E))
  }
  
  E.ci.T4.appearance[i,]<-c(i,"pancake","appearance",samplen(i))
  
  i=i+1
  if (i > length(pancake$appearance)) break()
  
}

## pancake==aroma
E.ci.T4.aroma<-data.frame(n=integer(),
                          food=numeric(),
                          variable=numeric(),
                          lower=numeric(),
                          mean=numeric(),
                          upper=numeric())

for (i in 1:300) {
  
  samplen<-function(n){
    E<-c()
    for(i in 1:500){
      x<-sample(pancake$aroma, n, replace = F) # change the parameters for others subgroups
      theta <- function(x){mean(x)}
      results <- bootstrap(x, 1000, theta)
      E[i]<-(quantile(results$thetastar, probs=.975 ,type=3, names=F)-quantile(results$thetastar, probs=.025 ,type=3, names=F))/2
    }
    return(CI(E))
  }
  
  E.ci.T4.aroma[i,]<-c(i,"pancake","aroma",samplen(i))
  
  i=i+1
  if (i > length(pancake$aroma)) break()
  
}

## pancake==taste
E.ci.T4.taste<-data.frame(n=integer(),
                          food=numeric(),
                          variable=numeric(),
                          lower=numeric(),
                          mean=numeric(),
                          upper=numeric())

for (i in 1:300) {
  
  samplen<-function(n){
    E<-c()
    for(i in 1:500){
      x<-sample(pancake$taste, n, replace = F) # change the parameters for others subgroups
      theta <- function(x){mean(x)}
      results <- bootstrap(x, 1000, theta)
      E[i]<-(quantile(results$thetastar, probs=.975 ,type=3, names=F)-quantile(results$thetastar, probs=.025 ,type=3, names=F))/2
    }
    return(CI(E))
  }
  
  E.ci.T4.taste[i,]<-c(i,"pancake","taste",samplen(i))
  
  i=i+1
  if (i > length(pancake$taste)) break()
  
}

## pancake==texture
E.ci.T4.texture<-data.frame(n=integer(),
                            food=numeric(),
                            variable=numeric(),
                            lower=numeric(),
                            mean=numeric(),
                            upper=numeric())

for (i in 1:300) {
  
  samplen<-function(n){
    E<-c()
    for(i in 1:500){
      x<-sample(pancake$texture, n, replace = F) # change the parameters for others subgroups
      theta <- function(x){mean(x)}
      results <- bootstrap(x, 1000, theta)
      E[i]<-(quantile(results$thetastar, probs=.975 ,type=3, names=F)-quantile(results$thetastar, probs=.025 ,type=3, names=F))/2
    }
    return(CI(E))
  }
  
  E.ci.T4.texture[i,]<-c(i,"pancake","texture",samplen(i))
  
  i=i+1
  if (i > length(pancake$texture)) break()
  
}

## pancake==color
E.ci.T4.color<-data.frame(n=integer(),
                          food=numeric(),
                          variable=numeric(),
                          lower=numeric(),
                          mean=numeric(),
                          upper=numeric())

for (i in 1:300) {
  
  samplen<-function(n){
    E<-c()
    for(i in 1:500){
      x<-sample(pancake$color, n, replace = F) # change the parameters for others subgroups
      theta <- function(x){mean(x)}
      results <- bootstrap(x, 1000, theta)
      E[i]<-(quantile(results$thetastar, probs=.975 ,type=3, names=F)-quantile(results$thetastar, probs=.025 ,type=3, names=F))/2
    }
    return(CI(E))
  }
  
  E.ci.T4.color[i,]<-c(i,"pancake","color",samplen(i))
  
  i=i+1
  if (i > length(pancake$color)) break()
  
}

## pancake==acceptance
E.ci.T4.acceptance<-data.frame(n=integer(),
                               food=numeric(),
                               variable=numeric(),
                               lower=numeric(),
                               mean=numeric(),
                               upper=numeric())

for (i in 1:300) {
  
  samplen<-function(n){
    E<-c()
    for(i in 1:500){
      x<-sample(pancake$acceptance, n, replace = F) # change the parameters for others subgroups
      theta <- function(x){mean(x)}
      results <- bootstrap(x, 1000, theta)
      E[i]<-(quantile(results$thetastar, probs=.975 ,type=3, names=F)-quantile(results$thetastar, probs=.025 ,type=3, names=F))/2
    }
    return(CI(E))
  }
  
  E.ci.T4.acceptance[i,]<-c(i,"pancake","acceptance",samplen(i))
  
  i=i+1
  if (i > length(pancake$acceptance)) break()
  
}


#### combining dataframes pancake
E.ci.table4.pancake<-rbind(E.ci.T4.appearance,E.ci.T4.aroma,E.ci.T4.taste,E.ci.T4.texture,E.ci.T4.color,E.ci.T4.acceptance)

#### SFIHA ####

## sfiha==appearance
E.ci.T4.appearance<-data.frame(n=integer(),
                               food=numeric(),
                               variable=numeric(),
                               lower=numeric(),
                               mean=numeric(),
                               upper=numeric())

for (i in 1:300) {
  
  samplen<-function(n){
    E<-c()
    for(i in 1:500){
      x<-sample(sfiha$appearance, n, replace = F) # change the parameters for others subgroups
      theta <- function(x){mean(x)}
      results <- bootstrap(x, 1000, theta)
      E[i]<-(quantile(results$thetastar, probs=.975 ,type=3, names=F)-quantile(results$thetastar, probs=.025 ,type=3, names=F))/2
    }
    return(CI(E))
  }
  
  E.ci.T4.appearance[i,]<-c(i,"sfiha","appearance",samplen(i))
  
  i=i+1
  if (i > length(sfiha$appearance)) break()
  
}

## sfiha==aroma
E.ci.T4.aroma<-data.frame(n=integer(),
                          food=numeric(),
                          variable=numeric(),
                          lower=numeric(),
                          mean=numeric(),
                          upper=numeric())

for (i in 1:300) {
  
  samplen<-function(n){
    E<-c()
    for(i in 1:500){
      x<-sample(sfiha$aroma, n, replace = F) # change the parameters for others subgroups
      theta <- function(x){mean(x)}
      results <- bootstrap(x, 1000, theta)
      E[i]<-(quantile(results$thetastar, probs=.975 ,type=3, names=F)-quantile(results$thetastar, probs=.025 ,type=3, names=F))/2
    }
    return(CI(E))
  }
  
  E.ci.T4.aroma[i,]<-c(i,"sfiha","aroma",samplen(i))
  
  i=i+1
  if (i > length(sfiha$aroma)) break()
  
}

## sfiha==taste
E.ci.T4.taste<-data.frame(n=integer(),
                          food=numeric(),
                          variable=numeric(),
                          lower=numeric(),
                          mean=numeric(),
                          upper=numeric())

for (i in 1:300) {
  
  samplen<-function(n){
    E<-c()
    for(i in 1:500){
      x<-sample(sfiha$taste, n, replace = F) # change the parameters for others subgroups
      theta <- function(x){mean(x)}
      results <- bootstrap(x, 1000, theta)
      E[i]<-(quantile(results$thetastar, probs=.975 ,type=3, names=F)-quantile(results$thetastar, probs=.025 ,type=3, names=F))/2
    }
    return(CI(E))
  }
  
  E.ci.T4.taste[i,]<-c(i,"sfiha","taste",samplen(i))
  
  i=i+1
  if (i > length(sfiha$taste)) break()
  
}

## sfiha==texture
E.ci.T4.texture<-data.frame(n=integer(),
                            food=numeric(),
                            variable=numeric(),
                            lower=numeric(),
                            mean=numeric(),
                            upper=numeric())

for (i in 1:300) {
  
  samplen<-function(n){
    E<-c()
    for(i in 1:500){
      x<-sample(sfiha$texture, n, replace = F) # change the parameters for others subgroups
      theta <- function(x){mean(x)}
      results <- bootstrap(x, 1000, theta)
      E[i]<-(quantile(results$thetastar, probs=.975 ,type=3, names=F)-quantile(results$thetastar, probs=.025 ,type=3, names=F))/2
    }
    return(CI(E))
  }
  
  E.ci.T4.texture[i,]<-c(i,"sfiha","texture",samplen(i))
  
  i=i+1
  if (i > length(sfiha$texture)) break()
  
}

## sfiha==color
E.ci.T4.color<-data.frame(n=integer(),
                          food=numeric(),
                          variable=numeric(),
                          lower=numeric(),
                          mean=numeric(),
                          upper=numeric())

for (i in 1:300) {
  
  samplen<-function(n){
    E<-c()
    for(i in 1:500){
      x<-sample(sfiha$color, n, replace = F) # change the parameters for others subgroups
      theta <- function(x){mean(x)}
      results <- bootstrap(x, 1000, theta)
      E[i]<-(quantile(results$thetastar, probs=.975 ,type=3, names=F)-quantile(results$thetastar, probs=.025 ,type=3, names=F))/2
    }
    return(CI(E))
  }
  
  E.ci.T4.color[i,]<-c(i,"sfiha","color",samplen(i))
  
  i=i+1
  if (i > length(sfiha$color)) break()
  
}

## sfiha==acceptance
E.ci.T4.acceptance<-data.frame(n=integer(),
                               food=numeric(),
                               variable=numeric(),
                               lower=numeric(),
                               mean=numeric(),
                               upper=numeric())

for (i in 1:300) {
  
  samplen<-function(n){
    E<-c()
    for(i in 1:500){
      x<-sample(sfiha$acceptance, n, replace = F) # change the parameters for others subgroups
      theta <- function(x){mean(x)}
      results <- bootstrap(x, 1000, theta)
      E[i]<-(quantile(results$thetastar, probs=.975 ,type=3, names=F)-quantile(results$thetastar, probs=.025 ,type=3, names=F))/2
    }
    return(CI(E))
  }
  
  E.ci.T4.acceptance[i,]<-c(i,"sfiha","acceptance",samplen(i))
  
  i=i+1
  if (i > length(sfiha$acceptance)) break()
  
}


#### combining dataframes sfiha
E.ci.table4.sfiha<-rbind(E.ci.T4.appearance,E.ci.T4.aroma,E.ci.T4.taste,E.ci.T4.texture,E.ci.T4.color,E.ci.T4.acceptance)


##### DF FOR TABLE 4 ##### 


### selecionar apenas os dados que correspondem aos criterios 
## critérios 1 - E e CI <0,50 
## critério 2 - por variável e alimento


E.ci.table4<-rbind(E.ci.table4.bread,
                   E.ci.table4.sfiha,
                   E.ci.table4.cookie,
                   E.ci.table4.muffin,
                   E.ci.table4.pancake)



save(E.ci.table4, file = "EciTable4.RData")

rm(list = ls())

















