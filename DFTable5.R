# Function in R to estimate the confidence interval of the admissible error (E).
# Created by: Daiana Novello, Adilson dos Anjosb
# Paper: Number of children needed to evaluate products made in cooking workshops
# TABLE 5

library(Rmisc)
library(bootstrap)


# Load data
load("bread.RData")
load("cookie.RData")
load("muffin.RData")
load("pancake.RData")
load("sfiha.RData")



# subgroups gender==1 (male)


#### BREAD ####
## BREAD==appearance
E.ci.T5.appearance<-data.frame(n=integer(),
                               food=numeric(),
                               gender=numeric(),
                               variable=numeric(),
                               lower=numeric(),
                               mean=numeric(),
                               upper=numeric())

for (i in 1:160) {
  
  samplen<-function(n){
    E<-c()
    for(i in 1:500){
      x<-sample(bread$appearance[bread$gender==1], n, replace = F) # change the parameters for others subgroups
      theta <- function(x){mean(x)}
      results <- bootstrap(x, 1000, theta)
      E[i]<-(quantile(results$thetastar, probs=.975 ,type=3, names=F)-quantile(results$thetastar, probs=.025 ,type=3, names=F))/2
    }
    return(CI(E))
  }
  
  E.ci.T5.appearance[i,]<-c(i,"bread","male","appearance",samplen(i))
  
  i=i+1
  if (i > length(bread$appearance[bread$gender==1])) break()
  
}

## BREAD==aroma
E.ci.T5.aroma<-data.frame(n=integer(),
                          food=numeric(),
                          gender=numeric(),
                          variable=numeric(),
                          lower=numeric(),
                          mean=numeric(),
                          upper=numeric())

for (i in 1:160) {
  
  samplen<-function(n){
    E<-c()
    for(i in 1:500){
      x<-sample(bread$aroma[bread$gender==1], n, replace = F) # change the parameters for others subgroups
      theta <- function(x){mean(x)}
      results <- bootstrap(x, 1000, theta)
      E[i]<-(quantile(results$thetastar, probs=.975 ,type=3, names=F)-quantile(results$thetastar, probs=.025 ,type=3, names=F))/2
    }
    return(CI(E))
  }
  
  E.ci.T5.aroma[i,]<-c(i,"bread","male","aroma",samplen(i))
  
  i=i+1
  if (i > length(bread$aroma[bread$gender==1])) break()
  
}

## BREAD==taste
E.ci.T5.taste<-data.frame(n=integer(),
                          food=numeric(),
                          gender=numeric(),
                          variable=numeric(),
                          lower=numeric(),
                          mean=numeric(),
                          upper=numeric())

for (i in 1:160) {
  
  samplen<-function(n){
    E<-c()
    for(i in 1:500){
      x<-sample(bread$taste[bread$gender==1], n, replace = F) # change the parameters for others subgroups
      theta <- function(x){mean(x)}
      results <- bootstrap(x, 1000, theta)
      E[i]<-(quantile(results$thetastar, probs=.975 ,type=3, names=F)-quantile(results$thetastar, probs=.025 ,type=3, names=F))/2
    }
    return(CI(E))
  }
  
  E.ci.T5.taste[i,]<-c(i,"bread","male","taste",samplen(i))
  
  i=i+1
  if (i > length(bread$taste[bread$gender==1])) break()
  
}

## BREAD==texture
E.ci.T5.texture<-data.frame(n=integer(),
                            food=numeric(),
                            gender=numeric(),
                            variable=numeric(),
                            lower=numeric(),
                            mean=numeric(),
                            upper=numeric())

for (i in 1:160) {
  
  samplen<-function(n){
    E<-c()
    for(i in 1:500){
      x<-sample(bread$texture[bread$gender==1], n, replace = F) # change the parameters for others subgroups
      theta <- function(x){mean(x)}
      results <- bootstrap(x, 1000, theta)
      E[i]<-(quantile(results$thetastar, probs=.975 ,type=3, names=F)-quantile(results$thetastar, probs=.025 ,type=3, names=F))/2
    }
    return(CI(E))
  }
  
  E.ci.T5.texture[i,]<-c(i,"bread","male","texture",samplen(i))
  
  i=i+1
  if (i > length(bread$texture[bread$gender==1])) break()
  
}

## BREAD==color
E.ci.T5.color<-data.frame(n=integer(),
                          food=numeric(),
                          gender=numeric(),
                          variable=numeric(),
                          lower=numeric(),
                          mean=numeric(),
                          upper=numeric())

for (i in 1:160) {
  
  samplen<-function(n){
    E<-c()
    for(i in 1:500){
      x<-sample(bread$color[bread$gender==1], n, replace = F) # change the parameters for others subgroups
      theta <- function(x){mean(x)}
      results <- bootstrap(x, 1000, theta)
      E[i]<-(quantile(results$thetastar, probs=.975 ,type=3, names=F)-quantile(results$thetastar, probs=.025 ,type=3, names=F))/2
    }
    return(CI(E))
  }
  
  E.ci.T5.color[i,]<-c(i,"bread","male","color",samplen(i))
  
  i=i+1
  if (i > length(bread$color[bread$gender==1])) break()
  
}

## BREAD==acceptance
E.ci.T5.acceptance<-data.frame(n=integer(),
                               food=numeric(),
                               gender=numeric(),
                               variable=numeric(),
                               lower=numeric(),
                               mean=numeric(),
                               upper=numeric())

for (i in 1:160) {
  
  samplen<-function(n){
    E<-c()
    for(i in 1:500){
      x<-sample(bread$acceptance[bread$gender==1], n, replace = F) # change the parameters for others subgroups
      theta <- function(x){mean(x)}
      results <- bootstrap(x, 1000, theta)
      E[i]<-(quantile(results$thetastar, probs=.975 ,type=3, names=F)-quantile(results$thetastar, probs=.025 ,type=3, names=F))/2
    }
    return(CI(E))
  }
  
  E.ci.T5.acceptance[i,]<-c(i,"bread","male","acceptance",samplen(i))
  
  i=i+1
  if (i > length(bread$acceptance[bread$gender==1])) break()
  
}


#### combining dataframes bread
E.ci.table5.bread.M<-rbind(E.ci.T5.appearance,E.ci.T5.aroma,E.ci.T5.taste,E.ci.T5.texture,E.ci.T5.color)



####COOKIE####
## cookie==appearance
E.ci.T5.appearance<-data.frame(n=integer(),
                               food=numeric(),
                               gender=numeric(),
                               variable=numeric(),
                               lower=numeric(),
                               mean=numeric(),
                               upper=numeric())

for (i in 1:160) {
  
  samplen<-function(n){
    E<-c()
    for(i in 1:500){
      x<-sample(cookie$appearance[cookie$gender==1], n, replace = F) # change the parameters for others subgroups
      theta <- function(x){mean(x)}
      results <- bootstrap(x, 1000, theta)
      E[i]<-(quantile(results$thetastar, probs=.975 ,type=3, names=F)-quantile(results$thetastar, probs=.025 ,type=3, names=F))/2
    }
    return(CI(E))
  }
  
  E.ci.T5.appearance[i,]<-c(i,"cookie","male","appearance",samplen(i))
  
  i=i+1
  if (i > length(cookie$appearance[cookie$gender==1])) break()
  
}

## cookie==aroma
E.ci.T5.aroma<-data.frame(n=integer(),
                          food=numeric(),
                          gender=numeric(),
                          variable=numeric(),
                          lower=numeric(),
                          mean=numeric(),
                          upper=numeric())

for (i in 1:160) {
  
  samplen<-function(n){
    E<-c()
    for(i in 1:500){
      x<-sample(cookie$aroma[cookie$gender==1], n, replace = F) # change the parameters for others subgroups
      theta <- function(x){mean(x)}
      results <- bootstrap(x, 1000, theta)
      E[i]<-(quantile(results$thetastar, probs=.975 ,type=3, names=F)-quantile(results$thetastar, probs=.025 ,type=3, names=F))/2
    }
    return(CI(E))
  }
  
  E.ci.T5.aroma[i,]<-c(i,"cookie","male","aroma",samplen(i))
  
  i=i+1
  if (i > length(cookie$aroma[cookie$gender==1])) break()
  
}

## cookie==taste
E.ci.T5.taste<-data.frame(n=integer(),
                          food=numeric(),
                          gender=numeric(),
                          variable=numeric(),
                          lower=numeric(),
                          mean=numeric(),
                          upper=numeric())

for (i in 1:160) {
  
  samplen<-function(n){
    E<-c()
    for(i in 1:500){
      x<-sample(cookie$taste[cookie$gender==1], n, replace = F) # change the parameters for others subgroups
      theta <- function(x){mean(x)}
      results <- bootstrap(x, 1000, theta)
      E[i]<-(quantile(results$thetastar, probs=.975 ,type=3, names=F)-quantile(results$thetastar, probs=.025 ,type=3, names=F))/2
    }
    return(CI(E))
  }
  
  E.ci.T5.taste[i,]<-c(i,"cookie","male","taste",samplen(i))
  
  i=i+1
  if (i > length(cookie$taste[cookie$gender==1])) break()
  
}

## cookie==texture
E.ci.T5.texture<-data.frame(n=integer(),
                            food=numeric(),
                            gender=numeric(),
                            variable=numeric(),
                            lower=numeric(),
                            mean=numeric(),
                            upper=numeric())

for (i in 1:160) {
  
  samplen<-function(n){
    E<-c()
    for(i in 1:500){
      x<-sample(cookie$texture[cookie$gender==1], n, replace = F) # change the parameters for others subgroups
      theta <- function(x){mean(x)}
      results <- bootstrap(x, 1000, theta)
      E[i]<-(quantile(results$thetastar, probs=.975 ,type=3, names=F)-quantile(results$thetastar, probs=.025 ,type=3, names=F))/2
    }
    return(CI(E))
  }
  
  E.ci.T5.texture[i,]<-c(i,"cookie","male","texture",samplen(i))
  
  i=i+1
  if (i > length(cookie$texture[cookie$gender==1])) break()
  
}

## cookie==color
E.ci.T5.color<-data.frame(n=integer(),
                          food=numeric(),
                          gender=numeric(),
                          variable=numeric(),
                          lower=numeric(),
                          mean=numeric(),
                          upper=numeric())

for (i in 1:160) {
  
  samplen<-function(n){
    E<-c()
    for(i in 1:500){
      x<-sample(cookie$color[cookie$gender==1], n, replace = F) # change the parameters for others subgroups
      theta <- function(x){mean(x)}
      results <- bootstrap(x, 1000, theta)
      E[i]<-(quantile(results$thetastar, probs=.975 ,type=3, names=F)-quantile(results$thetastar, probs=.025 ,type=3, names=F))/2
    }
    return(CI(E))
  }
  
  E.ci.T5.color[i,]<-c(i,"cookie","male","color",samplen(i))
  
  i=i+1
  if (i > length(cookie$color[cookie$gender==1])) break()
  
}

## cookie==acceptance
E.ci.T5.acceptance<-data.frame(n=integer(),
                               food=numeric(),
                               gender=numeric(),
                               variable=numeric(),
                               lower=numeric(),
                               mean=numeric(),
                               upper=numeric())

for (i in 1:160) {
  
  samplen<-function(n){
    E<-c()
    for(i in 1:500){
      x<-sample(cookie$acceptance[cookie$gender==1], n, replace = F) # change the parameters for others subgroups
      theta <- function(x){mean(x)}
      results <- bootstrap(x, 1000, theta)
      E[i]<-(quantile(results$thetastar, probs=.975 ,type=3, names=F)-quantile(results$thetastar, probs=.025 ,type=3, names=F))/2
    }
    return(CI(E))
  }
  
  E.ci.T5.acceptance[i,]<-c(i,"cookie","male","acceptance",samplen(i))
  
  i=i+1
  if (i > length(cookie$acceptance[cookie$gender==1])) break()
  
}


#### combining dataframes cookie
E.ci.table5.cookie.M<-rbind(E.ci.T5.appearance,E.ci.T5.aroma,E.ci.T5.taste,E.ci.T5.texture,E.ci.T5.color)

#### MUFFIN ####
## muffin==appearance
E.ci.T5.appearance<-data.frame(n=integer(),
                               food=numeric(),
                               gender=numeric(),
                               variable=numeric(),
                               lower=numeric(),
                               mean=numeric(),
                               upper=numeric())

for (i in 1:160) {
  
  samplen<-function(n){
    E<-c()
    for(i in 1:500){
      x<-sample(muffin$appearance[muffin$gender==1], n, replace = F) # change the parameters for others subgroups
      theta <- function(x){mean(x)}
      results <- bootstrap(x, 1000, theta)
      E[i]<-(quantile(results$thetastar, probs=.975 ,type=3, names=F)-quantile(results$thetastar, probs=.025 ,type=3, names=F))/2
    }
    return(CI(E))
  }
  
  E.ci.T5.appearance[i,]<-c(i,"muffin","male","appearance",samplen(i))
  
  i=i+1
  if (i > length(muffin$appearance[muffin$gender==1])) break()
  
}

## muffin==aroma
E.ci.T5.aroma<-data.frame(n=integer(),
                          food=numeric(),
                          gender=numeric(),
                          variable=numeric(),
                          lower=numeric(),
                          mean=numeric(),
                          upper=numeric())

for (i in 1:160) {
  
  samplen<-function(n){
    E<-c()
    for(i in 1:500){
      x<-sample(muffin$aroma[muffin$gender==1], n, replace = F) # change the parameters for others subgroups
      theta <- function(x){mean(x)}
      results <- bootstrap(x, 1000, theta)
      E[i]<-(quantile(results$thetastar, probs=.975 ,type=3, names=F)-quantile(results$thetastar, probs=.025 ,type=3, names=F))/2
    }
    return(CI(E))
  }
  
  E.ci.T5.aroma[i,]<-c(i,"muffin","male","aroma",samplen(i))
  
  i=i+1
  if (i > length(muffin$aroma[muffin$gender==1])) break()
  
}

## muffin==taste
E.ci.T5.taste<-data.frame(n=integer(),
                          food=numeric(),
                          gender=numeric(),
                          variable=numeric(),
                          lower=numeric(),
                          mean=numeric(),
                          upper=numeric())

for (i in 1:160) {
  
  samplen<-function(n){
    E<-c()
    for(i in 1:500){
      x<-sample(muffin$taste[muffin$gender==1], n, replace = F) # change the parameters for others subgroups
      theta <- function(x){mean(x)}
      results <- bootstrap(x, 1000, theta)
      E[i]<-(quantile(results$thetastar, probs=.975 ,type=3, names=F)-quantile(results$thetastar, probs=.025 ,type=3, names=F))/2
    }
    return(CI(E))
  }
  
  E.ci.T5.taste[i,]<-c(i,"muffin","male","taste",samplen(i))
  
  i=i+1
  if (i > length(muffin$taste[muffin$gender==1])) break()
  
}

## muffin==texture
E.ci.T5.texture<-data.frame(n=integer(),
                            food=numeric(),
                            gender=numeric(),
                            variable=numeric(),
                            lower=numeric(),
                            mean=numeric(),
                            upper=numeric())

for (i in 1:160) {
  
  samplen<-function(n){
    E<-c()
    for(i in 1:500){
      x<-sample(muffin$texture[muffin$gender==1], n, replace = F) # change the parameters for others subgroups
      theta <- function(x){mean(x)}
      results <- bootstrap(x, 1000, theta)
      E[i]<-(quantile(results$thetastar, probs=.975 ,type=3, names=F)-quantile(results$thetastar, probs=.025 ,type=3, names=F))/2
    }
    return(CI(E))
  }
  
  E.ci.T5.texture[i,]<-c(i,"muffin","male","texture",samplen(i))
  
  i=i+1
  if (i > length(muffin$texture[muffin$gender==1])) break()
  
}

## muffin==color
E.ci.T5.color<-data.frame(n=integer(),
                          food=numeric(),
                          gender=numeric(),
                          variable=numeric(),
                          lower=numeric(),
                          mean=numeric(),
                          upper=numeric())

for (i in 1:160) {
  
  samplen<-function(n){
    E<-c()
    for(i in 1:500){
      x<-sample(muffin$color[muffin$gender==1], n, replace = F) # change the parameters for others subgroups
      theta <- function(x){mean(x)}
      results <- bootstrap(x, 1000, theta)
      E[i]<-(quantile(results$thetastar, probs=.975 ,type=3, names=F)-quantile(results$thetastar, probs=.025 ,type=3, names=F))/2
    }
    return(CI(E))
  }
  
  E.ci.T5.color[i,]<-c(i,"muffin","male","color",samplen(i))
  
  i=i+1
  if (i > length(muffin$color[muffin$gender==1])) break()
  
}

## muffin==acceptance
E.ci.T5.acceptance<-data.frame(n=integer(),
                               food=numeric(),
                               gender=numeric(),
                               variable=numeric(),
                               lower=numeric(),
                               mean=numeric(),
                               upper=numeric())

for (i in 1:160) {
  
  samplen<-function(n){
    E<-c()
    for(i in 1:500){
      x<-sample(muffin$acceptance[muffin$gender==1], n, replace = F) # change the parameters for others subgroups
      theta <- function(x){mean(x)}
      results <- bootstrap(x, 1000, theta)
      E[i]<-(quantile(results$thetastar, probs=.975 ,type=3, names=F)-quantile(results$thetastar, probs=.025 ,type=3, names=F))/2
    }
    return(CI(E))
  }
  
  E.ci.T5.acceptance[i,]<-c(i,"muffin","male","acceptance",samplen(i))
  
  i=i+1
  if (i > length(muffin$acceptance[muffin$gender==1])) break()
  
}


#### combining dataframes muffin
E.ci.table5.muffin.M<-rbind(E.ci.T5.appearance,E.ci.T5.aroma,E.ci.T5.taste,E.ci.T5.texture,E.ci.T5.color)

#### PANCAKE ####

## pancake==appearance
E.ci.T5.appearance<-data.frame(n=integer(),
                               food=numeric(),
                               gender=numeric(),
                               variable=numeric(),
                               lower=numeric(),
                               mean=numeric(),
                               upper=numeric())

for (i in 1:160) {
  
  samplen<-function(n){
    E<-c()
    for(i in 1:500){
      x<-sample(pancake$appearance[pancake$gender==1], n, replace = F) # change the parameters for others subgroups
      theta <- function(x){mean(x)}
      results <- bootstrap(x, 1000, theta)
      E[i]<-(quantile(results$thetastar, probs=.975 ,type=3, names=F)-quantile(results$thetastar, probs=.025 ,type=3, names=F))/2
    }
    return(CI(E))
  }
  
  E.ci.T5.appearance[i,]<-c(i,"pancake","male","appearance",samplen(i))
  
  i=i+1
  if (i > length(pancake$appearance[pancake$gender==1])) break()
  
}

## pancake==aroma
E.ci.T5.aroma<-data.frame(n=integer(),
                          food=numeric(),
                          gender=numeric(),
                          variable=numeric(),
                          lower=numeric(),
                          mean=numeric(),
                          upper=numeric())

for (i in 1:160) {
  
  samplen<-function(n){
    E<-c()
    for(i in 1:500){
      x<-sample(pancake$aroma[pancake$gender==1], n, replace = F) # change the parameters for others subgroups
      theta <- function(x){mean(x)}
      results <- bootstrap(x, 1000, theta)
      E[i]<-(quantile(results$thetastar, probs=.975 ,type=3, names=F)-quantile(results$thetastar, probs=.025 ,type=3, names=F))/2
    }
    return(CI(E))
  }
  
  E.ci.T5.aroma[i,]<-c(i,"pancake","male","aroma",samplen(i))
  
  i=i+1
  if (i > length(pancake$aroma[pancake$gender==1])) break()
  
}

## pancake==taste
E.ci.T5.taste<-data.frame(n=integer(),
                          food=numeric(),
                          gender=numeric(),
                          variable=numeric(),
                          lower=numeric(),
                          mean=numeric(),
                          upper=numeric())

for (i in 1:160) {
  
  samplen<-function(n){
    E<-c()
    for(i in 1:500){
      x<-sample(pancake$taste[pancake$gender==1], n, replace = F) # change the parameters for others subgroups
      theta <- function(x){mean(x)}
      results <- bootstrap(x, 1000, theta)
      E[i]<-(quantile(results$thetastar, probs=.975 ,type=3, names=F)-quantile(results$thetastar, probs=.025 ,type=3, names=F))/2
    }
    return(CI(E))
  }
  
  E.ci.T5.taste[i,]<-c(i,"pancake","male","taste",samplen(i))
  
  i=i+1
  if (i > length(pancake$taste[pancake$gender==1])) break()
  
}

## pancake==texture
E.ci.T5.texture<-data.frame(n=integer(),
                            food=numeric(),
                            gender=numeric(),
                            variable=numeric(),
                            lower=numeric(),
                            mean=numeric(),
                            upper=numeric())

for (i in 1:160) {
  
  samplen<-function(n){
    E<-c()
    for(i in 1:500){
      x<-sample(pancake$texture[pancake$gender==1], n, replace = F) # change the parameters for others subgroups
      theta <- function(x){mean(x)}
      results <- bootstrap(x, 1000, theta)
      E[i]<-(quantile(results$thetastar, probs=.975 ,type=3, names=F)-quantile(results$thetastar, probs=.025 ,type=3, names=F))/2
    }
    return(CI(E))
  }
  
  E.ci.T5.texture[i,]<-c(i,"pancake","male","texture",samplen(i))
  
  i=i+1
  if (i > length(pancake$texture[pancake$gender==1])) break()
  
}

## pancake==color
E.ci.T5.color<-data.frame(n=integer(),
                          food=numeric(),
                          gender=numeric(),
                          variable=numeric(),
                          lower=numeric(),
                          mean=numeric(),
                          upper=numeric())

for (i in 1:160) {
  
  samplen<-function(n){
    E<-c()
    for(i in 1:500){
      x<-sample(pancake$color[pancake$gender==1], n, replace = F) # change the parameters for others subgroups
      theta <- function(x){mean(x)}
      results <- bootstrap(x, 1000, theta)
      E[i]<-(quantile(results$thetastar, probs=.975 ,type=3, names=F)-quantile(results$thetastar, probs=.025 ,type=3, names=F))/2
    }
    return(CI(E))
  }
  
  E.ci.T5.color[i,]<-c(i,"pancake","male","color",samplen(i))
  
  i=i+1
  if (i > length(pancake$color[pancake$gender==1])) break()
  
}

## pancake==acceptance
E.ci.T5.acceptance<-data.frame(n=integer(),
                               food=numeric(),
                               gender=numeric(),
                               variable=numeric(),
                               lower=numeric(),
                               mean=numeric(),
                               upper=numeric())

for (i in 1:160) {
  
  samplen<-function(n){
    E<-c()
    for(i in 1:500){
      x<-sample(pancake$acceptance[pancake$gender==1], n, replace = F) # change the parameters for others subgroups
      theta <- function(x){mean(x)}
      results <- bootstrap(x, 1000, theta)
      E[i]<-(quantile(results$thetastar, probs=.975 ,type=3, names=F)-quantile(results$thetastar, probs=.025 ,type=3, names=F))/2
    }
    return(CI(E))
  }
  
  E.ci.T5.acceptance[i,]<-c(i,"pancake","male","acceptance",samplen(i))
  
  i=i+1
  if (i > length(pancake$acceptance[pancake$gender==1])) break()
  
}


#### combining dataframes pancake
E.ci.table5.pancake.M<-rbind(E.ci.T5.appearance,E.ci.T5.aroma,E.ci.T5.taste,E.ci.T5.texture,E.ci.T5.color)


#### SFIHA ####

## sfiha==appearance
E.ci.T5.appearance<-data.frame(n=integer(),
                               food=numeric(),
                               gender=numeric(),
                               variable=numeric(),
                               lower=numeric(),
                               mean=numeric(),
                               upper=numeric())

for (i in 1:160) {
  
  samplen<-function(n){
    E<-c()
    for(i in 1:500){
      x<-sample(sfiha$appearance[sfiha$gender==1], n, replace = F) # change the parameters for others subgroups
      theta <- function(x){mean(x)}
      results <- bootstrap(x, 1000, theta)
      E[i]<-(quantile(results$thetastar, probs=.975 ,type=3, names=F)-quantile(results$thetastar, probs=.025 ,type=3, names=F))/2
    }
    return(CI(E))
  }
  
  E.ci.T5.appearance[i,]<-c(i,"sfiha","male","appearance",samplen(i))
  
  i=i+1
  if (i > length(sfiha$appearance[sfiha$gender==1])) break()
  
}

## sfiha==aroma
E.ci.T5.aroma<-data.frame(n=integer(),
                          food=numeric(),
                          gender=numeric(),
                          variable=numeric(),
                          lower=numeric(),
                          mean=numeric(),
                          upper=numeric())

for (i in 1:160) {
  
  samplen<-function(n){
    E<-c()
    for(i in 1:500){
      x<-sample(sfiha$aroma[sfiha$gender==1], n, replace = F) # change the parameters for others subgroups
      theta <- function(x){mean(x)}
      results <- bootstrap(x, 1000, theta)
      E[i]<-(quantile(results$thetastar, probs=.975 ,type=3, names=F)-quantile(results$thetastar, probs=.025 ,type=3, names=F))/2
    }
    return(CI(E))
  }
  
  E.ci.T5.aroma[i,]<-c(i,"sfiha","male","aroma",samplen(i))
  
  i=i+1
  if (i > length(sfiha$aroma[sfiha$gender==1])) break()
  
}

## sfiha==taste
E.ci.T5.taste<-data.frame(n=integer(),
                          food=numeric(),
                          gender=numeric(),
                          variable=numeric(),
                          lower=numeric(),
                          mean=numeric(),
                          upper=numeric())

for (i in 1:160) {
  
  samplen<-function(n){
    E<-c()
    for(i in 1:500){
      x<-sample(sfiha$taste[sfiha$gender==1], n, replace = F) # change the parameters for others subgroups
      theta <- function(x){mean(x)}
      results <- bootstrap(x, 1000, theta)
      E[i]<-(quantile(results$thetastar, probs=.975 ,type=3, names=F)-quantile(results$thetastar, probs=.025 ,type=3, names=F))/2
    }
    return(CI(E))
  }
  
  E.ci.T5.taste[i,]<-c(i,"sfiha","male","taste",samplen(i))
  
  i=i+1
  if (i > length(sfiha$taste[sfiha$gender==1])) break()
  
}

## sfiha==texture
E.ci.T5.texture<-data.frame(n=integer(),
                            food=numeric(),
                            gender=numeric(),
                            variable=numeric(),
                            lower=numeric(),
                            mean=numeric(),
                            upper=numeric())

for (i in 1:160) {
  
  samplen<-function(n){
    E<-c()
    for(i in 1:500){
      x<-sample(sfiha$texture[sfiha$gender==1], n, replace = F) # change the parameters for others subgroups
      theta <- function(x){mean(x)}
      results <- bootstrap(x, 1000, theta)
      E[i]<-(quantile(results$thetastar, probs=.975 ,type=3, names=F)-quantile(results$thetastar, probs=.025 ,type=3, names=F))/2
    }
    return(CI(E))
  }
  
  E.ci.T5.texture[i,]<-c(i,"sfiha","male","texture",samplen(i))
  
  i=i+1
  if (i > length(sfiha$texture[sfiha$gender==1])) break()
  
}

## sfiha==color
E.ci.T5.color<-data.frame(n=integer(),
                          food=numeric(),
                          gender=numeric(),
                          variable=numeric(),
                          lower=numeric(),
                          mean=numeric(),
                          upper=numeric())

for (i in 1:160) {
  
  samplen<-function(n){
    E<-c()
    for(i in 1:500){
      x<-sample(sfiha$color[sfiha$gender==1], n, replace = F) # change the parameters for others subgroups
      theta <- function(x){mean(x)}
      results <- bootstrap(x, 1000, theta)
      E[i]<-(quantile(results$thetastar, probs=.975 ,type=3, names=F)-quantile(results$thetastar, probs=.025 ,type=3, names=F))/2
    }
    return(CI(E))
  }
  
  E.ci.T5.color[i,]<-c(i,"sfiha","male","color",samplen(i))
  
  i=i+1
  if (i > length(sfiha$color[sfiha$gender==1])) break()
  
}

## sfiha==acceptance
E.ci.T5.acceptance<-data.frame(n=integer(),
                               food=numeric(),
                               gender=numeric(),
                               variable=numeric(),
                               lower=numeric(),
                               mean=numeric(),
                               upper=numeric())

for (i in 1:160) {
  
  samplen<-function(n){
    E<-c()
    for(i in 1:500){
      x<-sample(sfiha$acceptance[sfiha$gender==1], n, replace = F) # change the parameters for others subgroups
      theta <- function(x){mean(x)}
      results <- bootstrap(x, 1000, theta)
      E[i]<-(quantile(results$thetastar, probs=.975 ,type=3, names=F)-quantile(results$thetastar, probs=.025 ,type=3, names=F))/2
    }
    return(CI(E))
  }
  
  E.ci.T5.acceptance[i,]<-c(i,"sfiha","male","acceptance",samplen(i))
  
  i=i+1
  if (i > length(sfiha$acceptance[sfiha$gender==1])) break()
  
}


#### combining dataframes sfiha
E.ci.table5.sfiha.M<-rbind(E.ci.T5.appearance,E.ci.T5.aroma,E.ci.T5.taste,E.ci.T5.texture,E.ci.T5.color)

E.ci.table5.male<-rbind(E.ci.table5.bread.M,
                   E.ci.table5.sfiha.M,
                   E.ci.table5.cookie.M,
                   E.ci.table5.muffin.M,
                   E.ci.table5.pancake.M)



save(E.ci.table5.male, file = "EciTable5_male.RData")

rm(list = ls())



#################
####  female ####
#################

# subgroups gender==2 (fefemale)


#### BREAD ####
## BREAD==appearance
E.ci.T5.appearance<-data.frame(n=integer(),
                               food=numeric(),
                               gender=numeric(),
                               variable=numeric(),
                               lower=numeric(),
                               mean=numeric(),
                               upper=numeric())

for (i in 1:160) {
  
  samplen<-function(n){
    E<-c()
    for(i in 1:500){
      x<-sample(bread$appearance[bread$gender==2], n, replace = F) # change the parameters for others subgroups
      theta <- function(x){mean(x)}
      results <- bootstrap(x, 1000, theta)
      E[i]<-(quantile(results$thetastar, probs=.975 ,type=3, names=F)-quantile(results$thetastar, probs=.025 ,type=3, names=F))/2
    }
    return(CI(E))
  }
  
  E.ci.T5.appearance[i,]<-c(i,"bread","female","appearance",samplen(i))
  
  i=i+1
  if (i > length(bread$appearance[bread$gender==2])) break()
  
}

## BREAD==aroma
E.ci.T5.aroma<-data.frame(n=integer(),
                          food=numeric(),
                          gender=numeric(),
                          variable=numeric(),
                          lower=numeric(),
                          mean=numeric(),
                          upper=numeric())

for (i in 1:160) {
  
  samplen<-function(n){
    E<-c()
    for(i in 1:500){
      x<-sample(bread$aroma[bread$gender==2], n, replace = F) # change the parameters for others subgroups
      theta <- function(x){mean(x)}
      results <- bootstrap(x, 1000, theta)
      E[i]<-(quantile(results$thetastar, probs=.975 ,type=3, names=F)-quantile(results$thetastar, probs=.025 ,type=3, names=F))/2
    }
    return(CI(E))
  }
  
  E.ci.T5.aroma[i,]<-c(i,"bread","female","aroma",samplen(i))
  
  i=i+1
  if (i > length(bread$aroma[bread$gender==2])) break()
  
}

## BREAD==taste
E.ci.T5.taste<-data.frame(n=integer(),
                          food=numeric(),
                          gender=numeric(),
                          variable=numeric(),
                          lower=numeric(),
                          mean=numeric(),
                          upper=numeric())

for (i in 1:160) {
  
  samplen<-function(n){
    E<-c()
    for(i in 1:500){
      x<-sample(bread$taste[bread$gender==2], n, replace = F) # change the parameters for others subgroups
      theta <- function(x){mean(x)}
      results <- bootstrap(x, 1000, theta)
      E[i]<-(quantile(results$thetastar, probs=.975 ,type=3, names=F)-quantile(results$thetastar, probs=.025 ,type=3, names=F))/2
    }
    return(CI(E))
  }
  
  E.ci.T5.taste[i,]<-c(i,"bread","female","taste",samplen(i))
  
  i=i+1
  if (i > length(bread$taste[bread$gender==2])) break()
  
}

## BREAD==texture
E.ci.T5.texture<-data.frame(n=integer(),
                            food=numeric(),
                            gender=numeric(),
                            variable=numeric(),
                            lower=numeric(),
                            mean=numeric(),
                            upper=numeric())

for (i in 1:160) {
  
  samplen<-function(n){
    E<-c()
    for(i in 1:500){
      x<-sample(bread$texture[bread$gender==2], n, replace = F) # change the parameters for others subgroups
      theta <- function(x){mean(x)}
      results <- bootstrap(x, 1000, theta)
      E[i]<-(quantile(results$thetastar, probs=.975 ,type=3, names=F)-quantile(results$thetastar, probs=.025 ,type=3, names=F))/2
    }
    return(CI(E))
  }
  
  E.ci.T5.texture[i,]<-c(i,"bread","female","texture",samplen(i))
  
  i=i+1
  if (i > length(bread$texture[bread$gender==2])) break()
  
}

## BREAD==color
E.ci.T5.color<-data.frame(n=integer(),
                          food=numeric(),
                          gender=numeric(),
                          variable=numeric(),
                          lower=numeric(),
                          mean=numeric(),
                          upper=numeric())

for (i in 1:160) {
  
  samplen<-function(n){
    E<-c()
    for(i in 1:500){
      x<-sample(bread$color[bread$gender==2], n, replace = F) # change the parameters for others subgroups
      theta <- function(x){mean(x)}
      results <- bootstrap(x, 1000, theta)
      E[i]<-(quantile(results$thetastar, probs=.975 ,type=3, names=F)-quantile(results$thetastar, probs=.025 ,type=3, names=F))/2
    }
    return(CI(E))
  }
  
  E.ci.T5.color[i,]<-c(i,"bread","female","color",samplen(i))
  
  i=i+1
  if (i > length(bread$color[bread$gender==2])) break()
  
}

## BREAD==acceptance
E.ci.T5.acceptance<-data.frame(n=integer(),
                               food=numeric(),
                               gender=numeric(),
                               variable=numeric(),
                               lower=numeric(),
                               mean=numeric(),
                               upper=numeric())

for (i in 1:160) {
  
  samplen<-function(n){
    E<-c()
    for(i in 1:500){
      x<-sample(bread$acceptance[bread$gender==2], n, replace = F) # change the parameters for others subgroups
      theta <- function(x){mean(x)}
      results <- bootstrap(x, 1000, theta)
      E[i]<-(quantile(results$thetastar, probs=.975 ,type=3, names=F)-quantile(results$thetastar, probs=.025 ,type=3, names=F))/2
    }
    return(CI(E))
  }
  
  E.ci.T5.acceptance[i,]<-c(i,"bread","female","acceptance",samplen(i))
  
  i=i+1
  if (i > length(bread$acceptance[bread$gender==2])) break()
  
}


#### combining dataframes bread
E.ci.table5.bread.M<-rbind(E.ci.T5.appearance,E.ci.T5.aroma,E.ci.T5.taste,E.ci.T5.texture,E.ci.T5.color)



####COOKIE####
## cookie==appearance
E.ci.T5.appearance<-data.frame(n=integer(),
                               food=numeric(),
                               gender=numeric(),
                               variable=numeric(),
                               lower=numeric(),
                               mean=numeric(),
                               upper=numeric())

for (i in 1:160) {
  
  samplen<-function(n){
    E<-c()
    for(i in 1:500){
      x<-sample(cookie$appearance[cookie$gender==2], n, replace = F) # change the parameters for others subgroups
      theta <- function(x){mean(x)}
      results <- bootstrap(x, 1000, theta)
      E[i]<-(quantile(results$thetastar, probs=.975 ,type=3, names=F)-quantile(results$thetastar, probs=.025 ,type=3, names=F))/2
    }
    return(CI(E))
  }
  
  E.ci.T5.appearance[i,]<-c(i,"cookie","female","appearance",samplen(i))
  
  i=i+1
  if (i > length(cookie$appearance[cookie$gender==2])) break()
  
}

## cookie==aroma
E.ci.T5.aroma<-data.frame(n=integer(),
                          food=numeric(),
                          gender=numeric(),
                          variable=numeric(),
                          lower=numeric(),
                          mean=numeric(),
                          upper=numeric())

for (i in 1:160) {
  
  samplen<-function(n){
    E<-c()
    for(i in 1:500){
      x<-sample(cookie$aroma[cookie$gender==2], n, replace = F) # change the parameters for others subgroups
      theta <- function(x){mean(x)}
      results <- bootstrap(x, 1000, theta)
      E[i]<-(quantile(results$thetastar, probs=.975 ,type=3, names=F)-quantile(results$thetastar, probs=.025 ,type=3, names=F))/2
    }
    return(CI(E))
  }
  
  E.ci.T5.aroma[i,]<-c(i,"cookie","female","aroma",samplen(i))
  
  i=i+1
  if (i > length(cookie$aroma[cookie$gender==2])) break()
  
}

## cookie==taste
E.ci.T5.taste<-data.frame(n=integer(),
                          food=numeric(),
                          gender=numeric(),
                          variable=numeric(),
                          lower=numeric(),
                          mean=numeric(),
                          upper=numeric())

for (i in 1:160) {
  
  samplen<-function(n){
    E<-c()
    for(i in 1:500){
      x<-sample(cookie$taste[cookie$gender==2], n, replace = F) # change the parameters for others subgroups
      theta <- function(x){mean(x)}
      results <- bootstrap(x, 1000, theta)
      E[i]<-(quantile(results$thetastar, probs=.975 ,type=3, names=F)-quantile(results$thetastar, probs=.025 ,type=3, names=F))/2
    }
    return(CI(E))
  }
  
  E.ci.T5.taste[i,]<-c(i,"cookie","female","taste",samplen(i))
  
  i=i+1
  if (i > length(cookie$taste[cookie$gender==2])) break()
  
}

## cookie==texture
E.ci.T5.texture<-data.frame(n=integer(),
                            food=numeric(),
                            gender=numeric(),
                            variable=numeric(),
                            lower=numeric(),
                            mean=numeric(),
                            upper=numeric())

for (i in 1:160) {
  
  samplen<-function(n){
    E<-c()
    for(i in 1:500){
      x<-sample(cookie$texture[cookie$gender==2], n, replace = F) # change the parameters for others subgroups
      theta <- function(x){mean(x)}
      results <- bootstrap(x, 1000, theta)
      E[i]<-(quantile(results$thetastar, probs=.975 ,type=3, names=F)-quantile(results$thetastar, probs=.025 ,type=3, names=F))/2
    }
    return(CI(E))
  }
  
  E.ci.T5.texture[i,]<-c(i,"cookie","female","texture",samplen(i))
  
  i=i+1
  if (i > length(cookie$texture[cookie$gender==2])) break()
  
}

## cookie==color
E.ci.T5.color<-data.frame(n=integer(),
                          food=numeric(),
                          gender=numeric(),
                          variable=numeric(),
                          lower=numeric(),
                          mean=numeric(),
                          upper=numeric())

for (i in 1:160) {
  
  samplen<-function(n){
    E<-c()
    for(i in 1:500){
      x<-sample(cookie$color[cookie$gender==2], n, replace = F) # change the parameters for others subgroups
      theta <- function(x){mean(x)}
      results <- bootstrap(x, 1000, theta)
      E[i]<-(quantile(results$thetastar, probs=.975 ,type=3, names=F)-quantile(results$thetastar, probs=.025 ,type=3, names=F))/2
    }
    return(CI(E))
  }
  
  E.ci.T5.color[i,]<-c(i,"cookie","female","color",samplen(i))
  
  i=i+1
  if (i > length(cookie$color[cookie$gender==2])) break()
  
}

## cookie==acceptance
E.ci.T5.acceptance<-data.frame(n=integer(),
                               food=numeric(),
                               gender=numeric(),
                               variable=numeric(),
                               lower=numeric(),
                               mean=numeric(),
                               upper=numeric())

for (i in 1:160) {
  
  samplen<-function(n){
    E<-c()
    for(i in 1:500){
      x<-sample(cookie$acceptance[cookie$gender==2], n, replace = F) # change the parameters for others subgroups
      theta <- function(x){mean(x)}
      results <- bootstrap(x, 1000, theta)
      E[i]<-(quantile(results$thetastar, probs=.975 ,type=3, names=F)-quantile(results$thetastar, probs=.025 ,type=3, names=F))/2
    }
    return(CI(E))
  }
  
  E.ci.T5.acceptance[i,]<-c(i,"cookie","female","acceptance",samplen(i))
  
  i=i+1
  if (i > length(cookie$acceptance[cookie$gender==2])) break()
  
}


#### combining dataframes cookie
E.ci.table5.cookie.M<-rbind(E.ci.T5.appearance,E.ci.T5.aroma,E.ci.T5.taste,E.ci.T5.texture,E.ci.T5.color)

#### MUFFIN ####
## muffin==appearance
E.ci.T5.appearance<-data.frame(n=integer(),
                               food=numeric(),
                               gender=numeric(),
                               variable=numeric(),
                               lower=numeric(),
                               mean=numeric(),
                               upper=numeric())

for (i in 1:160) {
  
  samplen<-function(n){
    E<-c()
    for(i in 1:500){
      x<-sample(muffin$appearance[muffin$gender==2], n, replace = F) # change the parameters for others subgroups
      theta <- function(x){mean(x)}
      results <- bootstrap(x, 1000, theta)
      E[i]<-(quantile(results$thetastar, probs=.975 ,type=3, names=F)-quantile(results$thetastar, probs=.025 ,type=3, names=F))/2
    }
    return(CI(E))
  }
  
  E.ci.T5.appearance[i,]<-c(i,"muffin","female","appearance",samplen(i))
  
  i=i+1
  if (i > length(muffin$appearance[muffin$gender==2])) break()
  
}

## muffin==aroma
E.ci.T5.aroma<-data.frame(n=integer(),
                          food=numeric(),
                          gender=numeric(),
                          variable=numeric(),
                          lower=numeric(),
                          mean=numeric(),
                          upper=numeric())

for (i in 1:160) {
  
  samplen<-function(n){
    E<-c()
    for(i in 1:500){
      x<-sample(muffin$aroma[muffin$gender==2], n, replace = F) # change the parameters for others subgroups
      theta <- function(x){mean(x)}
      results <- bootstrap(x, 1000, theta)
      E[i]<-(quantile(results$thetastar, probs=.975 ,type=3, names=F)-quantile(results$thetastar, probs=.025 ,type=3, names=F))/2
    }
    return(CI(E))
  }
  
  E.ci.T5.aroma[i,]<-c(i,"muffin","female","aroma",samplen(i))
  
  i=i+1
  if (i > length(muffin$aroma[muffin$gender==2])) break()
  
}

## muffin==taste
E.ci.T5.taste<-data.frame(n=integer(),
                          food=numeric(),
                          gender=numeric(),
                          variable=numeric(),
                          lower=numeric(),
                          mean=numeric(),
                          upper=numeric())

for (i in 1:160) {
  
  samplen<-function(n){
    E<-c()
    for(i in 1:500){
      x<-sample(muffin$taste[muffin$gender==2], n, replace = F) # change the parameters for others subgroups
      theta <- function(x){mean(x)}
      results <- bootstrap(x, 1000, theta)
      E[i]<-(quantile(results$thetastar, probs=.975 ,type=3, names=F)-quantile(results$thetastar, probs=.025 ,type=3, names=F))/2
    }
    return(CI(E))
  }
  
  E.ci.T5.taste[i,]<-c(i,"muffin","female","taste",samplen(i))
  
  i=i+1
  if (i > length(muffin$taste[muffin$gender==2])) break()
  
}

## muffin==texture
E.ci.T5.texture<-data.frame(n=integer(),
                            food=numeric(),
                            gender=numeric(),
                            variable=numeric(),
                            lower=numeric(),
                            mean=numeric(),
                            upper=numeric())

for (i in 1:160) {
  
  samplen<-function(n){
    E<-c()
    for(i in 1:500){
      x<-sample(muffin$texture[muffin$gender==2], n, replace = F) # change the parameters for others subgroups
      theta <- function(x){mean(x)}
      results <- bootstrap(x, 1000, theta)
      E[i]<-(quantile(results$thetastar, probs=.975 ,type=3, names=F)-quantile(results$thetastar, probs=.025 ,type=3, names=F))/2
    }
    return(CI(E))
  }
  
  E.ci.T5.texture[i,]<-c(i,"muffin","female","texture",samplen(i))
  
  i=i+1
  if (i > length(muffin$texture[muffin$gender==2])) break()
  
}

## muffin==color
E.ci.T5.color<-data.frame(n=integer(),
                          food=numeric(),
                          gender=numeric(),
                          variable=numeric(),
                          lower=numeric(),
                          mean=numeric(),
                          upper=numeric())

for (i in 1:160) {
  
  samplen<-function(n){
    E<-c()
    for(i in 1:500){
      x<-sample(muffin$color[muffin$gender==2], n, replace = F) # change the parameters for others subgroups
      theta <- function(x){mean(x)}
      results <- bootstrap(x, 1000, theta)
      E[i]<-(quantile(results$thetastar, probs=.975 ,type=3, names=F)-quantile(results$thetastar, probs=.025 ,type=3, names=F))/2
    }
    return(CI(E))
  }
  
  E.ci.T5.color[i,]<-c(i,"muffin","female","color",samplen(i))
  
  i=i+1
  if (i > length(muffin$color[muffin$gender==2])) break()
  
}

## muffin==acceptance
E.ci.T5.acceptance<-data.frame(n=integer(),
                               food=numeric(),
                               gender=numeric(),
                               variable=numeric(),
                               lower=numeric(),
                               mean=numeric(),
                               upper=numeric())

for (i in 1:160) {
  
  samplen<-function(n){
    E<-c()
    for(i in 1:500){
      x<-sample(muffin$acceptance[muffin$gender==2], n, replace = F) # change the parameters for others subgroups
      theta <- function(x){mean(x)}
      results <- bootstrap(x, 1000, theta)
      E[i]<-(quantile(results$thetastar, probs=.975 ,type=3, names=F)-quantile(results$thetastar, probs=.025 ,type=3, names=F))/2
    }
    return(CI(E))
  }
  
  E.ci.T5.acceptance[i,]<-c(i,"muffin","female","acceptance",samplen(i))
  
  i=i+1
  if (i > length(muffin$acceptance[muffin$gender==2])) break()
  
}


#### combining dataframes muffin
E.ci.table5.muffin.M<-rbind(E.ci.T5.appearance,E.ci.T5.aroma,E.ci.T5.taste,E.ci.T5.texture,E.ci.T5.color)

#### PANCAKE ####

## pancake==appearance
E.ci.T5.appearance<-data.frame(n=integer(),
                               food=numeric(),
                               gender=numeric(),
                               variable=numeric(),
                               lower=numeric(),
                               mean=numeric(),
                               upper=numeric())

for (i in 1:160) {
  
  samplen<-function(n){
    E<-c()
    for(i in 1:500){
      x<-sample(pancake$appearance[pancake$gender==2], n, replace = F) # change the parameters for others subgroups
      theta <- function(x){mean(x)}
      results <- bootstrap(x, 1000, theta)
      E[i]<-(quantile(results$thetastar, probs=.975 ,type=3, names=F)-quantile(results$thetastar, probs=.025 ,type=3, names=F))/2
    }
    return(CI(E))
  }
  
  E.ci.T5.appearance[i,]<-c(i,"pancake","female","appearance",samplen(i))
  
  i=i+1
  if (i > length(pancake$appearance[pancake$gender==2])) break()
  
}

## pancake==aroma
E.ci.T5.aroma<-data.frame(n=integer(),
                          food=numeric(),
                          gender=numeric(),
                          variable=numeric(),
                          lower=numeric(),
                          mean=numeric(),
                          upper=numeric())

for (i in 1:160) {
  
  samplen<-function(n){
    E<-c()
    for(i in 1:500){
      x<-sample(pancake$aroma[pancake$gender==2], n, replace = F) # change the parameters for others subgroups
      theta <- function(x){mean(x)}
      results <- bootstrap(x, 1000, theta)
      E[i]<-(quantile(results$thetastar, probs=.975 ,type=3, names=F)-quantile(results$thetastar, probs=.025 ,type=3, names=F))/2
    }
    return(CI(E))
  }
  
  E.ci.T5.aroma[i,]<-c(i,"pancake","female","aroma",samplen(i))
  
  i=i+1
  if (i > length(pancake$aroma[pancake$gender==2])) break()
  
}

## pancake==taste
E.ci.T5.taste<-data.frame(n=integer(),
                          food=numeric(),
                          gender=numeric(),
                          variable=numeric(),
                          lower=numeric(),
                          mean=numeric(),
                          upper=numeric())

for (i in 1:160) {
  
  samplen<-function(n){
    E<-c()
    for(i in 1:500){
      x<-sample(pancake$taste[pancake$gender==2], n, replace = F) # change the parameters for others subgroups
      theta <- function(x){mean(x)}
      results <- bootstrap(x, 1000, theta)
      E[i]<-(quantile(results$thetastar, probs=.975 ,type=3, names=F)-quantile(results$thetastar, probs=.025 ,type=3, names=F))/2
    }
    return(CI(E))
  }
  
  E.ci.T5.taste[i,]<-c(i,"pancake","female","taste",samplen(i))
  
  i=i+1
  if (i > length(pancake$taste[pancake$gender==2])) break()
  
}

## pancake==texture
E.ci.T5.texture<-data.frame(n=integer(),
                            food=numeric(),
                            gender=numeric(),
                            variable=numeric(),
                            lower=numeric(),
                            mean=numeric(),
                            upper=numeric())

for (i in 1:160) {
  
  samplen<-function(n){
    E<-c()
    for(i in 1:500){
      x<-sample(pancake$texture[pancake$gender==2], n, replace = F) # change the parameters for others subgroups
      theta <- function(x){mean(x)}
      results <- bootstrap(x, 1000, theta)
      E[i]<-(quantile(results$thetastar, probs=.975 ,type=3, names=F)-quantile(results$thetastar, probs=.025 ,type=3, names=F))/2
    }
    return(CI(E))
  }
  
  E.ci.T5.texture[i,]<-c(i,"pancake","female","texture",samplen(i))
  
  i=i+1
  if (i > length(pancake$texture[pancake$gender==2])) break()
  
}

## pancake==color
E.ci.T5.color<-data.frame(n=integer(),
                          food=numeric(),
                          gender=numeric(),
                          variable=numeric(),
                          lower=numeric(),
                          mean=numeric(),
                          upper=numeric())

for (i in 1:160) {
  
  samplen<-function(n){
    E<-c()
    for(i in 1:500){
      x<-sample(pancake$color[pancake$gender==2], n, replace = F) # change the parameters for others subgroups
      theta <- function(x){mean(x)}
      results <- bootstrap(x, 1000, theta)
      E[i]<-(quantile(results$thetastar, probs=.975 ,type=3, names=F)-quantile(results$thetastar, probs=.025 ,type=3, names=F))/2
    }
    return(CI(E))
  }
  
  E.ci.T5.color[i,]<-c(i,"pancake","female","color",samplen(i))
  
  i=i+1
  if (i > length(pancake$color[pancake$gender==2])) break()
  
}

## pancake==acceptance
E.ci.T5.acceptance<-data.frame(n=integer(),
                               food=numeric(),
                               gender=numeric(),
                               variable=numeric(),
                               lower=numeric(),
                               mean=numeric(),
                               upper=numeric())

for (i in 1:160) {
  
  samplen<-function(n){
    E<-c()
    for(i in 1:500){
      x<-sample(pancake$acceptance[pancake$gender==2], n, replace = F) # change the parameters for others subgroups
      theta <- function(x){mean(x)}
      results <- bootstrap(x, 1000, theta)
      E[i]<-(quantile(results$thetastar, probs=.975 ,type=3, names=F)-quantile(results$thetastar, probs=.025 ,type=3, names=F))/2
    }
    return(CI(E))
  }
  
  E.ci.T5.acceptance[i,]<-c(i,"pancake","female","acceptance",samplen(i))
  
  i=i+1
  if (i > length(pancake$acceptance[pancake$gender==2])) break()
  
}


#### combining dataframes pancake
E.ci.table5.pancake.M<-rbind(E.ci.T5.appearance,E.ci.T5.aroma,E.ci.T5.taste,E.ci.T5.texture,E.ci.T5.color)


#### SFIHA ####

## sfiha==appearance
E.ci.T5.appearance<-data.frame(n=integer(),
                               food=numeric(),
                               gender=numeric(),
                               variable=numeric(),
                               lower=numeric(),
                               mean=numeric(),
                               upper=numeric())

for (i in 1:160) {
  
  samplen<-function(n){
    E<-c()
    for(i in 1:500){
      x<-sample(sfiha$appearance[sfiha$gender==2], n, replace = F) # change the parameters for others subgroups
      theta <- function(x){mean(x)}
      results <- bootstrap(x, 1000, theta)
      E[i]<-(quantile(results$thetastar, probs=.975 ,type=3, names=F)-quantile(results$thetastar, probs=.025 ,type=3, names=F))/2
    }
    return(CI(E))
  }
  
  E.ci.T5.appearance[i,]<-c(i,"sfiha","female","appearance",samplen(i))
  
  i=i+1
  if (i > length(sfiha$appearance[sfiha$gender==2])) break()
  
}

## sfiha==aroma
E.ci.T5.aroma<-data.frame(n=integer(),
                          food=numeric(),
                          gender=numeric(),
                          variable=numeric(),
                          lower=numeric(),
                          mean=numeric(),
                          upper=numeric())

for (i in 1:160) {
  
  samplen<-function(n){
    E<-c()
    for(i in 1:500){
      x<-sample(sfiha$aroma[sfiha$gender==2], n, replace = F) # change the parameters for others subgroups
      theta <- function(x){mean(x)}
      results <- bootstrap(x, 1000, theta)
      E[i]<-(quantile(results$thetastar, probs=.975 ,type=3, names=F)-quantile(results$thetastar, probs=.025 ,type=3, names=F))/2
    }
    return(CI(E))
  }
  
  E.ci.T5.aroma[i,]<-c(i,"sfiha","female","aroma",samplen(i))
  
  i=i+1
  if (i > length(sfiha$aroma[sfiha$gender==2])) break()
  
}

## sfiha==taste
E.ci.T5.taste<-data.frame(n=integer(),
                          food=numeric(),
                          gender=numeric(),
                          variable=numeric(),
                          lower=numeric(),
                          mean=numeric(),
                          upper=numeric())

for (i in 1:160) {
  
  samplen<-function(n){
    E<-c()
    for(i in 1:500){
      x<-sample(sfiha$taste[sfiha$gender==2], n, replace = F) # change the parameters for others subgroups
      theta <- function(x){mean(x)}
      results <- bootstrap(x, 1000, theta)
      E[i]<-(quantile(results$thetastar, probs=.975 ,type=3, names=F)-quantile(results$thetastar, probs=.025 ,type=3, names=F))/2
    }
    return(CI(E))
  }
  
  E.ci.T5.taste[i,]<-c(i,"sfiha","female","taste",samplen(i))
  
  i=i+1
  if (i > length(sfiha$taste[sfiha$gender==2])) break()
  
}

## sfiha==texture
E.ci.T5.texture<-data.frame(n=integer(),
                            food=numeric(),
                            gender=numeric(),
                            variable=numeric(),
                            lower=numeric(),
                            mean=numeric(),
                            upper=numeric())

for (i in 1:160) {
  
  samplen<-function(n){
    E<-c()
    for(i in 1:500){
      x<-sample(sfiha$texture[sfiha$gender==2], n, replace = F) # change the parameters for others subgroups
      theta <- function(x){mean(x)}
      results <- bootstrap(x, 1000, theta)
      E[i]<-(quantile(results$thetastar, probs=.975 ,type=3, names=F)-quantile(results$thetastar, probs=.025 ,type=3, names=F))/2
    }
    return(CI(E))
  }
  
  E.ci.T5.texture[i,]<-c(i,"sfiha","female","texture",samplen(i))
  
  i=i+1
  if (i > length(sfiha$texture[sfiha$gender==2])) break()
  
}

## sfiha==color
E.ci.T5.color<-data.frame(n=integer(),
                          food=numeric(),
                          gender=numeric(),
                          variable=numeric(),
                          lower=numeric(),
                          mean=numeric(),
                          upper=numeric())

for (i in 1:160) {
  
  samplen<-function(n){
    E<-c()
    for(i in 1:500){
      x<-sample(sfiha$color[sfiha$gender==2], n, replace = F) # change the parameters for others subgroups
      theta <- function(x){mean(x)}
      results <- bootstrap(x, 1000, theta)
      E[i]<-(quantile(results$thetastar, probs=.975 ,type=3, names=F)-quantile(results$thetastar, probs=.025 ,type=3, names=F))/2
    }
    return(CI(E))
  }
  
  E.ci.T5.color[i,]<-c(i,"sfiha","female","color",samplen(i))
  
  i=i+1
  if (i > length(sfiha$color[sfiha$gender==2])) break()
  
}

## sfiha==acceptance
E.ci.T5.acceptance<-data.frame(n=integer(),
                               food=numeric(),
                               gender=numeric(),
                               variable=numeric(),
                               lower=numeric(),
                               mean=numeric(),
                               upper=numeric())

for (i in 1:160) {
  
  samplen<-function(n){
    E<-c()
    for(i in 1:500){
      x<-sample(sfiha$acceptance[sfiha$gender==2], n, replace = F) # change the parameters for others subgroups
      theta <- function(x){mean(x)}
      results <- bootstrap(x, 1000, theta)
      E[i]<-(quantile(results$thetastar, probs=.975 ,type=3, names=F)-quantile(results$thetastar, probs=.025 ,type=3, names=F))/2
    }
    return(CI(E))
  }
  
  E.ci.T5.acceptance[i,]<-c(i,"sfiha","female","acceptance",samplen(i))
  
  i=i+1
  if (i > length(sfiha$acceptance[sfiha$gender==2])) break()
  
}


#### combining dataframes sfiha
E.ci.table5.sfiha.M<-rbind(E.ci.T5.appearance,E.ci.T5.aroma,E.ci.T5.taste,E.ci.T5.texture,E.ci.T5.color,E.ci.T5.acceptance)

E.ci.table5.female<-rbind(E.ci.table5.bread.M,
                        E.ci.table5.sfiha.M,
                        E.ci.table5.cookie.M,
                        E.ci.table5.muffin.M,
                        E.ci.table5.pancake.M)



save(E.ci.table5.female, file = "EciTable5_female.RData")

rm(list = ls())
