
rm(list=ls())
library(dplyr) #dataframe management
library(haven) #library for importing data set
library(psych) #library for trace matrix
library(momentfit)


ajr <- read_dta("AJR2001.dta")
ajr <- ajr %>% mutate(logmort0sq = logmort0^2)
Y <- as.matrix(ajr$loggdp)
X <- as.matrix(ajr %>% 
                 select(risk) %>% 
                 bind_cols(data.frame(con=rep(1,nrow(ajr)))))
X.endo <- X[,1]
X.exo <- X[,2]
Z <- as.matrix(ajr %>% 
                 select(logmort0) %>%
                 mutate(logmort0sq = logmort0^2) %>% 
                 bind_cols(data.frame(con=rep(1,nrow(ajr)))))



b2sls <- solve(t(X)%*%Z%*%solve(t(Z)%*%Z)%*%t(Z)%*%X)%*%t(X)%*%Z%*%
  solve(t(Z)%*%Z)%*%t(Z)%*%Y
e_hat2sls <- Y - X %*% b2sls


w <- Z*(e_hat2sls%*%matrix(1,1,ncol(Z)))
O <- t(w)%*%w

bgmm <- solve(t(X)%*%Z%*%solve(O)%*%t(Z)%*%X)%*%(t(X)%*%Z%*%solve(O)%*%t(Z)%*%Y)
bgmm

mod <- momentModel(loggdp ~ risk, ~logmort0 + logmort0sq, data= ajr)
res <- gmm4(loggdp ~ risk, ~logmort0 + logmort0sq, data= ajr,
     type="twostep",vcov = 'CL',initW ='tsls')
summary(res)
