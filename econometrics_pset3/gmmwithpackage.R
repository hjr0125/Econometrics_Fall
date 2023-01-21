
rm(list=ls())
library(dplyr) #dataframe management
library(haven) #library for importing data set
library(psych) #library for trace matrix
library(gmm)


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




# Running GMM
lin_gmm_mod = gmm(
  g = Y ~ X.endo,
  x = Z,type = 'twoStep',traceIter = T,itermax = 100
)


# Reporting results
summary(lin_gmm_mod)
