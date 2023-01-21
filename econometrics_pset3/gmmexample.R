rm(list=ls())
library(gmm)
library(modelsummary)

# Parameters we are going to estimate
mu = 3
sigma = 2

# Generating random numbers
set.seed(0219)
n = 500
x = rnorm(n = n, mean = mu, sd = sigma)

# Moment restrictions
g1 <- function(theta, x) {
  m1 = (theta[1]-x)
  m2 = (theta[2]^2 - (x - theta[1])^2)
  m3 = x^3-theta[1]*(theta[1]^2+3*theta[2]^2)
  f = cbind(m1,m2,m3)
  return(f)
}

# Running GMM
gmm_mod = gmm(
  # Moment restriction equations
  g = g1,
  # Matrix of data
  x = x,
  # Starting location for minimization algorithm
  t0 = c(0,0) # Required when g argument is a function
)

# Reporting results
summary(gmm_mod)


g2 <- function(theta, x) {
  m1 = (x-theta[1])
  m2 = (x^2 - theta[1]^2 - theta[2]^2)
  m3 = x^3-theta[1]*(theta[1]^2+3*theta[2]^2)
  f = cbind(m1,m2,m3)
  return(f)
}

# Running GMM
gmm_mod2 = gmm(
  # Moment restriction equations
  g = g2,
  # Matrix of data
  x = x,
  # Starting location for minimization algorithm
  t0 = c(0,0), # Required when g argument is a function
  type = 'iterative'
)

# Reporting results
summary(gmm_mod2)



# gmm for linear ----------------------------------------------------------

# Setting parameter values
alpha = 1
beta = 2

# Taking random draws
set.seed(0219)
z1 = rnorm(n = 500, 1,2)
z2 = rnorm(n = 500,-1,1)
e = rnorm(n = 500, 0, 1)

# Collecting instruments
Z = cbind(z1, z2)

# Specifying model, where x is endogenous
x = z1 + z2 + e
y = alpha + beta * x + e

# Running GMM
lin_gmm_mod = gmm(
  g = y ~ x,
  x = Z
)

# Reporting results
summary(lin_gmm_mod)
