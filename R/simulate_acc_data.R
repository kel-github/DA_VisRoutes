############################################################################
## written by K. Garner, 2022
## write statistical model for generative process fro acc data, to check its sane
## based on code from chapter 14 of Statistical Rethinking by Richard McElreath
############################################################################

rm(list=ls())

###### statistical model
# tt | trials(td) ~  Binom(p, N) 
# logit(p) ~ beta*b + (1 + sub) + (b|sub)

# get what you need
library(tidyverse)
library(MASS)
library(ellipse)
set.seed(42)

# set params
alpha <- 0.4 # intercept
b <- scale(c(1:8))
beta <- 0.5 
sigma_intercepts <- 1
sigma_slopes <- 0.5
rho <- (-0.7) # covariation between intercepts and slopes

# create multivariate normal
mu <- c(alpha, beta)
sigmas <- c(sigma_intercepts, sigma_slopes) # standard deviations
rhos <- matrix( c(1, rho, rho, 1), nrow=2) # correlation matrix
Sigma <- diag(sigmas) %*% rhos %*% diag(sigmas)

# now simulate subjects with their own intercepts and slopes
n_subs <- 40
# randomly sample from multivariate Gaussian distribution
vary_effects <- mvrnorm(n_subs, mu, Sigma)
intercepts <- vary_effects[,1]
slopes <- vary_effects[,2]

# do some visualisation!
# first plot the parameters against each other
plot(intercepts, slopes, col="blue",
     xlab="intercepts", ylab="slopes")
# overlay population distribution
for (l in c(0.1, 0.3, 0.5, 0.8, 0.99)) 
  lines(ellipse(Sigma, centre=mu, level=l),col="grey")

# now simulate the data
n_samps <- 2
sub <- rep(c(1:n_subs), each=n_samps*length(b))
s <- rep(c(0,1), each=length(b), times=n_subs)
bs <- rep(b, times=n_subs*n_samps)
mu <- intercepts[sub] + slopes[sub]*bs
sigma <- sigma_slopes
real_mu <- rnorm(n_samps*n_subs*length(b), mu, sigma)
p <- 1/(1+exp(-real_mu))
dat <- data.frame(sub=as.factor(sub), s=as.factor(s),
                  b=b,
                  real_mu=real_mu, p=p)

# plot sess 1
dat %>% ggplot(aes(x=b, y=p, group=sub, colour=sub)) +
           geom_line() + facet_wrap(~s)

dat %>% ggplot(aes(x=b, y=real_mu, group=sub, colour=sub)) +
  geom_line() + facet_wrap(~s)
