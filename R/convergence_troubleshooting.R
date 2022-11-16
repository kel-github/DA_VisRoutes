## K. Garner, 2022
## perform modelling of accuracy data as specified in 
## https://osf.io/2y6pk
## troubleshooting convergence issues
####------------------------------------------------------
# use this code to apply the bayesian multi-level modelling
# of the accuracy data
# 
# convergence problems:
# https://psyarxiv.com/xmhfr/
# https://mc-stan.org/misc/warnings.html#divergent-transitions-after-warmup
# https://hyunjimoon.github.io/SBC/articles/small_model_workflow.html
# https://betanalpha.github.io/assets/case_studies/identifiability.html
# https://www.martinmodrak.cz/2018/02/19/taming-divergences-in-stan-models/
# https://mc-stan.org/users/documentation/case-studies/divergences_and_bias.html
# https://arxiv.org/abs/2011.01808
###------------------------------------------------------------------
# https://m-clark.github.io/posts/2020-03-16-convergence/
# step 1: look at the data. Are any regressors collinear with otehrs?
# assess vif factors, check if any of the regressors can be categorised
# eg if they only contain 2 or 3 different types of observations
# check singularity - are any of the random effects effectively zero?
library(car)
library(tidyverse)

load('../data/derivatives/accuracy.Rda')

###------------------------------------------------------
# sum over context, as initial peruse of data showed
# no information in this variable, for accuracy
###-----------------------------------------------------
acc_dat <- door_acc_sum %>% group_by(sub, sess, drug, b) %>%
  summarise(tt = sum(tt),
            td = sum(td))
acc_dat <- acc_dat[!is.na(acc_dat$b),]
acc_dat$drug[acc_dat$drug == "placebo"] <- 0
acc_dat$drug[acc_dat$drug == "levodopa"] <- 1
acc_dat$drug <- as.numeric(acc_dat$drug)
pairs(~sess+drug+b+tt+td, data = acc_dat)


# https://mc-stan.org/misc/warnings.html#divergent-transitions-after-warmup
# worry about divergent transitions when they are large in number, and/or 
# they are combined with high Rhat and low ESS numbers