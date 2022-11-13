## K. Garner, 2022
## perform modelling of accuracy data as specified in 
## https://osf.io/2y6pk
####------------------------------------------------------
# use this code to apply the bayesian multi-level modelling
# of the accuracy data
# 
# assumes following folder structure:
# -- top/
#      R/code is here
#      data/ 
#        derivatives/ # summary data is here
#
#
# RESOURCES:
# https://www.jstatsoft.org/article/view/v080i01
# https://github.com/paul-buerkner/brms
# https://bayesball.github.io/BRMS/multilevel-regression.html
###-------------------------------------------------------


###------------------------------------------------------
# load packages
###-----------------------------------------------------
library(brms)
library(tidyverse)

###------------------------------------------------------
# load data
###-----------------------------------------------------
load('../data/derivatives/accuracy.Rda')

###------------------------------------------------------
# sum over context, as initial peruse of data showed
# no information in this variable, for accuracy
###-----------------------------------------------------
acc_dat <- door_acc_sum %>% group_by(sub, sess, drug, b) %>%
                                  summarise(tt = sum(tt),
                                            td = sum(td))
acc_dat <- acc_dat[!is.na(acc_dat$b),]
# scale the block factor
acc_dat$b <- scale(acc_dat$b)
acc_dat$sub <- as.factor(acc_dat$sub)
acc_dat$sess <- as.factor(acc_dat$sess)

###------------------------------------------------------
# define accuracy models
###-----------------------------------------------------
# start with an effect of block and a subject intercept
priors <- c(
  prior(normal(0, 10), class = Intercept),
  prior(normal(0, 10), class = b),
  prior(cauchy(0, 10), class = sd),
  prior(lkj(2), class=cor)
)

# models
# b + (1 + sub)
# b + (1 + sub) + (b|sub)
b_only <- brm(formula= tt | trials(td) ~  b + (1 + sub) + (b|sub),
                      data = acc_dat,
                      prior = priors,
                      warmup = 2000, iter = 10000,
                      family = binomial,
                      save_pars = save_pars(all=TRUE)) # for model comparisons 
prior_summary(b_only)
# see https://paul-buerkner.github.io/brms/reference/set_prior.html
# for derails on the class etc
# see https://pubmed.ncbi.nlm.nih.gov/31082309/
# for a guide on how to set priors etc
# also need to check how I can do dic comparisons between these models
plot(b_only)
summary(b_only)
b_only <- add_criterion(b_only, "waic")

# now add session, and an intercept per subject for each session
b_sess <- brm(formula= tt | trials(td) ~ b + sess + (1+ sub) + (1 + b|sub) + (1|sess:sub),
                      data = acc_dat,
                      prior = priors,
                      warmup = 2000, iter = 10000,
                      family = binomial,
                      save_pars = save_pars(all=TRUE))
plot(b_sess)
b_sess <- add_criterion(b_sess, "waic")

loo(b_only, b_sess, compare=TRUE, moment_match = TRUE, reloo=TRUE)

save.image(file = '../data/derivatives/acc_dat_models.Rda')
