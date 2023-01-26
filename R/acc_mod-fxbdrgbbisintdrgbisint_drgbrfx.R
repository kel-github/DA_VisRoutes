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


# ###------------------------------------------------------
# # load packages
# ###-----------------------------------------------------
# library(brms)
# library(tidyverse)

###------------------------------------------------------
# have you run this model before?
###-----------------------------------------------------
# new <- TRUE
# verbal <- FALSE
dir.create(sprintf('../data/derivatives/%s', dir_name), showWarnings=FALSE)

if (new){

  if (faux){
    ###------------------------------------------------------
    # generate data for a ffx of drug & mind, and an b*sub rfx
    ###-----------------------------------------------------
    nsubs <- 40
    intercept = 0.5
    b <- scale(1:8)
    nb <- 8
    drug_levels <- 2
    drug_reg <- rep(c(0,1), each=nsubs*nb)
    drug_fx <- .2
    b <- rep(b, times=nsubs*drug_levels) # adding drug manipulation
    betab <- rep(rnorm(nsubs, mean=.5, sd=.2), each=nb, times=drug_levels)
    sub_int <- rep(rnorm(nsubs, mean=.2, sd=.4), each=nb, times=drug_levels)
    mind_scores <- rep(rnorm(nsubs), each=nb, times=drug_levels)
    mind_beta <- 2
    
    log_odds <- intercept + betab*b + drug_fx*drug_reg + mind_scores*mind_beta + sub_int
    # now turn mu into p
    p <- 1/(1+exp(-log_odds))
    # now sample binomial distribution
    trials <- sample(acc_dat$td, size=length(b))
    tt <- mapply(function(x,y) rbinom(1,size=x,prob=y), trials, p)
    faux_dat <- tibble(sub = as.factor(rep(1:nsubs, each=nb, times=2)),
                       drug = drug_reg,
                       mnd = mind_scores,
                       b = b,
                       tt = tt, 
                       td = trials)
    
    faux_fxdrgmnd_bsubrfx <- brm(formula = tt | trials(td) ~ drug + mnd + (b|sub),
                                 data = faux_dat,
                                 warmup = 2000, iter = 10000,
                                 family = binomial,
                                 save_pars = save_pars(all=TRUE)) # for model comparisons 
    
    summary(faux_fxdrgmnd_bsubrfx) # this looks good to me, time 
  }
  
  ###------------------------------------------------------
  # define accuracy models
  ###-----------------------------------------------------
  # start with an effect of block and a subject intercept

  bisbd <- brm(formula = tt | trials(td) ~ b + drug + bis + b:bis + drug:bis + (b:drug|sub),
                   data = acc_dat,
                   warmup = 2000, iter = 10000,
                   family = binomial,
                   save_pars = save_pars(all=TRUE)) # for model comparisons 
  bisbd <- add_criterion(bisbd, "loo",  moment_match=TRUE, reloo=TRUE)
  
  # now save!
  save.image(file = sprintf('../data/derivatives/%s/%s.Rda', dir_name, mod_name))
  
} else {
  
  load(file = sprintf('../data/derivatives/%s/%s.Rda', dir_name, mod_name))
}

if (verbal){
  # info
 # prior_summary(mnd)

  verbal_output(bisbd, dir_name = dir_name)
}

rm(bisbd)
