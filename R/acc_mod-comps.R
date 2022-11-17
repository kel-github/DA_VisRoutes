## K. Garner, 2022
## perform modelling of accuracy data as specified in 
## https://osf.io/2y6pk
####------------------------------------------------------
# use this code to apply the bayesian multi-level modelling
# of the accuracy data
# in this script I detail the process of model comparisons,
# highlighting any required deviations from https://osf.io/2y6pk
###----------------------------------------------------------
# remember to setwd to source file
rm(list=ls())
# get what you need and settings for loading models
library(brms)
library(tidyverse)

new <- FALSE
verbal <- FALSE

# first step: modelling the beta*b 
# model 1: tt | trials(td) ~  b + (1|sub) - however, the learning rate varies by sub, therefore...
# model 2: tt | trials(td) ~ (b|sub) - to fit each individual's learning rate, because I'm not controlling this
# model 3: tt | trials(td) ~ b + (b|sub) 
load('../data/derivatives/acc_mod-bonly/acc_mod-bonly.Rda')
load('../data/derivatives/acc_mod-int-bsubrfx/acc_mod-int-bsubrfx.Rda')
load('../data/derivatives/acc_mod-fxb-bsubrfx/acc_mod-fxb_bsubrfx.Rda')

b_subint_loo <- add_criterion(b_subint, "loo", moment_match=TRUE)
bsubrfx_loo <- add_criterion(bsubrfx, "loo", moment_match=TRUE) 
fxb_bsubrfx_loo <- add_criterion(fxb_bsubrfx, "loo", moment_match=TRUE)

loo_compare(b_subint_loo, bsubrfx_loo, fxb_bsubrfx_loo, criterion = "loo") # bsubrfx_loo is the preferred model

loo(b_subint, moment_match = TRUE) # just to see
loo(bsubrfx, moment_match=TRUE)
# yay! pareto k diagnostics are ok, so am going to keep this one
loo(fxb_bsubrfx_loo, moment_match=TRUE)

save.image(file='../data/derivatives/acc_model-comps.Rda')
# step 2: add drig as a fixed effect and as a random effect
# model 1: tt | trials(td) ~ drug + (b|sub)
# model 2: tt | trials(td) ~ drug + (drug:b|sub)