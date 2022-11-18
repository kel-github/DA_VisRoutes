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

##################################################################################################################
# first step: modelling the beta*b 
# model 1: tt | trials(td) ~  b + (1|sub) - however, the learning rate varies by sub, therefore...
# model 2: tt | trials(td) ~ (b|sub) - to fit each individual's learning rate, because I'm not controlling this
# model 3: tt | trials(td) ~ b + (b|sub) 
load('../data/derivatives/acc_mod-bonly/acc_mod-bonly.Rda')
load('../data/derivatives/acc_mod-int-bsubrfx/acc_mod-int-bsubrfx.Rda')
load('../data/derivatives/acc_mod-fxb-bsubrfx/acc_mod-fxb_bsubrfx.Rda')

b_subint_loo <- add_criterion(b_subint, "loo", moment_match=TRUE)

# Computed from 32000 by 640 log-likelihood matrix
# 
# Estimate    SE
# elpd_loo  -3581.9 117.1
# p_loo       218.8  22.3
# looic      7163.8 234.3
# ------
#   Monte Carlo SE of elpd_loo is 0.1.
# 
# Pareto k diagnostic values:
#   Count Pct.    Min. n_eff
# (-Inf, 0.5]   (good)     630   98.4%   1482      
# (0.5, 0.7]   (ok)        10    1.6%   381       
# (0.7, 1]   (bad)        0    0.0%   <NA>      
#   (1, Inf)   (very bad)   0    0.0%   <NA>      
#   
#   All Pareto k estimates are ok (k < 0.7).
# See help('pareto-k-diagnostic') for details.

bsubrfx_loo <- add_criterion(bsubrfx, "loo", moment_match=TRUE) 

# Computed from 32000 by 640 log-likelihood matrix
# 
# Estimate    SE
# elpd_loo  -3091.6  89.1
# p_loo       245.5  19.0
# looic      6183.2 178.2
# ------
#   Monte Carlo SE of elpd_loo is 0.2.
# 
# Pareto k diagnostic values:
#   Count Pct.    Min. n_eff
# (-Inf, 0.5]   (good)     619   96.7%   1450      
# (0.5, 0.7]   (ok)        21    3.3%   242       
# (0.7, 1]   (bad)        0    0.0%   <NA>      
#   (1, Inf)   (very bad)   0    0.0%   <NA>      
#   
#   All Pareto k estimates are ok (k < 0.7).
# See help('pareto-k-diagnostic') for details.

fxb_bsubrfx_loo <- add_criterion(fxb_bsubrfx, "loo", moment_match=TRUE)

# Computed from 32000 by 640 log-likelihood matrix
# 
# Estimate    SE
# elpd_loo  -3091.7  89.0
# p_loo       245.8  19.2
# looic      6183.4 178.1
# ------
#   Monte Carlo SE of elpd_loo is NA.
# 
# Pareto k diagnostic values:
#   Count Pct.    Min. n_eff
# (-Inf, 0.5]   (good)     614   95.9%   2108      
# (0.5, 0.7]   (ok)        25    3.9%   252       
# (0.7, 1]   (bad)        0    0.0%   <NA>      
#   (1, Inf)   (very bad)   1    0.2%   6         
# See help('pareto-k-diagnostic') for details.

loo_compare(b_subint_loo, bsubrfx_loo, fxb_bsubrfx_loo, criterion = "loo") # bsubrfx_loo is the preferred model
# elpd_diff se_diff
# bsubrfx_loo        0.0       0.0 
# fxb_bsubrfx_loo   -0.1       1.2 
# b_subint_loo    -490.3      87.9

# things to do about bsubrfx_loo
# 1. what is the influential point? can I ID it?
# 2. if p_loo << p, then the model is likely misspecified
# 3. is p>N/5 N = 640, N/5 = 128 p=1 + 40 + 40 = 81

loo(b_subint, moment_match = TRUE) # just to see
loo(bsubrfx, moment_match=TRUE)
# yay! pareto k diagnostics are ok, so am going to keep this one
loo(fxb_bsubrfx_loo, moment_match=TRUE)

save.image(file='../data/derivatives/acc_model-comps.Rda')
##################################################################################################################
# step 2: add drig as a fixed effect and as a random effect
# my plan is to add it to both the bfx and brfx models, as
# there was only 1 influential value in the bfx, and this may
# improve as the drug term is added to the model

# model 1: tt | trials(td) ~ drug + (b|sub)
# model 2: tt | trials(td) ~ drug + (drug:b|sub)
# model 3: tt | trials(td) ~ b + drug + (b|sub)
# model 4: tt | trials(td) ~ b + drug + (b:drug|sub)

load('../data/derivatives/acc_model-fxdrg-bsubrfx/acc_model-fxdrg-bsubrfx.Rda')
load('../data/derivatives/acc_mod-fxdrg-bdrugsubrfx/acc_mod-fxdrg-bdrugsubrfx.Rda')
load('../data/derivatives/acc_mod-fxbdrg-bsubrfx/acc_mod-fxbdrg-bsubrfx.Rda')
load('../data/derivatives/acc_mod-fxbdrg-bdrgsubrfx/acc_mod-fxbdrg-bdrgsubrfx.Rda')

fxdrg_bsubrfx <- add_criterion(fxdrg_bsubrfx, "loo", moment_match=TRUE, reloo=TRUE) #PK ok
fxdrg_bdrugsubrfx <- add_criterion(fxdrg_bdrugsubrfx, "loo", moment_match=TRUE, reloo=TRUE) # 4 very bad DROP! (its not preferred anyway)
fxbdrg_bsubrfx <- add_criterion(fxbdrg_bsubrfx, "loo", moment_match=TRUE, reloo=TRUE) # also ok (pk)
fxbdrg_rfxbdrg <- add_criterion(fxbdrg_rfxbdrg, "loo", moment_match=TRUE, reloo=TRUE) # also ok

loo_compare(fxdrg_bsubrfx, fxbdrg_bsubrfx, fxbdrg_rfxbdrg, criterion = "loo") 
#fxdrg_bsubrfx       0.0       0.0  - this guy was a tad better at recovering parameters also
# Now I'll add mindfulness to this guy
#fxbdrg_bsubrfx     -0.1       1.2  