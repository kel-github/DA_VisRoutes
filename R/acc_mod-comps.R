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

run_b <- FALSE
run_d <- TRUE# run drug models

if (run_b){
  ##################################################################################################################
  # first step: modelling the beta*b 
  # model 1: tt | trials(td) ~  b + (1|sub) - however, the learning rate varies by sub, therefore...
  # model 2: tt | trials(td) ~ (b|sub) - to fit each individual's learning rate, because I'm not controlling this
  # model 3: tt | trials(td) ~ b + (b|sub) 
  load('../data/derivatives/acc_model-bonly/acc_model-bonly.Rda')
  load('../data/derivatives/acc_model-int-bsubrfx/acc_model-int-bsubrfx.Rda')
  load('../data/derivatives/acc_model-fxb-bsubrfx/acc_model-fxb-bsubrfx.Rda')
  
  fxb_subint <- add_criterion(fxb_subint, "loo", moment_match=TRUE, reloo=TRUE)
  bsubrfx <- add_criterion(bsubrfx, "loo", moment_match=TRUE, reloo=TRUE) 
  fxb_bsubrfx <- add_criterion(fxb_bsubrfx, "loo", moment_match=TRUE, reloo=TRUE)
  
  
  loo_compare(fxb_subint,  bsubrfx, fxb_bsubrfx, criterion = "loo") # bsubrfx_loo is the preferred model

  save(fxb_subint, file='../data/derivatives/acc_mod-bonly/acc_mod-bonly.Rda')
  save(bsubrfx, file='../data/derivatives/acc_model-int-bsubrfx/acc_model-int-bsubrfx.Rda')
  save(fxb_bsubrfx, file='../data/derivatives/acc_model-fxb-bsubrfx/acc_model-fxb-bsubrfx.Rda')
  
  # fxb_bsubrfx is now the preferred model (just), but I think it makes more sense
} else {
 # load('../data/derivatives/acc_model-comps.Rda')
}
##################################################################################################################
# step 2: add drig as a fixed effect and as a random effect
# my plan is to add it to both the bfx and brfx models, as
# there was only 1 influential value in the bfx, and this may
# improve as the drug term is added to the model
# 
# if (run_d){

#   # model 1: tt | trials(td) ~ b + drug + (b|sub)
#   # model 2: tt | trials(td) ~ b + drug + (b:drug|sub)
   load('../data/derivatives/acc_model-fxbdrg-brfx/acc_model-fxbdrg-brfx.Rda')
   load('../data/derivatives/acc_model-fxbdrg-bdrgsubrfx/acc_model-fxbdrg-bdrgsubrfx.Rda')
#   
   fxbdrg_bsubrfx <- add_criterion(fxbdrg_bsubrfx, "loo", moment_match=TRUE, reloo=TRUE)
   fxbdrg_rfxbdrg <- add_criterion(fxbdrg_rfxbdrg, "loo", moment_match=TRUE, reloo=TRUE)

   loo_compare(fxbdrg_bsubrfx, fxbdrg_rfxbdrg, criterion="loo")
   
   save(fxbdrg_bsubrfx, file='../data/derivatives/acc_model-fxbdrg-brfx/acc_model-fxbdrg-brfx.Rda')
   save(fxbdrg_rfxbdrg, file='../data/derivatives/acc_model-fxbdrg-bdrgsubrfx/acc_model-fxbdrg-bdrgsubrfx.Rda')  
#   save.image(file='../data/derivatives/acc_model-comps.Rda')
# } else {
#   
#   load('../data/derivatives/acc_model-comps.Rda')
# }

