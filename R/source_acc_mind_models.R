rm(list=ls())
###------------------------------------------------------
# load packages
###-----------------------------------------------------
library(brms)
library(tidyverse)
source('verbal.R')

new <- TRUE
verbal <- TRUE
faux <- FALSE

data_names <- c('acc', 'cacc')
#mod_rnms <- c('acc_mod-fxdrgmnd_drgbrfx', 'cacc_mod-fxdrgmnd_drgbrfx')
msv_fnms <- c('acc_winplusmind', 'cacc_winplusmind') # save names

for (i in 1:length(data_names)){
  
  ###------------------------------------------------------
  # load data
  ###-----------------------------------------------------
  load(sprintf('../data/derivatives/%s_dat4_model.Rda', data_names[i]))
  load('../data/derivatives/mind_scores.Rda')
  mind_sum$sub <- as.factor(mind_sum$sub)
  acc_dat <- inner_join(acc_dat, mind_sum, by="sub")
  acc_dat$m <- scale(acc_dat$m)
  
  dir_name <- msv_fnms[i]
  mod_name <- dir_name
  
  source('acc_mod-fxbdrgmnd_drgbrfx.R')
  
  dir_name <- paste(dir_name, 'bmint', sep="")
  mod_name <- dir_name
  source('acc_mod-fxbdrgbmndint_drgbrfx.R')
}

# now load models and compare
# accuracy data
# WHERE IS THE BEST ACCURACY MODEL
load('../data/derivatives/cacc_model-fxbdrg-bdrgsubrfx/cacc_model-fxbdrg-bdrgsubrfx.Rda')
load("../data/derivatives/cacc_winplusmind/cacc_winplusmind.Rda")
load("../data/derivatives/cacc_winplusmindbmint/cacc_winplusmindbmint.Rda")

loo_compare(fxbdrg_rfxbdrg, mnd, mndb)

plot(mndb)

## summary of results:
## for accuracy; the winning models are the ones without mindfulness
## for contextual accuracy: the winning model has a 
## mindfulness + mindfulness*block effect. The next step is to see if
## drug and mindfulness interact?

## next step - does mindfulness also interact with drug for cacc?
