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

## move the comparisons code to a different file
## run through the specification of the final cacc + the interaction (see below)
## define and run the BIS models for both acc & cacc
# now load models and compare
# accuracy data


plot(mndb)

## summary of results:
## for accuracy; the winning models are the ones without mindfulness
## for contextual accuracy: the winning model has a 
## mindfulness + mindfulness*block effect. This model is preferred
## to one where drug and mindfulness interact

## next step - does mindfulness also interact with drug for cacc?



