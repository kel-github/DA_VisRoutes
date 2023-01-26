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

# next, I need both winning models to contain a drug x mindfulness interaction


