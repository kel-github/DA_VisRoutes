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
msv_fnms <- c('acc_winplusbis', 'cacc_winplusbis') # save names

for (i in 1:length(data_names)){
  
  ###------------------------------------------------------
  # load data
  ###-----------------------------------------------------
  load(sprintf('../data/derivatives/%s_dat4_model.Rda', data_names[i]))
  load('../data/derivatives/bis_scores.Rda')
  bis_sum$sub <- as.factor(bis_sum$sub)
  acc_dat <- inner_join(acc_dat, bis_sum, by="sub")
  acc_dat$bis <- scale(acc_dat$bis)
  
  dir_name <- msv_fnms[i]
  mod_name <- dir_name
  
  source('acc_mod-fxbdrgbis_drgbrfx.R')
  
  dir_name <- paste(dir_name, 'bbisint', sep="")
  mod_name <- dir_name
  source('acc_mod-fxbdrgbbisint_drgbrfx.R')

}


# for accuracy data, allow BIS to interact with drug
i = 1
dir_name <- paste(msv_fnms[i], 'bdbisint', sep="")
mod_name <- dir_name
source('acc_mod-fxbdrgbbisintdrgbisint_drgbrfx.R')

dir_name <- paste(msv_fnms[i], 'plus3way', sep="")
mod_name <- dir_name
source('acc_mod-fxbdrgbdrgbisint_drgbrfx.R')



