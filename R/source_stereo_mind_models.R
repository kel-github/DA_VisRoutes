# use this code to source model fitting commands stored
# in separate R files (see below)

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

msv_fnms <- c('stereo_winplusmind')
rfs <- c("stereo_mod-fxbdmnd_bdrfx.R", "stereo_mod-fxbdmnd_bmndint_bdrfx.R", 
         "stereo_mod-fxbdmnd_dmndint_bdrfx.R", "stereo_mod-fxbdmnd_bdmndint_bdrfx.R")
app <- c(NA, "_bmindint", '_dmindint', '_bdmindint')


###------------------------------------------------------
# load data
###-----------------------------------------------------
load('../data/derivatives/dat4_seq_model.Rda')
load('../data/derivatives/mind_scores.Rda')
sub_var_dat$sub <- as.factor(sub_var_dat$sub)
mind_sum$sub <- as.factor(mind_sum$sub)
sub_var_dat <- inner_join(sub_var_dat, mind_sum, by="sub")
sub_var_dat$drug <- as.factor(sub_var_dat$drug) # makes no difference if fct or chr
sub_var_dat$m <- scale(sub_var_dat$m)
sub_var_dat <- sub_var_dat[!is.na(sub_var_dat$v),]
  
for (j in 1:length(rfs)){
  if (is.na(app[j])){
    dir_name <- paste(msv_fnms, sep="")
  } else {
    dir_name <- paste(msv_fnms, app[j], sep="")
  }
  mod_name <- dir_name
  source(rfs[j])
}

###------------------------------------------------------
# perform model comparisons
###-----------------------------------------------------
ftmplt = '../data/derivatives/%s/%s.Rda'
load(file=sprintf(ftmplt, 'stereo_model-fxbdrg-brfx', 'stereo_model-fxbdrg-brfx'))
load(file=sprintf(ftmplt, 'stereo_winplusmind', 'stereo_winplusmind'))
load(file=sprintf(ftmplt, 'stereo_winplusmind_bmindint', 'stereo_winplusmind_bmindint'))
load(file=sprintf(ftmplt, 'stereo_winplusmind_dmindint', 'stereo_winplusmind_dmindint'))
load(file=sprintf(ftmplt, 'stereo_winplusmind_bdmindint', 'stereo_winplusmind_bdmindint'))

loo_compare(fxbdrg_bsubrfx, mndbd, mndbd_bm, mndbd_dm, mndbd_bdm)
# elpd_diff se_diff
# mndbd_bdm       0.0       0.0   
# mndbd_bm       -0.1       3.3   
# mndbd          -0.1       3.3   
# mndbd_dm       -0.3       2.5   
# fxbdrg_bsubrfx -0.7       3.6   