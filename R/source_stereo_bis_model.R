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

msv_fnms <- c('stereo_winplusbis')
rfs <- c("stereo_mod-fxbdmndbis_bmndint_brfx.R")
app <- c(NA)

###------------------------------------------------------
# load data
###-----------------------------------------------------
load('../data/derivatives/dat4_seq_model.Rda')
load('../data/derivatives/mind_scores.Rda')
load('../data/derivatives/bis_scores.Rda')

sub_var_dat$sub <- as.factor(sub_var_dat$sub)
mind_sum$sub <- as.factor(mind_sum$sub)
sub_var_dat <- inner_join(sub_var_dat, mind_sum, by="sub")
sub_var_dat$drug <- as.factor(sub_var_dat$drug) # makes no difference if fct or chr
sub_var_dat$m <- scale(sub_var_dat$m)
bis_sum$sub <- as.factor(bis_sum$sub)
sub_var_dat <- inner_join(sub_var_dat, bis_sum, by="sub")
sub_var_dat$bis <- scale(sub_var_dat$bis)

  
dir_name <- paste(msv_fnms, sep="")
mod_name <- dir_name
source(rfs)


###------------------------------------------------------
# perform model comparisons
###-----------------------------------------------------
ftmplt = '../data/derivatives/%s/%s.Rda'
load(file=sprintf(ftmplt, 'stereo_winplusmind_bmindint', 'stereo_winplusmind_bmindint'))
load(file=sprintf(ftmplt, 'stereo_winplusbis', 'stereo_winplusbis'))

loo_compare(mndbd_bm, mndbdbis_bm)
# elpd_diff se_diff
