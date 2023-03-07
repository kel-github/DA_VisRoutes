## written by K. Garner, 2022
### use this file to source (i.e. run) all the accuracy models
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

###------------------------------------------------------
# run models
###-----------------------------------------------------
load('../data/derivatives/dat4_seq_model.Rda')
sub_var_dat$sub <- as.factor(sub_var_dat$sub)
sub_var_dat$drug <- as.factor(sub_var_dat$drug)

# (above line already done in load_data etc)
# line with the accuracy data terminology

dir_name <- 'stereo_model-bonly'
source('stereo_mod-bonly.R')

dir_name <- 'stereo_model-fxb-bsubrfx'
source('stereo_mod-fxb_bsubrfx.R')

# make a directory for results if required
dir_name <- 'stereo_model-int-bsubrfx'
source('stereo_mod-int_bsubrfx.R')

dir_name <- 'stereo_model-fxbdrg-brfx'
source('stereo_mod-fxbdrug_brfx.R')

dir_name <- 'stereo_model-fxbdrg-bdrgsubrfx'
source('stereo_mod-fxbdrug_drugbrfx.R')

dir_name <- 'stereo_model-fxbdrgint-bdrgrfx'
source('stereo_mod-fxbdrugint_drgbrfx.R')

dir_name <- 'stereo_model-fxbdrgint-brfx'
source('stereo_mod-fxbdrugint_brfx.R')

###------------------------------------------------------
# compare models
###-----------------------------------------------------
ftmplt = '../data/derivatives/%s/%s.Rda'
load(file=sprintf(ftmplt, 'stereo_model-bonly', 'stereo_model-bonly'))
load(file=sprintf(ftmplt, 'stereo_model-fxb-bsubrfx', 'stereo_model-fxb-bsubrfx'))
load(file=sprintf(ftmplt, 'stereo_model-int-bsubrfx', 'stereo_model-int-bsubrfx'))
load(file=sprintf(ftmplt, 'stereo_model-fxbdrg-brfx', 'stereo_model-fxbdrg-brfx'))
load(file=sprintf(ftmplt, 'stereo_model-fxbdrg-bdrgsubrfx', 'stereo_model-fxbdrg-bdrgsubrfx'))
load(file=sprintf(ftmplt, 'stereo_model-fxbdrgint-bdrgrfx', 'stereo_model-fxbdrgint-bdrgrfx'))
load(file=sprintf(ftmplt, 'stereo_model-fxbdrgint-brfx', 'stereo_model-fxbdrgint-brfx'))

loo_compare(fxb_subint, bsubrfx, fxb_bsubrfx, fxbdrg_bsubrfx, fxbdrg_rfxbdrg, 
            fxbdrgint_bdrgsubrfx, fxbdrgint_bsubrfx) #,

# fxbdrg_rfxbdrg          0.0       0.0 # 'stereo_model-fxbdrg-bdrgsubrfx'
# fxbdrgint_bdrgsubrfx   -0.9       0.3 
# fxbdrg_bsubrfx       -113.2      16.2 
# fxb_bsubrfx          -113.2      16.6 
# fxbdrgint_bsubrfx    -113.5      16.2 
# bsubrfx              -114.5      16.5 
# fxb_subint           -188.8      18.6 
