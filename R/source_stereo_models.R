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

# fxbdrg_bsubrfx         0.0       0.0  
# fxbdrgint_bsubrfx     -1.0       0.7  
# fxb_bsubrfx           -1.0       2.1  
# bsubrfx               -1.8       2.7  
# fxbdrg_rfxbdrg        -2.1       2.1  
# fxbdrgint_bdrgsubrfx  -2.6       2.3  
# fxb_subint           -68.1      13.9 
