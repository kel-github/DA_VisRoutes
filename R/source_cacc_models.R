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
load('../data/derivatives/cacc_dat4_model.Rda')
# 76/162 = wrong data
# 46/76 = correct dat

dir_name <- 'cacc_model-bonly'
source('acc_mod-bonly.R')

dir_name <- 'cacc_model-fxb-bsubrfx' # load this one post-hoc to check data correct
source('acc_mod-fxb_bsubrfx.R')

dir_name <- 'cacc_model-int-bsubrfx'
source('acc_mod-int_bsubrfx.R')

dir_name <- 'cacc_model-fxbdrg-brfx'
source('acc_mod-fxbdrug_brfx.R')

dir_name <- 'cacc_model-fxbdrg-bdrgsubrfx'
source('acc_mod-fxbdrug_drugbrfx.R')

dir_name <- 'cacc_model-fxbdrgint-bdrgrfx'
source('acc_mod-fxbdrugint_drgbrfx.R')

dir_name <- 'cacc_model-fxbdrgint-brfx'
source('acc_mod-fxbdrugint_brfx.R')

###------------------------------------------------------
# compare models
###-----------------------------------------------------
ftmplt = '../data/derivatives/%s/%s.Rda'
load(file=sprintf(ftmplt, 'cacc_model-bonly', 'cacc_model-bonly')) # wd
load(file=sprintf(ftmplt, 'cacc_model-fxb-bsubrfx', 'cacc_model-fxb-bsubrfx')) # wd
load(file=sprintf(ftmplt, 'cacc_model-int-bsubrfx', 'cacc_model-int-bsubrfx')) # wd
load(file=sprintf(ftmplt, 'cacc_model-fxbdrg-brfx', 'cacc_model-fxbdrg-brfx')) # wd
load(file=sprintf(ftmplt, 'cacc_model-fxbdrg-bdrgsubrfx', 'cacc_model-fxbdrg-bdrgsubrfx')) # cd
load(file=sprintf(ftmplt, 'cacc_model-fxbdrgint-bdrgrfx', 'cacc_model-fxbdrgint-bdrgrfx')) # wd
load(file=sprintf(ftmplt, 'cacc_model-fxbdrgint-brfx', 'cacc_model-fxbdrgint-brfx')) # wd

loo_compare(fxb_subint, bsubrfx, fxb_bsubrfx, fxbdrg_bsubrfx, fxbdrg_rfxbdrg, 
            fxbdrgint_bdrgsubrfx, fxbdrgint_bsubrfx) #,

#                       elpd_diff se_diff
# fxbdrg_rfxbdrg          0.0       0.0 
# fxbdrgint_bdrgsubrfx   -1.6       0.9 
# fxbdrg_bsubrfx         -6.7      14.3 
# fxbdrgint_bsubrfx      -7.0      14.3 
# bsubrfx               -25.0      16.9 
# fxb_bsubrfx           -25.2      16.8 
# fxb_subint           -321.0      67.0 