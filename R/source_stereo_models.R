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
names(sub_var_dat)[names(sub_var_dat) == "block"] <- "b" # to keep in 
sub_var_dat$b <- scale(sub_var_dat$b, scale=FALSE) # mean center regressor
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
