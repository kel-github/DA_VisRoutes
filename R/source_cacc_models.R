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

dir_name <- 'cacc_model-bonly'
source('acc_mod-bonly.R')

dir_name <- 'cacc_model-fxb-bsubrfx'
source('acc_mod-fxb_bsubrfx.R')

# make a directory for results if required
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
load(file=sprintf(ftmplt, 'cacc_model-bonly', 'cacc_model-bonly'))
load(file=sprintf(ftmplt, 'cacc_model-fxb-bsubrfx', 'cacc_model-fxb-bsubrfx'))
load(file=sprintf(ftmplt, 'cacc_model-int-bsubrfx', 'cacc_model-int-bsubrfx'))
load(file=sprintf(ftmplt, 'cacc_model-fxbdrg-brfx', 'cacc_model-fxbdrg-brfx'))
load(file=sprintf(ftmplt, 'cacc_model-fxbdrg-bdrgsubrfx', 'cacc_model-fxbdrg-bdrgsubrfx'))
load(file=sprintf(ftmplt, 'cacc_model-fxbdrgint-bdrgrfx', 'cacc_model-fxbdrgint-bdrgrfx'))
load(file=sprintf(ftmplt, 'cacc_model-fxbdrgint-brfx', 'cacc_model-fxbdrgint-brfx'))

loo_compare(fxb_subint, bsubrfx, fxb_bsubrfx, fxbdrg_bsubrfx, fxbdrg_rfxbdrg, 
            fxbdrgint_bdrgsubrfx, fxbdrgint_bsubrfx) #,
