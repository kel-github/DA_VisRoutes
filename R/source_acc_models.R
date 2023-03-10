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
load('../data/derivatives/acc_dat4_model.Rda')
acc_dat$b <- rep(0:7, times=nrow(acc_dat)/8)

dir_name <- 'acc_model-bonly'
source('acc_mod-bonly.R')

dir_name <- 'acc_model-fxb-bsubrfx'
source('acc_mod-fxb_bsubrfx.R')

# make a directory for results if required
dir_name <- 'acc_model-int-bsubrfx'
source('acc_mod-int_bsubrfx.R')

dir_name <- 'acc_model-fxbdrg-brfx'
source('acc_mod-fxbdrug_brfx.R')

dir_name <- 'acc_model-fxbdrg-bdrgsubrfx'
source('acc_mod-fxbdrug_drugbrfx.R')

dir_name <- 'acc_model-fxbdrgint-bdrgrfx'
source('acc_mod-fxbdrugint_drgbrfx.R')

dir_name <- 'acc_model-fxbdrgint-brfx'
source('acc_mod-fxbdrugint_brfx.R')

###------------------------------------------------------
# compare models
###-----------------------------------------------------
ftmplt = '../data/derivatives/%s/%s.Rda'
load(file=sprintf(ftmplt, 'acc_model-bonly', 'acc_model-bonly'))
load(file=sprintf(ftmplt, 'acc_model-fxb-bsubrfx', 'acc_model-fxb-bsubrfx'))
load(file=sprintf(ftmplt, 'acc_model-int-bsubrfx', 'acc_model-int-bsubrfx'))
load(file=sprintf(ftmplt, 'acc_model-fxbdrg-brfx', 'acc_model-fxbdrg-brfx'))
load(file=sprintf(ftmplt, 'acc_model-fxbdrg-bdrgsubrfx', 'acc_model-fxbdrg-bdrgsubrfx'))
load(file=sprintf(ftmplt, 'acc_model-fxbdrgint-bdrgrfx', 'acc_model-fxbdrgint-bdrgrfx'))
load(file=sprintf(ftmplt, 'acc_model-fxbdrgint-brfx', 'acc_model-fxbdrgint-brfx'))

loo_compare(fxb_subint, bsubrfx, fxb_bsubrfx, fxbdrg_bsubrfx, fxbdrg_rfxbdrg, 
            fxbdrgint_bdrgsubrfx, fxbdrgint_bsubrfx) #,

# fxbdrg_rfxbdrg           0.0       0.0
# fxbdrgint_bdrgsubrfx    -0.2       0.5
# fxbdrg_bsubrfx        -674.1      80.8
# fxbdrgint_bsubrfx     -674.8      80.5
# bsubrfx               -677.8      81.3
# fxb_bsubrfx           -679.3      81.7
# fxb_subint           -1224.6     118.0

acc_comp <- loo_compare(fxb_subint, bsubrfx, fxb_bsubrfx, fxbdrg_bsubrfx, fxbdrg_rfxbdrg, 
                        fxbdrgint_bdrgsubrfx, fxbdrgint_bsubrfx)
save(acc_comp, file="../data/derivatives/acc_loo.Rda")
