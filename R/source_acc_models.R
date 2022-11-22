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

load('../data/derivatives/acc_dat4_model.Rda')
source('acc_mod-bonly.R')
source('acc_mod-fxb_bsubrfx.R')
source('acc_mod-int_bsubrfx.R')