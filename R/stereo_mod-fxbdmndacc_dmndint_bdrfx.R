## K. Garner, 2022
## perform modelling of stereotypy
## adding overall accuracy to the model
## https://osf.io/2y6pk
####------------------------------------------------------
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

msv_fnms <- c('stereo_winplusmindplusacc_dmindint')


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

load('../data/derivatives/acc_dat4_model.Rda')
acc_dat <- acc_dat %>% mutate(acc = tt/td)
acc_dat$b <- log(acc_dat$b+1)
sub_var_dat <- inner_join(sub_var_dat, acc_dat, by=c("sub", "drug", "b"))
###------------------------------------------------------
# have you run this model before? defined in sourcing file
###-----------------------------------------------------
# new <- TRUE
# verbal <- FALSE
dir_name <- msv_fnms
mod_name <- dir_name
dir.create(sprintf('../data/derivatives/%s', dir_name), showWarnings=FALSE)

if (new){

  mndbdacc_dm <- brm(formula = v ~ b + drug + m + drug:m + acc + (b:drug|sub),
              data = sub_var_dat,
              warmup = 2000, iter = 10000,
              family = skew_normal,
              save_pars = save_pars(all=TRUE)) # for model comparisons 
  mndbdacc_dm <- add_criterion(mndbdacc_dm, "loo",  moment_match=TRUE, reloo=TRUE)
  
  # now save!
  save.image(file = sprintf('../data/derivatives/%s/%s.Rda', dir_name, mod_name))
  
} else {
  
  load(file = sprintf('../data/derivatives/%s/%s.Rda', dir_name, mod_name))
}

if (verbal){

  verbal_output(mndbdacc_dm, dir_name = dir_name)
}

rm(mndbdacc_dm)
