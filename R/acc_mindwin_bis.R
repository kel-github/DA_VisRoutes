## K. Garner, 2022
## perform modelling of accuracy data as specified in 
## https://osf.io/2y6pk
####------------------------------------------------------
# use this code to apply the bayesian multi-level modelling
# of the accuracy data
# 
# assumes following folder structure:
# -- top/
#      R/code is here
#      data/ 
#        derivatives/ # summary data is here
#
#
# RESOURCES:
# https://www.jstatsoft.org/article/view/v080i01
# https://github.com/paul-buerkner/brms
# https://bayesball.github.io/BRMS/multilevel-regression.html
###-------------------------------------------------------


# ###------------------------------------------------------
# # load packages
# ###-----------------------------------------------------
# library(brms)
# library(tidyverse)

###------------------------------------------------------
# have you run this model before?
###-----------------------------------------------------
# new <- TRUE
# verbal <- FALSE
dir.create(sprintf('../data/derivatives/%s', dir_name), showWarnings=FALSE)

if (new){

  ###------------------------------------------------------
  # define accuracy models
  ###-----------------------------------------------------
  
  if (data_names[i] == "acc"){
    
    acc_mindwin_bis <- brm(formula = tt | trials(td) ~ b + drug + m + b:m + drug:m + bis + (b:drug|sub),
                            data = acc_dat,
                            warmup = 2000, iter = 10000,
                            family = binomial,
                            save_pars = save_pars(all=TRUE)) # for model comparisons 

  } else if (data_names[i] == "cacc"){
    
    acc_mindwin_bis <- brm(formula = tt | trials(td) ~ b + drug + m + bis + (b:drug|sub),
                           data = acc_dat,
                           warmup = 2000, iter = 10000,
                           family = binomial,
                           save_pars = save_pars(all=TRUE)) # for model comparisons 
    
    
  }
  acc_mindwin_bis <- add_criterion(acc_mindwin_bis, "loo",  moment_match=TRUE, reloo=TRUE)
  # now save!
  save.image(file = sprintf('../data/derivatives/%s/%s.Rda', dir_name, mod_name))
  
} else {
  
  load(file = sprintf('../data/derivatives/%s/%s.Rda', dir_name, mod_name))
}

if (verbal){
  # info
 # prior_summary(mnd)

  verbal_output(acc_mindwin_bis, dir_name = dir_name)

}

rm(acc_mindwin_bis)
