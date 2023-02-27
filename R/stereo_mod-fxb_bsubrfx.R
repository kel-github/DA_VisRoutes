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


###------------------------------------------------------
# have you run this model before?
###-----------------------------------------------------
# new <- TRUE
# verbal <- TRUE
dir.create(sprintf('../data/derivatives/%s', dir_name), showWarnings=FALSE)
mod_name <- dir_name

if (new){
  
  fxb_bsubrfx <- brm(formula = log(v) ~ b + (b|sub),
                     data = sub_var_dat,
                     warmup = 2000, iter = 10000,
                     family = skew_normal,
                     save_pars = save_pars(all=TRUE)) # for model comparisons 
  
  fxb_bsubrfx <- add_criterion(fxb_bsubrfx, "loo", moment_match=TRUE, reloo=TRUE)
  # now save!
  save(fxb_bsubrfx, file = sprintf('../data/derivatives/%s/%s.Rda', dir_name, mod_name))
  
} else {
  
  load( file = sprintf('../data/derivatives/%s/%s.Rda', dir_name, mod_name))
}

if (verbal){
  # info
#  prior_summary(fxb_bsubrfx)
  verbal_output(fxb_bsubrfx, dir_name = dir_name)
}

rm(fxb_bsubrfx, est, check_dat)