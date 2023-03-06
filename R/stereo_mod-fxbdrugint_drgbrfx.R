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
###-------------------------------------------------------

###------------------------------------------------------
# have you run this model before? define in sourcing
###-----------------------------------------------------
# new <- TRUE
# verbal <- TRUE
# make a directory for results if required
dir.create(sprintf('../data/derivatives/%s', dir_name), showWarnings=FALSE)
mod_name <- dir_name

if (new){

  fxbdrgint_bdrgsubrfx <- brm(formula = v ~ b * drug + (b:drug|sub),
                        data = sub_var_dat,
                        warmup = 2000, iter = 10000,
                        family = skew_normal,
                        save_pars = save_pars(all=TRUE)) # for model comparisons 
  
  fxbdrgint_bdrgsubrfx <- add_criterion(fxbdrgint_bdrgsubrfx, "loo", moment_match=TRUE, reloo=TRUE)
  # now save!
  save(fxbdrgint_bdrgsubrfx, file = sprintf('../data/derivatives/%s/%s.Rda', dir_name, mod_name))
  
} else {
  
  load(file = sprintf('../data/derivatives/%s/%s.Rda', dir_name, mod_name))
}

if (verbal){
 
  verbal_output(fxbdrgint_bdrgsubrfx, dir_name = dir_name)
}

rm(fxbdrgint_bdrgsubrfx, est, check_dat)