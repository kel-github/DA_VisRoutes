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

  fxbdrgint_bsubrfx <- brm(formula = log(v) ~ b * drug + (b|sub),
                        data = sub_var_dat,
                        warmup = 2000, iter = 10000,
                        family = skew_normal,
                        save_pars = save_pars(all=TRUE)) # for model comparisons 
  
  fxbdrgint_bsubrfx <- add_criterion(fxbdrgint_bsubrfx, "loo", moment_match=FALSE, reloo=FALSE)
  # now save!
  save(fxbdrgint_bsubrfx, file = sprintf('../data/derivatives/%s/%s.Rda', dir_name, mod_name))
  
} else {
  
  load(file = sprintf('../data/derivatives/%s/%s.Rda', dir_name, mod_name))
}

if (verbal){
 
  verbal_output(fxbdrgint_bsubrfx, dir_name = dir_name)
}

rm(fxbdrgint_bsubrfx, est, check_dat)
