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
# have you run this model before? define in sourcing file
###-----------------------------------------------------
# new <- TRUE
# verbal <- TRUE
dir.create(sprintf('../data/derivatives/%s', dir_name), showWarnings=FALSE)
mod_name <- dir_name

if (new){

  bsubrfx <- brm(formula = log(v) ~ (b|sub),
                            data = sub_var_dat,
                            warmup = 2000, iter = 10000,
                            family = gaussian,
                            save_pars = save_pars(all=TRUE)) # for model comparisons 
  
  bsubrfx <- add_criterion(bsubrfx, "loo", moment_match=TRUE, reloo=TRUE)
  # now save!
  save(bsubrfx, file = sprintf('../data/derivatives/%s/%s.Rda', dir_name, mod_name))
  
} else {
  
  load(file = sprintf('../data/derivatives/%s/%s.Rda', dir_name, mod_name))
}

if (verbal){
  # info
  verbal_output(bsubrfx, dir_name = dir_name)
  
}

rm(bsubrfx, check_dat, est)