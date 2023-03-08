## K. Garner, 2022
## perform modelling of accuracy data as specified in 
## https://osf.io/2y6pk
####------------------------------------------------------

###------------------------------------------------------
# have you run this model before? defined in sourcing file
###-----------------------------------------------------
# new <- TRUE
# verbal <- FALSE
dir.create(sprintf('../data/derivatives/%s', dir_name), showWarnings=FALSE)

if (new){

  mndbd_bm <- brm(formula = v ~ b + drug + m + b:m + (b:drug|sub),
              data = sub_var_dat,
              warmup = 2000, iter = 10000,
              family = skew_normal,
              save_pars = save_pars(all=TRUE)) # for model comparisons 
  mndbd_bm <- add_criterion(mndbd_bm, "loo",  moment_match=TRUE, reloo=TRUE)
  
  # now save!
  save.image(file = sprintf('../data/derivatives/%s/%s.Rda', dir_name, mod_name))
  
} else {
  
  load(file = sprintf('../data/derivatives/%s/%s.Rda', dir_name, mod_name))
}

if (verbal){

  verbal_output(mndbd_bm, dir_name = dir_name)
}

rm(mndbd_bm)
