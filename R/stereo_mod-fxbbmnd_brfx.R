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

  mndb <- brm(formula = log(v) ~ b + drug + m + (b|sub),
              data = sub_var_dat,
              warmup = 2000, iter = 10000,
              family = skew_normal,
              save_pars = save_pars(all=TRUE)) # for model comparisons 
  mndb <- add_criterion(mndb, "loo",  moment_match=TRUE, reloo=TRUE)
  
  # now save!
  save.image(file = sprintf('../data/derivatives/%s/%s.Rda', dir_name, mod_name))
  
} else {
  
  load(file = sprintf('../data/derivatives/%s/%s.Rda', dir_name, mod_name))
}

if (verbal){

  verbal_output(mndb, dir_name = dir_name)
}

rm(mndb)
