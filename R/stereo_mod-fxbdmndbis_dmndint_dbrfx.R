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

  mndbdbis_bm <- brm(formula = v ~ b + drug + m + drug:m + bis + (b:drug|sub),
                    data = sub_var_dat,
                    warmup = 2000, iter = 10000,
                    family = skew_normal,
                    save_pars = save_pars(all=TRUE)) # for model comparisons 
  mndbdbis_bm <- add_criterion(mndbdbis_bm, "loo",  moment_match=TRUE, reloo=TRUE)
  
  # now save!
  save.image(file = sprintf('../data/derivatives/%s/%s.Rda', dir_name, mod_name))
  
} else {
  
  load(file = sprintf('../data/derivatives/%s/%s.Rda', dir_name, mod_name))
}

if (verbal){

  verbal_output(mndbdbis_bm, dir_name = dir_name)
}

rm(mndbdbis_bm)
