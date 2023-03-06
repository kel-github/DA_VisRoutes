## K. Garner, 2022
## perform modelling of accuracy data as specified in 
## https://osf.io/2y6pk
####------------------------------------------------------
# use this code to apply bayesian multi-level modelling
# of the stereotypy data
#
#
# RESOURCES:
# https://paul-buerkner.github.io/brms/reference/brmsformula.html
###-------------------------------------------------------

# ###------------------------------------------------------
# # have you run this model before? (these should be set in the
# R file that sources this file)
# ###-----------------------------------------------------
# new <- TRUE
# verbal <- TRUE

# make a directory for results if required
dir.create(sprintf('../data/derivatives/%s', dir_name), showWarnings=FALSE)
mod_name <- dir_name
if (new){

    ###------------------------------------------------------
  # now run on true data
  ###-----------------------------------------------------
  fxb_subint_test <- brm(formula = v ~ b + (1|sub),
                  data = sub_var_dat,
                  warmup = 2000, iter = 10000,
                  family = skew_normal,
                  save_pars = save_pars(all=TRUE)) # for model comparisons 
  # will try mean centering the x-axis - works!
  # putting v data on the log scale (make the relationship linear) - works!
  # mean centering the b regressor is working, and log(v) is also
  # great. log(b) and log(v) makes a nice straight line, but lob(b) makes the
  # residuals less random, so ditching that idea
  # not log(v) is baaaad (divergence + tree depth issues)
  # mod <- fxb_subint 
  # est <- coef(mod, robust = TRUE)$sub[, "Estimate", ] %>% as.data.frame() %>%
  #   mutate(sub = unique(sub_var_dat$sub))
  # # create a summary dataframe for wrangling and plotting
  # sum_dat <- inner_join(sub_var_dat, est, by="sub")
  # # put the data back together, given the winning model for acc
  # # to understand the accuracy data I need to:
  # sum_dat <- rbind(sum_dat %>% mutate(v_est = Intercept + b.x*b.y)) %>% 
  #   mutate(resid = log(v) - v_est)
  
  # # quick residuals check
  # sum_dat %>% ggplot(aes(x=v_est, y=resid, colour=sub)) + geom_point() 
  fxb_subint <- add_criterion(fxb_subint, "loo", moment_match=TRUE, reloo=TRUE)
  # now save!
  save(fxb_subint, file = sprintf('../data/derivatives/%s/%s.Rda', dir_name, mod_name))
  # I think these residuals are broadly ok
} else {
  
  load(file = sprintf('../data/derivatives/%s/%s.Rda', dir_name, mod_name))
}

if (verbal){

  verbal_output(mod=fxb_subint, dir_name=dir_name)
}

rm(fxb_subint, est, check_dat)