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
  # start with an effect of block and a subject intercept

  mndb <- brm(formula = tt | trials(td) ~ b + drug + m + b:m + (b:drug|sub),
                   data = acc_dat,
                   warmup = 2000, iter = 10000,
                   family = binomial,
                   save_pars = save_pars(all=TRUE)) # for model comparisons 
  mndb <- add_criterion(mndb, "loo",  moment_match=TRUE, reloo=TRUE)
  
  # now save!
  save.image(file = sprintf('../data/derivatives/%s/%s.Rda', dir_name, mod_name))
  
} else {
  
  load(file = sprintf('../data/derivatives/%s/%s.Rda', dir_name, mod_name))
}

if (verbal){
  # info
 # prior_summary(mnd)

  verbal_output(mndb, dir_name = dir_name)
# visualising data using:
# https://bookdown.org/ajkurz/DBDA_recoded/dichotomous-predicted-variable.html#interpreting-the-regression-coefficients
# see end of section 22.2.1
#   est <- coef(fxdrgmnd_bsubrfx)$sub[, "Estimate", ] %>% as.data.frame() %>%
#               mutate(sub = unique(acc_dat$sub))
#   check_dat <- inner_join(acc_dat, est, by = "sub")
# 
#   check_dat <- rbind(check_dat %>% filter(drug == "levodopa") %>% 
#                        mutate(log_odds = Intercept + b.y * b.x + m.x*m.y),
#                        check_dat %>% filter(drug == "placebo") %>%
#                        mutate(log_odds = Intercept + drugplacebo + b.y*b.x + m.x*m.y)) %>% 
#                        mutate(p= 1/(1+exp(-log_odds))) %>%
#                        mutate(obs = tt/td)
# 
#   pdf(file='../data/derivatives/acc_mod-fxdrgmnd_bsubrfx/predobs_resid.pdf')
#     check_dat %>% ggplot(aes(x=b.x, y=obs, group=drug, colour=drug)) +
#       geom_point() + geom_point(aes(x=b.x, y=p, group=drug, colour=drug), shape=2, inherit.aes = FALSE) +
#       facet_wrap(~sub)
# 
# # plot the residuals to check nowt too crazy is happening
#   check_dat %>% mutate(resid=p-obs) %>% 
#     ggplot(aes(x=b.x, y=resid, group=drug, colour=drug)) +
#     geom_point()
#   dev.off()
  
  # now compare this model to the next best
#@  load('../data/derivatives/acc_model-fxdrg-bsubrfx/acc_model-fxdrg-bsubrfx.Rda')
}

rm(mndb)
