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
# verbal <- TRUE
# make a directory for results if required
dir.create(sprintf('../data/derivatives/%s', dir_name), showWarnings=FALSE)
mod_name <- dir_name

if (new){

  if (faux){
    ###------------------------------------------------------
    # generate data for a ffx of drug, b and an b*sub rfx
    ###-----------------------------------------------------
    nsubs <- 40
    intercept = 0.5
    b <- scale(1:8)
    nb <- 8
    drug_levels <- 2
    drug_reg <- rep(c(0,1), each=nsubs*nb)
    drug_fx <- .2
    b_fx <- .5
    b <- rep(b, times=nsubs*drug_levels) # adding drug manipulation
    betab <- rnorm(nsubs, mean=.5, sd=.2)
    betab <- rep(betab, each=nb, times=drug_levels)
    int_beta <- .4 # drug * beta interaction
    sub_int <- rep(rnorm(nsubs, mean=.2, sd=.4), each=nb, times=drug_levels)
    log_odds <- intercept + b_fx*b + betab*b + drug_fx*drug_reg + drug_reg*b*int_beta + sub_int
    # now turn mu into p
    p <- 1/(1+exp(-log_odds))
    # now sample binomial distribution
    trials <- sample(acc_dat$td, size=length(b), replace=TRUE)
    tt <- mapply(function(x,y) rbinom(1,size=x,prob=y), trials, p)
    faux_dat <- tibble(sub = as.factor(rep(1:nsubs, each=nb, times=drug_levels)),
                       drug = drug_reg,
                       b = b,
                       tt = tt, 
                       td = trials)
    
    faux_fxbdrgint_bsubrfx <- brm(formula = tt | trials(td) ~ b * drug + (b|sub),
                               data = faux_dat,
                               warmup = 2000, iter = 10000,
                               family = binomial,
                               save_pars = save_pars(all=TRUE)) # for model comparisons 
    
    summary(faux_fxbdrgint_bsubrfx) # model recovers parameters okaaaay 
    # but is a tad biased on the fx of b 
  }
  ###------------------------------------------------------
  # define accuracy models
  ###-----------------------------------------------------
  # start with an effect of block and a subject intercept

  fxbdrgint_bdrgsubrfx <- brm(formula = tt | trials(td) ~ b * drug + (b:drug|sub),
                        data = acc_dat,
                        warmup = 2000, iter = 10000,
                        family = binomial,
                        save_pars = save_pars(all=TRUE)) # for model comparisons 
  
  fxbdrgint_bdrgsubrfx <- add_criterion(fxbdrgint_bdrgsubrfx, "loo", moment_match=TRUE, reloo=TRUE)
  # now save!
  save(fxbdrgint_bdrgsubrfx, file = sprintf('../data/derivatives/%s/%s.Rda', dir_name, mod_name))
  
} else {
  
  load(file = sprintf('../data/derivatives/%s/%s.Rda', dir_name, mod_name))
}

if (verbal){
 
  verbal_output(fxbdrgint_bdrgsubrfx, dir_name = dir_name)
# visualising data using:
# https://bookdown.org/ajkurz/DBDA_recoded/dichotomous-predicted-variable.html#interpreting-the-regression-coefficients
# see end of section 22.2.1
  est <- coef(fxbdrgint_bdrgsubrfx)$sub[, "Estimate", ] %>% as.data.frame() %>%
              mutate(sub = unique(acc_dat$sub))
  check_dat <- inner_join(acc_dat, est, by = "sub")

  check_dat <- rbind(check_dat %>% filter(drug == "levodopa") %>% 
                       mutate(log_odds = Intercept + b.y * b.x),
                       check_dat %>% filter(drug == "placebo") %>%
                       mutate(log_odds = Intercept + drugplacebo + b.y*b.x + `b:drugplacebo`*b.x)) %>% 
                       mutate(p= 1/(1+exp(-log_odds))) %>%
                       mutate(obs = tt/td)

  pdf(file=sprintf('../data/derivatives/%s/predobs_resid.pdf', dir_name))
  check_dat %>% ggplot(aes(x=b.x, y=obs, group=drug, colour=drug)) +
    geom_point() + geom_point(aes(x=b.x, y=p, group=drug, colour=drug), shape=2, inherit.aes = FALSE) +
    facet_wrap(~sub)
  
  # plot the residuals to check nowt too crazy is happening
  check_dat %>% mutate(resid=p-obs) %>% 
    ggplot(aes(x=b.x, y=resid, group=drug, colour=drug)) +
    geom_point()
  dev.off()
}

rm(fxbdrgint_bdrgsubrfx, est, check_dat)