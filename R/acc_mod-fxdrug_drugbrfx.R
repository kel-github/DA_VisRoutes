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
# load packages
###-----------------------------------------------------
library(brms)
library(tidyverse)

###------------------------------------------------------
# have you run this model before?
###-----------------------------------------------------
#new <- TRUE
#verbal <- TRUE

if (new){
  ###------------------------------------------------------
  # load data
  ###-----------------------------------------------------
  load('../data/derivatives/accuracy.Rda')
  
  ###------------------------------------------------------
  # sum over context, as initial peruse of data showed
  # no information in this variable, for accuracy
  ###-----------------------------------------------------
  acc_dat <- door_acc_sum %>% group_by(sub, sess, drug, b) %>%
    summarise(tt = sum(tt),
              td = sum(td))
  acc_dat <- acc_dat[!is.na(acc_dat$b),]
  # scale the block factor
  acc_dat$b <- scale(acc_dat$b)
  acc_dat$sub <- as.factor(acc_dat$sub)
  acc_dat$sess <- as.factor(acc_dat$sess)
  acc_dat$drug <- as.factor(acc_dat$drug)
  
  ###------------------------------------------------------
  # generate data for a ffx of drug, and an b*sub rfx
  ###-----------------------------------------------------
  nsubs <- 40
  intercept = 0.5
  b <- scale(1:8)
  nb <- 8
  drug_levels <- 2
  drug_reg <- rep(c(0,1), each=nsubs*nb)
  drug_fx <- .2
  drug_rfx <- rnorm(nsubs, .3, sd=.1)
  b <- rep(b, times=nsubs*drug_levels) # adding drug manipulation
  betab <- rnorm(nsubs, mean=.5, sd=.2)
  betab <- rep(betab, each=nb, times=drug_levels)
  sub_int <- rep(rnorm(nsubs, mean=.2, sd=.4), each=nb)
  sub_int <- c(sub_int, sub_int)
  log_odds <- intercept + betab*b*(drug_rfx*drug_reg) + drug_fx*drug_reg + sub_int
  # now turn mu into p
  p <- 1/(1+exp(-log_odds))
  # now sample binomial distribution
  trials <- sample(acc_dat$td, size=length(b))
  tt <- mapply(function(x,y) rbinom(1,size=x,prob=y), trials, p)
  faux_dat <- tibble(sub = as.factor(rep(1:nsubs, each=nb, times=2)),
                     drug = drug_reg,
                     b = b,
                     tt = tt, 
                     td = trials)
  
  faux_fxdrg_bdrugsubrfx <- brm(formula = tt | trials(td) ~ drug + (b:drug|sub),
                                data = faux_dat,
                                warmup = 2000, iter = 10000,
                                family = binomial,
                                save_pars = save_pars(all=TRUE)) # for model comparisons 
  
  summary(faux_fxdrg_bdrugsubrfx) # model recovers parameters well enough to continue
  
  ###------------------------------------------------------
  # define accuracy models
  ###-----------------------------------------------------
  # start with an effect of block and a subject intercept

  fxdrg_bdrugsubrfx <- brm(formula = tt | trials(td) ~  drug + (b:drug|sub),
                              data = acc_dat,
                              warmup = 2000, iter = 10000,
                              family = binomial,
                              save_pars = save_pars(all=TRUE)) # for model comparisons 
  
  # now save!
  save.image(file = '../data/derivatives/acc_mod-fxdrg-bdrugsubrfx/acc_mod-fxdrg-bdrugsubrfx.Rda')
  
} else {
  
  load(file = '../data/derivatives/acc_mod-fxdrg-bdrugsubrfx/acc_mod-fxdrg-bdrugsubrfx.Rda')
}

if (verbal){
  # info
  prior_summary(fxdrg_bdrugsubrfx)

  pdf(file='../data/derivatives/acc_mod-fxdrg-bdrugsubrfx/ps_and_chains.pdf')
    plot(fxdrg_bdrugsubrfx)
  dev.off()

  summary(fxdrg_bsubrfx)

  # now look at some posterior predictive checks
  pdf(file='../data/derivatives/acc_mod-fxdrg-bdrugsubrfx/pp_check.pdf')
    pp_check(fxdrg_bdrugsubrfx)
  dev.off()

# visualising data using:
# https://bookdown.org/ajkurz/DBDA_recoded/dichotomous-predicted-variable.html#interpreting-the-regression-coefficients
# see end of section 22.2.1
  est <- coef(fxdrg_bdrugsubrfx)$sub[, "Estimate", ] %>% as.data.frame() %>%
              mutate(sub = unique(acc_dat$sub))
  check_dat <- inner_join(acc_dat, est, by = "sub")

  check_dat <- rbind(check_dat %>% filter(drug == "levodopa") %>% 
                       mutate(log_odds = Intercept + `b:druglevodopa` * b),
                       check_dat %>% filter(drug == "placebo") %>%
                       mutate(log_odds = Intercept + drugplacebo + `b:drugplacebo`*b)) %>% 
                       mutate(p= 1/(1+exp(-log_odds))) %>%
                       mutate(obs = tt/td)

  pdf(file='../data/derivatives/acc_mod-fxdrg-bdrugsubrfx/predobs_resid.pdf')
    check_dat %>% ggplot(aes(x=b, y=obs, group=drug, colour=drug)) +
      geom_point() + geom_point(aes(x=b, y=p, group=drug, colour=drug), shape=2, inherit.aes = FALSE) +
      facet_wrap(~sub)

# plot the residuals to check nowt too crazy is happening
  check_dat %>% mutate(resid=p-obs) %>% 
    ggplot(aes(x=b, y=resid, group=drug, colour=drug)) +
    geom_point()
  dev.off()
}