## K. Garner, 2022
## perform modelling of accuracy data as specified in 
## https://osf.io/2y6pk
####------------------------------------------------------
# use this code to apply the bayesian multi-level modelling
# of the accuracy data
#
# Notes on modelling:
# here is a useful reference to GLMMs: 
# https://stats.oarc.ucla.edu/other/mult-pkg/introduction-to-generalized-linear-mixed-models/
# take aways:
# rfx's are modelled as a deviation from the fxd effect, therefore
# we are only estimating the variance parameter (and not a mean parameter)
# all rfxs will go into the Z matrix, but the main intercept will be the average of the rfx
# intercepts
# 
# for data-wrangling/pulling data out of the brms object
# https://bookdown.org/ajkurz/DBDA_recoded/dichotomous-predicted-variable.html
# 
# this page is useful for definitions:
# https://stats.oarc.ucla.edu/stata/seminars/deciphering-interactions-in-logistic-regression/
#
# convergence problems:
# https://m-clark.github.io/posts/2020-03-16-convergence/
# https://psyarxiv.com/xmhfr/ - this paper suggests that backing off to random intercepts only
# is a suboptimal strategy
# https://mc-stan.org/misc/warnings.html#divergent-transitions-after-warmup
# https://betanalpha.github.io/assets/case_studies/identifiability.html
# https://www.martinmodrak.cz/2018/02/19/taming-divergences-in-stan-models/
# main overview of what is happening when there are divergences
# https://mc-stan.org/users/documentation/case-studies/divergences_and_bias.html
# https://arxiv.org/abs/2011.01808
# 
# Q. does adding a term constitute an interaction in probability space? sometimes it will, and sometimes
# the inverse is true (adding a multiplicative term may not be significant in log odds space but may be present
# in probability space)
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

# ###------------------------------------------------------
# # have you run this model before?
# ###-----------------------------------------------------
# new <- TRUE
# verbal <- TRUE

# make a directory for results if required
dir.create(sprintf('../data/derivatives/%s', dir_name), showWarnings=FALSE)
mod_name <- dir_name
if (new){

  if (faux){
    ###-----------------------------------------------------
    # first, generate some data to check the model covers
    # the correct parameters
    ###-----------------------------------------------------
    nsubs <- 40
    intercept <- 2
    b <- scale(1:8)
    nb <- 8
    b <- rep(b, times=nsubs)
    betab <- .5
    sub_int <- rep(rnorm(nsubs, mean=.2, sd=.4), each=nb)
    log_odds <- intercept + betab*b + sub_int
    # now turn mu into p
    p <- 1/(1+exp(-log_odds))
    # now sample binomial distribution
    trials <- sample(acc_dat$td, size=length(b))
    tt <- mapply(function(x,y) rbinom(1,size=x,prob=y), trials, p)
    faux_dat <- tibble(sub = as.factor(rep(1:nsubs, each=nb)),
                       b = b,
                       tt = tt, 
                       td = trials)
    ###------------------------------------------------------
    # define test accuracy model on faux data
    ###-----------------------------------------------------
    # start with an effect of block and a subject intercept
    # NOTE: using default priors from: https://www.jstatsoft.org/article/view/v080i01
    # 
    # models
    faux_b <- brm(formula = tt | trials(td) ~  b + (1|sub),
                  data = faux_dat,
                  warmup = 2000, iter = 10000,
                  family = binomial,
                  save_pars = save_pars(all=TRUE)) # for model comparisons 
    
    plot(faux_b)
    # the model roughly retrieves the parameters, so we're happy
  }
  ###------------------------------------------------------
  # now run on true data
  ###-----------------------------------------------------
  fxb_subint <- brm(formula = tt | trials(td) ~  b + (1|sub),
                  data = acc_dat,
                  warmup = 2000, iter = 10000,
                  family = binomial,
                  save_pars = save_pars(all=TRUE)) # for model comparisons 
  fxb_subint <- add_criterion(fxb_subint, "loo", moment_match=TRUE, reloo=TRUE)
  # now save!
  save(fxb_subint, file = sprintf('../data/derivatives/%s/%s.Rda', dir_name, mod_name))
  # I think these residuals are broadly ok
} else {
  
  load(file = sprintf('../data/derivatives/%s/%s.Rda', dir_name, mod_name))
}

if (verbal){

  verbal_output(mod=fxb_subint, dir_name=dir_name)
  
  # now predict each data point and plot the real over the fitted data
  est <- coef(fxb_subint)$sub[, "Estimate", ] %>% as.data.frame() %>%
    mutate(sub = unique(acc_dat$sub))
  check_dat <- inner_join(acc_dat, est, by = "sub")
  check_dat <- check_dat %>% mutate(log_odds = Intercept + b.y*b.x) %>% 
                        mutate(p= 1/(1+exp(-log_odds))) %>%
                        mutate(obs = tt/td)
  
  pdf(file=sprintf('../data/derivatives/%s/predobs_resid.pdf', dir_name))
    check_dat %>% ggplot(aes(x=b.x, y=obs, group=sess, colour=sess)) +
      geom_point() + geom_point(aes(x=b.x, y=p), shape=2, inherit.aes = FALSE) +
      facet_wrap(~sub)
  
  # plot the residuals to check nowt too crazy is happening
    check_dat %>% mutate(resid=p-obs) %>% 
      ggplot(aes(x=b.x, y=resid, group=sess, colour=sess)) +
      geom_point()
  dev.off()
}

rm(fxb_subint, est, check_dat)