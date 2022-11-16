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
new <- TRUE

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
  
  ###------------------------------------------------------
  # define accuracy models
  ###-----------------------------------------------------
  # start with an effect of block and a subject intercept
  priors <- c(
    prior(normal(0, 10), class = Intercept),
    prior(normal(0, 10), class = b), #,
    prior(cauchy(0, 10), class = sd) #, 
    # prior(lkj(2), class=cor)
  )
  
  b_sess_drug <- brm(formula = tt | trials(td) ~  b + sess + drug + (1|sub),
                  data = acc_dat,
                  prior = priors,
                  warmup = 2000, iter = 10000,
                  family = binomial,
                  save_pars = save_pars(all=TRUE)) # for model comparisons 
  
  # now save!
  save.image(file = '../data/derivatives/acc_mod-bsessdrug/acc_mod-bsessdrug.Rda')
  
} else {
  
  load(file = '../data/derivatives/acc_mod-bsessdrug/acc_mod-bsessdrug.Rda')
}

# info
prior_summary(b_sess_drug)

pdf(file='../data/derivatives/acc_mod-bsessdrug/ps_and_chains.pdf')
plot(b_sess_drug)
dev.off()

summary(b_sess_drug)

# now look at some posterior predictive checks
pdf(file='../data/derivatives/acc_mod-bsessdrug/pp_check.pdf')
pp_check(b_sess_drug)
dev.off()