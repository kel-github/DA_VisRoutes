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
  acc_dat$drug <- as.factor(acc_dat$drug)
  
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
  
  drug_bsubrfx <- brm(formula = tt | trials(td) ~  drug + (b|sub),
                            data = acc_dat,
                            prior = priors,
                            warmup = 2000, iter = 10000,
                            family = binomial,
                            save_pars = save_pars(all=TRUE)) # for model comparisons 
  
  # now save!
  save.image(file = '../data/derivatives/acc_mod-drug-bsubrfx/acc_mod-drug-bsubrfx.Rda')
  
} else {
  
  load(file = '../data/derivatives/acc_mod-drug-bsubrfx/acc_mod-drug_bsubrfx.Rda')
}

# info
prior_summary(drug_int_bsubrfx)

pdf(file='../data/derivatives/acc_mod-drug-bsubrfx/ps_and_chains.pdf')
plot(drug_int_bsubrfx)
dev.off()

summary(drug_subrfx)

# now look at some posterior predictive checks
pdf(file='../data/derivatives/acc_mod-drug-bsubrfx/pp_check.pdf')
pp_check(drug_bsubrfx)
dev.off()

# visualising data using:
# https://bookdown.org/ajkurz/DBDA_recoded/dichotomous-predicted-variable.html#interpreting-the-regression-coefficients
# see end of section 22.2.1
est <- coef(drug_bsubrfx)$sub[, "Estimate", ] %>% as.data.frame() %>%
            mutate(sub = unique(acc_dat$sub))
check_dat <- inner_join(acc_dat, est, by = "sub")
check_dat <- rbind(check_dat %>% filter(drug == "levodopa") %>% 
                     mutate(log_odds = Intercept + b.y * b.x),
                   check_dat %>% filter(drug == "placebo") %>%
                     mutate(log_odds = Intercept + drugplacebo + b.y*b.x)) %>% 
                     mutate(p= 1/(1+exp(-log_odds))) %>%
                     mutate(obs = tt/td)

pdf(file='../data/derivatives/acc_mod-drug-bsubrfx/predobs_resid.pdf')
check_dat %>% ggplot(aes(x=b.x, y=obs, group=drug, colour=drug)) +
  geom_point() + geom_point(aes(x=b.x, y=p, group=drug, colour=drug), shape=2, inherit.aes = FALSE) +
  facet_wrap(~sub)

# plot the residuals to check nowt too crazy is happening
check_dat %>% mutate(resid=p-obs) %>% 
  ggplot(aes(x=b.x, y=resid, group=drug, colour=drug)) +
  geom_point()
dev.off()