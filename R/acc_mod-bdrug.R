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
  
  b_drug <- brm(formula = tt | trials(td) ~  b + drug + (1|sub),
                  data = acc_dat,
                  prior = priors,
                  warmup = 2000, iter = 10000,
                  family = binomial,
                  save_pars = save_pars(all=TRUE)) # for model comparisons 
  
  # now save!
  save.image(file = '../data/derivatives/acc_mod-bdrug/acc_mod-bdrug.Rda')
  
} else {
  
  load(file = '../data/derivatives/acc_mod-bdrug/acc_mod-bdrug.Rda')
}

# info
prior_summary(b_drug)

pdf(file='../data/derivatives/acc_mod-bdrug/ps_and_chains.pdf')
plot(b_drug)
dev.off()

summary(b_drug)

# now look at some posterior predictive checks
pdf(file='../data/derivatives/acc_mod-bdrug/pp_check.pdf')
pp_check(b_drug)
dev.off()

# now predict each data point and plot the real over the fitted data
est <- coef(b_drug)$sub[, "Estimate", ] %>% as.data.frame() %>%
  mutate(sub = unique(acc_dat$sub))
check_dat <- inner_join(acc_dat, est, by = "sub")
check_dat <- rbind(check_dat %>% filter(drug == "levodopa") %>% 
                                  mutate(fit_p = plogis(Intercept + b.y * b.x)),
                   check_dat %>% filter(drug == "placebo") %>%
                                  mutate(fit_p = plogis(Intercept + drugplacebo + b.y*b.x)))
check_dat <- check_dat %>% mutate(obs = tt/td)

pdf(file='../data/derivatives/acc_mod-bdrug/predobs_resid.pdf')
check_dat %>% ggplot(aes(x=b.x, y=obs, group=drug, colour=drug)) +
  geom_point() + geom_line(aes(x=b.x, y=fit_p, group=drug, colour=drug), inherit.aes = FALSE) +
  facet_wrap(~sub)

# plot the residuals to check nowt too crazy is happening
check_dat %>% mutate(resid=fit_p-obs) %>% 
  ggplot(aes(x=b.x, y=resid, group=drug, colour=drug)) +
  geom_point()
dev.off()
