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
# https://psyarxiv.com/xmhfr/
# https://mc-stan.org/misc/warnings.html#divergent-transitions-after-warmup
# https://betanalpha.github.io/assets/case_studies/identifiability.html
# https://www.martinmodrak.cz/2018/02/19/taming-divergences-in-stan-models/
# https://mc-stan.org/users/documentation/case-studies/divergences_and_bias.html
# https://arxiv.org/abs/2011.01808
# 
# main questions:
# how to code interactions? as usual
# does adding a term constitute an interaction in probability space? sometimes it will, and sometimes
# the inverse is true (adding a multiplicative term may not be significant in log odds space but may be present
# in probability space)
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
    #prior(lkj(2), class=cor)
  )
  
  # NOTE: first time I ran was with these priors
  # prior     class      coef group resp dpar nlpar lb ub       source
  # normal(0, 10)         b                                               user
  # normal(0, 10)         b         b                             (vectorized)
  # normal(0, 10) Intercept                                               user
  # lkj_corr_cholesky(1)         L                                            default
  # lkj_corr_cholesky(1)         L             sub                       (vectorized)
  # student_t(3, 0, 2.5)        sd                                  0         default
  # student_t(3, 0, 2.5)        sd             sub                  0    (vectorized)
  # student_t(3, 0, 2.5)        sd         b   sub                  0    (vectorized)
  # student_t(3, 0, 2.5)        sd Intercept   sub                  0    (vectorized)
  # 
  # models
  b_only <- brm(formula = tt | trials(td) ~  b + (1|sub),
                data = acc_dat,
                prior = priors,
                warmup = 2000, iter = 10000,
                family = binomial,
                save_pars = save_pars(all=TRUE)) # for model comparisons 
  
  # now save!
  save.image(file = '../data/derivatives/acc_mod-bonly/acc_mod-bonly.Rda')
  # I think these residuals are broadly ok
} else {
  
  load(file = '../data/derivatives/acc_mod-bonly/acc_mod-bonly.Rda')
}

prior_summary(b_only)
# see https://paul-buerkner.github.io/brms/reference/set_prior.html
# for derails on the class etc
# see https://pubmed.ncbi.nlm.nih.gov/31082309/
# for a guide on how to set priors etc
# also need to check how I can do dic comparisons between these models
pdf(file='../data/derivatives/acc_mod-bonly/ps_and_chains.pdf')
plot(b_only)
dev.off()

summary(b_only)
b_only <- add_criterion(b_only, "waic")

# now look at some posterior predictive checks
pdf(file='../data/derivatives/acc_mod-bonly/pp_check.pdf')
pp_check(b_only)
dev.off()

# now predict each data point and plot the real over the fitted data
est <- coef(b_only)$sub[, "Estimate", ] %>% as.data.frame() %>%
  mutate(sub = unique(acc_dat$sub))
check_dat <- inner_join(acc_dat, est, by = "sub")
check_dat <- check_dat %>% mutate(fit_p = plogis(Intercept + b.y * b.x))
check_dat <- check_dat %>% mutate(obs = tt/td)

pdf(file='../data/derivatives/acc_mod-bonly/predobs_resid.pdf')
check_dat %>% ggplot(aes(x=b.x, y=obs, group=sess, colour=sess)) +
  geom_point() + geom_line(aes(x=b.x, y=fit_p), inherit.aes = FALSE) +
  facet_wrap(~sub)

# plot the residuals to check nowt too crazy is happening
check_dat %>% mutate(resid=fit_p-obs) %>% 
  ggplot(aes(x=b.x, y=resid, group=sess, colour=sess)) +
  geom_point()
dev.off()

