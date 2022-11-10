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
###-------------------------------------------------------

###------------------------------------------------------
# load packages
###-----------------------------------------------------
library(brms)
library(tidyverse)

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

# scale the block factor
acc_dat$b <- scale(acc_dat$b)

###------------------------------------------------------
# define accuracy model
###-----------------------------------------------------
acc_fit <- brm(formula= tt | trials(td) ~  sess*drug*b + (1 + sub|sess*drug*b),
               data = acc_dat,
               family = binomial)