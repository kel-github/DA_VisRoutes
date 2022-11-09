#### K. Garner, 2022. Free to share, please cite
#### This code comes as is, with no guarantees
####------------------------------------------------------
# use this code to plot the variables of interest for the
# DA_VisRoutes study, for the analysis of accuracy,
# as specified here: https://osf.io/2y6pk
# 
#
# assumes following folder structure:
# -- top/
#      R/code 
#      data/ 
#        derivatives/ # summary data lives here
###-------------------------------------------------------
### loading block (to be moved into document when required)
library(tidyverse)
source('variability_exp_behav_data_wrangling_functions.R')

###-------------------------------------------------------
## LOAD DATA
###-------------------------------------------------------
load('../data/derivatives/accuracy.Rda')

###-------------------------------------------------------
## PLOT, for each subject: sess x drug x block x cond
###-------------------------------------------------------
door_acc_sum %>% ggplot(aes(x=b, y=acc, 
                            colour=drug,
                            shape=as.factor(cond),
                            group=interaction(drug, as.factor(cond)))) +
                   geom_line() + facet_wrap(~as.factor(sub))
ggsave(filename='../images/quick-plots/acc_by_drug.pdf', plot=last_plot(),
                height = 35, width = 20, units = 'cm', dpi = 300)

door_acc_sum %>% ggplot(aes(x=b, y=acc, 
                            colour=as.factor(sess),
                            shape=as.factor(cond),
                            group=interaction(as.factor(sess), as.factor(cond)))) +
                  geom_line() + facet_wrap(~as.factor(sub))
ggsave(filename='../images/quick-plots/acc_by_sess.pdf', plot=last_plot(),
       height = 35, width = 20, units = 'cm', dpi = 300)

# now plot a quick average by drug and session
door_acc_sum %>% group_by(drug, cond, b) %>%
                   summarise(mu = mean(acc)) %>%
                   ggplot(aes(x=b, y=mu,
                              colour=drug,
                              shape=as.factor(cond),
                              group=interaction(drug, as.factor(cond)))) +
                   geom_line()
ggsave(filename='../images/quick-plots/mu_acc_by_drug.pdf', plot=last_plot(),
       height = 15, width = 15, units = 'cm', dpi = 300)


###-------------------------------------------------------
## PLOT, for each subject: sess x drug x block x cond
###-------------------------------------------------------
door_acc_sum %>% group_by(sess, drug, cond, b) %>%
  summarise(mu = mean(acc)) %>%
  ggplot(aes(x=b, y=mu,
             colour=drug,
             shape=as.factor(cond),
             group=interaction(drug, as.factor(cond)))) +
  geom_line() + facet_wrap(~as.factor(sess))
ggsave(filename='../images/quick-plots/mu_acc_by_sess_by_cond_int.pdf', plot=last_plot(),
       height = 15, width = 15, units = 'cm', dpi = 300)

###-------------------------------------------------------
## NOTES on accuracy plots
###-------------------------------------------------------
# the main take homes at this point are:
# - is that there is clearly a difference between sessions, 
#   but whether it is related to drug remains a tad unclear - 
#   e.g. N=14 of 40 show poorer performance on L-dopa
# - there is the slight possibility of a drug x session interaction,
#   but this needs follow up to determine error on potential effect
# - IMPORTANTLY, a non-linear block regressor is required, as
#   accuracy sometimes improves across blocks, but the data also 
#   certainly suggests 'aha' steps where people get it all of a sudden.
#   Also, there are 1 or 2 downhill trends in the data (over blocks)
#   But nothing so systematic that I think a model needs it.
# - context is not providing a lot of info here

###-------------------------------------------------------
## Just investigating further the session x drug interaction
###-------------------------------------------------------
door_acc_sum %>% group_by(sub, drug, sess, b) %>%
                  summarise(acc=mean(acc)) %>%
                  ggplot(aes(x=b, y=acc,
                        group=as.factor(sub),
                        colour=as.factor(sub))) +
                  geom_line() + facet_wrap(~drug*as.factor(sess))
ggsave(filename='../images/quick-plots/acc_by_sub_by_sess_by_drug_int.pdf', plot=last_plot(),
       height = 20, width = 20, units = 'cm', dpi = 300)

# I think this supports the interaction, but it is noisy.
# More people in placebo session 2 get better than those in 
# placebo session 1.
