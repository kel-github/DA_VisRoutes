#### K. Garner, 2022. Free to share, please cite
#### This code comes as is, with no guarantees
####------------------------------------------------------
# use this code to load and prepare data from the
# DA_VisRoutes study for the analysis of accuracy
# as specified here: https://osf.io/2y6pk
# 
#
# assumes following folder structure:
# -- top/
#      R/code is here
#      data/ 
###-------------------------------------------------------

rm(list=ls())
set.seed(42)
### loading block (to be moved into document when required)
library(tidyverse)
source('variability_exp_behav_data_wrangling_functions.R')

not_new <- T # if T then load existing RData file with data
###-------------------------------------------------------
## LOAD DATA
###-------------------------------------------------------
# NOTE: you only want to do this once and then save the dataframe
# as it is very slooooow
if (!not_new){
  nses <- 2 # number of sessions
  nsub <- 40
  sub_nums <- rep(c(1:40), each = nses)
  ses_nums <- rep(c(1,2), times = length(sub_nums)/2)
  dat <- mapply(get_data, sub_num = sub_nums, ses = ses_nums, MoreArgs = list(fpth='../data/'), SIMPLIFY = FALSE )
  dat <- do.call(rbind, dat)
  saveRDS(dat, file='../data/derivatives/raw.Rds')
  
} else {
  # load the RData file instead
  dat <- readRDS('../data/derivatives/raw.Rds')
}


###-------------------------------------------------------
## ADD DRUG INFO
###-------------------------------------------------------

#if (!not_new){
drug_info <- read_csv('../data/derivatives/drug-assignment.csv', col_names=T) 
drug_info <- drug_info %>%
  pivot_longer(cols=starts_with('S_'), names_to = 'sess', 
               names_prefix='S_', values_to = 'drug')
drug_info$sess <- as.numeric(drug_info$sess)

dat <- inner_join(dat, drug_info, by=c('sub', 'sess'))
#}

###-------------------------------------------------------
## ADD BLOCK REGRESSOR
###-------------------------------------------------------
# data wrangling to set up the wrap over subs, sessions and conditions
sub_nums <- unique(dat$sub)
sess_labels <- unique(dat$drug)
sess_nums <- unique(dat$sess)
cond_labels <- unique(dat$cond)

# get total numbers
t_subs <- length(sub_nums)
t_sess <- length(sess_labels)
t_conds <- length(cond_labels)

# make assignation vectors for mapply
sub_nums <- rep(sub_nums, each=t_sess*t_conds)
sess_labels <- rep(sess_labels, each=t_sess, times=t_subs)
sess_nums <- rep(sess_nums, each=t_sess, times=t_subs)
cond_labels <- rep(cond_labels, times=t_subs*t_sess)

blocked_dat <- do.call(rbind, mapply(apply_insert_block_Ns, 
                                     subN=sub_nums, 
                                     sessN=sess_labels, 
                                     condN=cond_labels, 
                                     MoreArgs=list(data=dat), 
                                     SIMPLIFY=FALSE))

#### if doing stereotypy data, now skip ahead to 
#### ## REMOVE TARGET FIND TRIALS FROM BLOCKED DAT TO MAKE DATA
## FOR SEQUENCE MODELLING
###-------------------------------------------------------
## ADD DOOR TYPE REGRESSOR
###-------------------------------------------------------
sub_nums <- rep(unique(blocked_dat$sub), each = max(dat$sess))
ses_nums <- rep(unique(dat$sess), times=t_subs)

blocked_dat <- do.call(rbind,
                       mapply(function(i,j) assign_door_types(blocked_dat %>%
                                                                filter(sub == i & sess == j)),
                              sub_nums, ses_nums,
                              SIMPLIFY=FALSE))


### ------------------------------------------------------
## CONTROL ANALYSIS
### ------------------------------------------------------
# here I am looking to check the distribution underlying the selection
# of errors - i.e. other context vs never relevant, averaged over subject for 
# the drug and the placebo sessions
err_dat <- blocked_dat %>% group_by(sub) %>%
  summarise(oc_N = length(door[door_type == "oc"]),
            ter_N =length(door[door_type == "oc"| door_type == "n"]),
            err = oc_N/ter_N) 
err_dat <- err_dat %>% filter(sub != "21")
hist(err_dat$err)

# first I am just going to do a bayesian one sample t-test, comparing to a null
# value of .33
library(BayesFactor)
bf <- ttestBF(x=err_dat$err, nullInterval = c(.33, 1))
bf
tt <- t.test(x=err_dat$err, alternative="greater", mu=.33)

desc_stats_err <- err_dat %>% summarise(mu = mean(err),
                                        med = median(err),
                                        sd = sd(err),
                                        N = length(err),
                                        ci = 1.96*(sd/sqrt(N))) %>%
                              mutate(upper = mu+ci,
                                     lower = mu-ci)
save(desc_stats_err, bf, tt, file = sprintf('../data/derivatives/sanity_check_contextual.Rda'))