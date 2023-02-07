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

# replication stuff
# 76/162 = acc
# 46/76 = cacc

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

###-------------------------------------------------------
## SUMMARISE DATA INTO ACC COUNTS, CA COUNTS, OR RT
###-------------------------------------------------------
door_acc_sum <- blocked_dat %>% group_by(sub, sess, drug, cond, b) %>% 
                      summarise(cc = length(door[door_type == "cc"]),
                                oc = length(door[door_type == "oc"]),
                                n = length(door[door_type == "n"]),
                                td = length(door),
                                tt = cc+oc,
                                acc = tt/td) %>% # total on a target door
                ungroup()

# NOTE: ISSUE w RT EXTRACTION AS IS. AMEND THIS IF YOU DECIDE TO USE IT!
# remove rts where the target appeared
# blocked_dat <- blocked_dat %>% filter(onset != 999)
# 
# door_rts <- blocked_dat %>% group_by(sub, sess, drug, cond, b, door_type) %>%
#                               summarise(mu_rt = mean(rt),
#                                         med_rt = median(rt),
#                                         var_rt = var(rt),
#                                         sku = (mu_rt - med_rt)/sqrt(var(rt)) )

###-------------------------------------------------------
## NOTE: create regressor for those who scored < .65 at the final block
# probably not going to use this as the criteria is too strict
###-------------------------------------------------------
acc_4_excl <- blocked_dat %>% group_by(sub, sess) %>% filter(b==8) %>%
                              summarise(acc = length(door[door_type == "cc"|
                                                          door_type == "oc"])/
                                                     length(door)) %>%
                              group_by(sub) %>% filter(acc == max(acc))
                                                     
subs_l65 <- unique(acc_4_excl$sub[acc_4_excl$acc < .65])

door_acc_sum$excl <- 0
for (i in subs_l65) door_acc_sum$excl[door_acc_sum$sub == i] <- 1  

# door_rts$excl <- 0
# for (i in subs_l65) door_rts$excl[door_rts$sub == i] <- 1

###-------------------------------------------------------
## NOTE: Save 
###-------------------------------------------------------
save(door_acc_sum, file='../data/derivatives/accuracy.Rda')

###-------------------------------------------------------
## NOW MAKE SUMMARY FOR MODELLING
###-------------------------------------------------------
acc_dat <- door_acc_sum %>% group_by(sub, sess, drug, b) %>%
                              summarise(tt = sum(tt),
                              td = sum(td))
acc_dat <- acc_dat[!is.na(acc_dat$b),]
# scale the block factor
acc_dat$b <- acc_dat$b - 1
acc_dat$sub <- as.factor(acc_dat$sub)
acc_dat$sess <- as.factor(acc_dat$sess)

# appears sub 21 is an outlier on most their measures
boxplot(acc_dat$tt) 
boxplot(acc_dat$tt[acc_dat$sub != 21])
boxplot(acc_dat$td)
boxplot(acc_dat$td[acc_dat$sub != 21]) ###

# removing subject 21 because they are noisy, don't trust the measue
acc_dat <- acc_dat %>% filter(sub != 21)
acc_dat <- acc_dat %>% mutate(acc=tt/td)

# now plot block x drug data
acc_dat %>% ggplot(aes(x=b, y=acc, group=sub, colour=drug)) +
  geom_line() + facet_wrap(~sess*drug)

acc_dat$acc <- NULL
# save summary
save(acc_dat, file='../data/derivatives/acc_dat4_model.Rda')

###-------------------------------------------------------
## NOW MAKE CONTEXT ACC SUMMARY FOR MODELLING
## BUT GIVE IT THE SAME LABELS AS ABOVE FOR EASE OF MODELLING
###-------------------------------------------------------
acc_dat <- door_acc_sum %>% group_by(sub, sess, drug, b) %>%
                summarise(tt = sum(cc),
                          td = sum(cc)+sum(oc))
acc_dat <- acc_dat[!is.na(acc_dat$b),]
# scale the block factor
acc_dat$b <- acc_dat$b - 1
acc_dat$sub <- as.factor(acc_dat$sub)
acc_dat$sess <- as.factor(acc_dat$sess)

boxplot(acc_dat$tt) 
boxplot(acc_dat$tt[acc_dat$sub != 21])
boxplot(acc_dat$td)
boxplot(acc_dat$td[acc_dat$sub != 21]) ###

acc_dat <- acc_dat %>% filter(sub != 21)
acc_dat <- acc_dat %>% mutate(acc=tt/td)

# now plot block x drug data
acc_dat %>% ggplot(aes(x=b, y=acc, group=sub, colour=drug)) +
  geom_line() + facet_wrap(~drug)

acc_dat$acc <- NULL
# save summary
save(acc_dat, file='../data/derivatives/cacc_dat4_model.Rda')

###-------------------------------------------------------
## REMOVE TARGET FIND TRIALS FROM BLOCKED DAT TO MAKE DATA
## FOR SEQUENCE MODELLING
###-------------------------------------------------------
blocked_dat <- blocked_dat %>% filter(onset != 999.000)
blocked_dat <- blocked_dat %>% filter(sub != 21)
save(blocked_dat, file='../data/derivatives/dat4_seq_model.Rda')
