# written by K. Garner, 2023
# use this code to get the observed and permuted null door transition counts for 
# each participant

rm(list=ls())
library(tidyverse)

# ----------------------------------------------------------------------------
# load data
# ---------------------------------------------------------------------------
load('../data/derivatives/dat4_seq_model.Rda')

### development
# data from one subject and context 

this_trial <- blocked_dat %>% filter(sub == 1 & drug == "placebo" & cond == 1 & b == 1 & t == 2)


do_one_trial <- function(this_trial, ndoors=16){
  # get the permuted null, and return a list for one trial
  # Kwargs:
  # -- tmp [data_frame]: data filtered from blocked_dat, for 1 trial
  # -- ndoors [integer]: number of doors in the display, default 16
  # Returns:
  # list(this_trials_seq, null_hyp)
  
  this_trials_doors <- this_trial$door
  nset <- ndoors
  # get all the possible ways the trial could have been performed
  # NOTE: when this_trials_doors >= 15 
  # Error: cannot allocate vector of size 9742.9 Gb
  all_possible_routes <- do.call(rbind, combinat::permn(this_trials_doors))
  # remove illegal trials if they exist
  dups_idx <- rowSums(t(diff(t(all_possible_routes))) == 0)
  if (sum(dups_idx) > 0){
    all_possible_routes <- all_possible_routes[!dups_idx,]
  }
  
  # count all the transitions in each route
  this_trials_transitions <- table(c(all_possible_routes[,-ncol(all_possible_routes)]), c(all_possible_routes[,-1]))
  
  # add NA to make this_trials_doors uniform length across iterations
  nttd <- length(this_trials_doors)
  if(nttd < nset) this_trials_doors <- c(this_trials_doors, rep(NA, times = nset-nttd))
  list(this_trials_doors, this_trials_transitions) 
} 

this_block <- blocked_dat %>% filter(sub == 1 & sess == 1 & cond == 1 & b == 1)
summarise_seqs_and_nulls_across_block <- function(this_block){
  # run through all the trials for one participant, session, context, and block
  # return the null hypotheses and the actual doors from each trial, summarised
  # at the block level
  # Kwargs:
  # this_block [dataframe] - data from one subject, session, condition and block
  
  # Returns:
  trials <- unique(this_block$t)
  trials <- trials[trials != 3]
  lapply(trials, function(x) do_one_trial(this_block %>% filter(t == x)))
  
  
  
  
}
