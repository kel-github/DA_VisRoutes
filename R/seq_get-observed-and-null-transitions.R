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

tmp <- blocked_dat %>% filter(sub == 1 & drug == "placebo" & cond == 1 & b == 1 & t == 2)


do_one_trial <- function(...){

  this_trials_doors <- tmp$door
  nset <- ndoors
  # get all the possible ways the trial could have been performed
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
  #### question on how I want to collect the data here - should I make it a 16 x 16 matrix
  #### with everything in the right place
  #### same above, for this_trials_doors, but for the vector case
  list(this_trials_doors, this_trials_transitions) 
} 
