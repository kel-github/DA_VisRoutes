rm(list=ls())
library(tidyverse)

set.seed(42) # meaning of life

########## now run an actual null simulation
set = seq(1, 8, by = 1) # broad set to draw from
nsims = 1000 # number of simulations
ntrials = 20 # number of trials per simulation

# defining some ps for an ordered agent:
# paths: 1, 3, 2, 5, 8, 7, 6, 5, 4 # 90 %
# path2: 2, 1, 3, 4, 5, 6, 7, 8, 5 # 5%
# path3: 3, 5, 6, 7, 8, 5, 1, 2, 4 # 
ps <- matrix(0, nrow=8, ncol = 8)
ps[1, c(2:3)] <- c(.05, .95)
ps[2, c(5, 1, 4)] <- c(.9, .05, .05)
ps[3, c(2, 4, 5)] <- c(.9, .05, .05)
ps[4, c(5)] <- 1
ps[5, c(8, 4, 6, 1)] <- c(.45, .45, .05, .05)
ps[6, c(5, 7)] <- c(.9, .1)
ps[7, c(6, 8)] <- c(.9, .1)
ps[8, c(7, 5)] <- c(.9, .1)
starts <- list(c(1, 2, 3), c(.9, .05, .05))

############# FUNCTIONS #######################

run_one_block_of_trials <- function(set, ntrials, random_agent, ps=NA, starts=NA){
  # set [vec]: the set of possible doors to be selected for that trial
  # ntrials [int]: how many trials to run on this simulation?
  # random_agent [logical] T or F
  # ps [matrix]: if random_agent == F, then provide the transition probs to generate a sequence
  # for that trial
  # starts [list]: if random_agent == F, provide possible starting points and their probabilities
  # get the number of items in full set e.g. list(c(1, 2, 3), c(.9, .05, .05))
  
  all_trials <- replicate(ntrials, do_one_trial(set = set, 
                                                random_agent = random_agent, 
                                                ps=ps, 
                                                starts=starts), simplify=FALSE)

  # put trial possibilities into one big matrix
  nrows <- length(set)
  ncols <- nrows
  tps <- matrix(0, nrows, ncols) # here is where I'll collect all the transitions
  rownames(tps) <- paste(seq(1, nrows, 1))
  colnames(tps) <- paste(seq(1, ncols, 1))
  
  # count up the randomised transitions and take the probability
  for(i in 1:length(all_trials)) tps[rownames(all_trials[[i]][[2]]), colnames(all_trials[[i]][[2]])] = tps[rownames(all_trials[[i]][[2]]), colnames(all_trials[[i]][[2]])] + all_trials[[i]][[2]]
  tps <- tps /rowSums(tps) # here is the null hypothesis
  tps[is.na(tps)] <- 0 # just in case
  # now count up all the transitions that happened
  all_agent_events <- do.call(rbind, lapply(all_trials, function(x) x[[1]])) # turn actions into a trials x actions matrix 
  tmp <- table(c(all_agent_events[,-ncol(all_agent_events)]), c(all_agent_events[,-1]))
  tmp <- tmp/rowSums(tmp)
  tmp[is.na(tmp)] <- 0 # just in case
  
  # do the matrices match up? if not make them the same shape
  agent_tps <- matrix(0, nrows, ncols)
  rownames(agent_tps) <- rownames(tps)
  colnames(agent_tps) <- colnames(tps)
  agent_tps[rownames(tmp), colnames(tmp)] <- tmp

#  now convert the matrices to a dataframe
  tp_id <- apply(expand.grid(rownames(agent_tps), colnames(agent_tps)), 1, paste, collapse = ".")
  a_tps <- c(agent_tps) # for agent tps
  n_tps <- c(tps) # for null tps

  data_frame(tp_id, a_tps, n_tps)


#  all_trials <- all_trials %>% mutate(xgs = sprintf('%d%d', state, x))
#  state_summary <- all_trials %>% group_by(state) %>% summarise(ep = mean(p)) # get expected value of any next door given state
#  transition_sum <- all_trials %>% group_by(state, xgs) %>% summarise(n=length(p)) # get observed transitions for all the times was in that state
#  nstate <- all_trials %>% group_by(state) %>% summarise(nstate = length(p)) # total nstates visited, denominator for observed ps
#  out <- inner_join(transition_sum, nstate, by="state") %>% mutate(obs=n/nstate) # compute observed ps
#  inner_join(out, state_summary, by=c("state"))
}


do_one_trial <- function(set, random_agent, ps=NA, starts=NA){
  # set [vec]: EITHER the set of possible doors to be selected for that trial (if a random agent)
  # OR the actual doors selected for that trial (if a non-random agent)
  # random_agent [logical] T or F
  # ps [matrix]: if random_agent == F, then provide the transition probs to generate a sequence
  # for that trial
  # starts [list]: if random_agent == F, provide possible starting points and their probabilities
  # get the number of items in full set e.g. list(c(1, 2, 3), c(.9, .05, .05))

  if (random_agent){
    this_set <- sample(2:length(set), size=1)
    # sample a legal set
    legal_set_found <- 0
    while (!legal_set_found){
      this_trials_doors <- sample(set, size=this_set, replace=TRUE)
      if (!(0 %in% diff(this_trials_doors))) legal_set_found = 1 
    }
  } else {
    this_trials_doors <- do_one_trial_ordered_agent(ps, starts)
  }

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
  nset <- length(set)
  nttd <- length(this_trials_doors)
  if(nttd < nset) this_trials_doors <- c(this_trials_doors, rep(NA, times = nset-nttd))
  
  list(this_trials_doors, this_trials_transitions) 
} 

do_one_trial_ordered_agent <- function(ps, starts){
  # this function takes the transition matrix ps, and generates a 
  # sequence, the length of which is determined from a randomly 
  # drawn value between 2 and nset
  # kwargs
  #  -- ps [matrix, nset x nset]: transition ps determining the sequence
  #  -- starts [list, 2 elements]: possible starting points and their probabilities
  # get the number of items in full set e.g. list(c(1, 2, 3), c(.9, .05, .05))
  # returns
  # -- sequence for one trial
  nset <- nrow(ps)
  this_trial_steps <- sample(2:nset, 1)
  
  # simulate the sequence that length
  start <- sample(starts[[1]], size=1, prob = starts[[2]])
  this_trials_sequence <- rep(0, this_trial_steps)
  this_trials_sequence[1] <- start
  for(i in 2:this_trial_steps) {
    this_trials_sequence[i] <- sample(1:nset, 1, prob=ps[this_trials_sequence[i-1],])
  }
  this_trials_sequence
}

#################### RUN SIMULATIONS ######################
# one_set_of_null_sims <-  replicate(nsims, 
#                                    run_one_block_of_trials(set = set, 
#                                                       ntrials = ntrials, 
#                                                       random_agent = TRUE), 
#                                                       simplify=FALSE)
# #
# save.image("/home/kelly/Insync/documents/projects/variability-decisions-analysis/derivatives/null_sims_set8_trials20_N1000.RData")

one_set_of_alt_sims <-  replicate(nsims, 
                                  run_one_block_of_trials(set = set, 
                                                          ntrials = ntrials, 
                                                          random_agent = FALSE,
                                                          ps = ps,
                                                          starts = starts), 
                                   simplify=FALSE)
save.image("/home/kelly/Insync/documents/projects/variability-decisions-analysis/derivatives/alt_sims_set8_trials20_N1000.RData")

################################################################################################
##### Now plot the things! (remember to reload the null sims)
load("/home/kelly/Insync/documents/projects/variability-decisions-analysis/derivatives/null_sims_set8_trials20_N1000.RData")
load("/home/kelly/Insync/documents/projects/variability-decisions-analysis/derivatives/alt_sims_set8_trials20_N1000.RData")


#### Summarise null and plot
tmp <- do.call(rbind, one_set_of_null_sims)
null_sim_results <- tmp
rm(one_set_of_null_sims, tmp)

null_sims <- null_sim_results %>% pivot_longer(c(a_tps, n_tps), names_to = "cond", values_to = "p")
null_sims$sim <- "null"
null_sims$n <- rep(1:nsims, each=length(set)^2*2)

tmp <- do.call(rbind, one_set_of_alt_sims)
alt_sim_results <- tmp
rm(one_set_of_alt_sims, tmp)
alt_sims <- alt_sim_results %>% pivot_longer(c(a_tps, n_tps), names_to = "cond", values_to = "p")
alt_sims$sim <- "alt"
alt_sims$n <- rep(1:nsims, each=length(set)^2*2)

sims <- rbind(null_sims, alt_sims) 


####################################################
# Null plot
# order data for plotting
null_sum_diff <- null_sims %>% group_by(n, tp_id) %>%
                     summarise(diff = p[cond == "a_tps"] - p[cond == "n_tps"]) %>%
                     group_by(tp_id) %>%
                     summarise(mu = mean(diff))

null_sum <- null_sims %>% group_by(tp_id, cond) %>%
                     summarise(mu = mean(p))
idx <- order(null_sum$mu[null_sum$cond == "a_tps"], decreasing = TRUE)
tps <- null_sum$tp_id[null_sum$cond == "a_tps"]
tps <- tps[idx]
act_mu <- null_sum$mu[null_sum$cond == "a_tps"]
act_mu <- act_mu[idx]
null_mu <- null_sum$mu[null_sum$cond == "n_tps"]
null_mu <- null_mu[idx]
diff_mu <- null_sum_diff$mu
diff_mu <- diff_mu[idx]

par(mfrow=c(1,2))
x = seq(1, length(idx), 1)
plot(x, act_mu, ylim = c(0,1), col='blue', ylab="E[p(d=x|s=y)]", xlab="d|s")
points(x, null_mu, type="l", lty=2, col='red', lwd=2)
legend(1, 0.9, legend=c("null", "data"), col=c("red", "blue"), lty=2:1, bty="n")

### alt plot
alt_sum_diff <- alt_sims %>% group_by(n, tp_id) %>%
  summarise(diff = p[cond == "a_tps"] - p[cond == "n_tps"]) %>%
  group_by(tp_id) %>%
  summarise(mu = mean(diff))

alt_sum <- alt_sims %>% group_by(tp_id, cond) %>%
                          summarise(mu = mean(p))
idx <- order(alt_sum$mu[alt_sum$cond == "a_tps"], decreasing = TRUE)

tps <- alt_sum$tp_id[alt_sum$cond == "a_tps"]
tps <- tps[idx]

act_mu <- alt_sum$mu[alt_sum$cond == "a_tps"]
act_mu <- act_mu[idx]
null_mu <- alt_sum$mu[alt_sum$cond == "n_tps"]
null_mu <- null_mu[idx]
diff_mu <- alt_sum_diff$mu
diff_mu <- diff_mu[idx]

plot(x, act_mu, ylim = c(0,1), col='blue', ylab="E[p(d=x|s=y)]", xlab="d|s")
points(x, null_mu, type="l", lty=2, col='red', lwd=2)

