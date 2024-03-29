##########################################################################################################################################
############################ DATA WRANGLES ########################################################################################
##########################################################################################################################################

get_data <- function(fpth, sub_num, ses){
  # this function reads in the behavioural and trial logs, and combines the two to provide a trial by trial dataframe
  # that contains the pre-defined door probabilities and participant response to them
  # input a numeric subject value (not zero padded)
  # -- sub_num = integer
  # -- ses = integer
  if (sub_num < 10){
    sub_str <- paste("0", sub_num, sep="")
  } else {
    sub_str <- paste(sub_num)
  }
  trials_here <- file.exists(sprintf(paste(fpth, 'sub-%s/ses-%d/beh/sub-%s_ses-%d_task-iforage-v1_trls.tsv', sep=""), sub_str, ses, sub_str, ses))
  resps_here <- file.exists(sprintf(paste(fpth, 'sub-%s/ses-%d/beh/sub-%s_ses-%d_task-iforage-v1_beh.tsv', sep=""), sub_str, ses, sub_str, ses))
  
  if (trials_here & resps_here){
    trials <- read.table(sprintf(paste(fpth, 'sub-%s/ses-%d/beh/sub-%s_ses-%d_task-iforage-v1_trls.tsv', sep=""), sub_str, ses, sub_str, ses), header = TRUE)
    resps <- read.table(sprintf(paste(fpth, 'sub-%s/ses-%d/beh/sub-%s_ses-%d_task-iforage-v1_beh.tsv', sep=""), sub_str, ses, sub_str, ses), header = TRUE)
    # remove all practice trials
    trials = trials %>% filter(t != 999) 
    
    # remove the practice trials and the times where participants were not looking at a door,
    # then select the last of the multiple entries for each door, for when there were greater than
    # 100 msec spent on any door
    resps  = resps  %>% filter(cond != 3 & door > 0) # start with practice trials and when no
    # door was registered at all
    resps$t = resps$t - 5 # correct trial numbers (as 1 = first practice trial)
    
    # now, for each set of door presses, compute the total number of samples
    # the door was selected for, if it was greater than 10, then put a 1
    # also create a t column and compute, using the onset column,
    # the ongoing time spent on that door (as recording of depress_dur turned 
    # out to be a bit flakey)
    
    # for counting responses
    count <- 1
    resps$idx <- NA
    resps$idx[1] <- count 
    
    # for getting the timings
    onsets <- resps$onset # for indexing with ease
    rt <- vector(mode="numeric", length=length(onsets))
    
    # get variables
    for (i in 2:length(resps$door)){ # increment an index if the p is on the same door, and that same door belongs to the same trial
      if (resps$door[i] == resps$door[i-1] & resps$t[i] == resps$t[i-1] ){
        count <- count + 1
        rt[i] <- rt[i-1] + diff(c(onsets[i-1], onsets[i]))
      } else {
        count <- 1
        rt[i] <- 0
      }
      resps$idx[i] <- count
    }
    resps$rt <- rt
    # the below comments are here for some sanity checking
    # now, keep only those with > 17 entries as 17 * .017 ~ 300 ms
    # trials of interest - sub 14 - 142 146 152 153 156 157 158 159
    #                               147 148 149 150 151 154 155 160
                                  # 147 148 149 150 151 154 155 160 filter > 17
                                  # 147 148 149 150 151 154 155 160 filter(lead(door,1)) etc
                                  # 147 148 149 150 151 154 155 160 filter....tgt_found == 1 etc
                                  # for Cond 1 - none of the filtering causes a problem
    #                        Cond : 142 146 152 153 156 157 158 159
                                  # 142 146 152 153 156 157 158 159 filter > 17
                                  # 142 146 152 153 156 157 158 159 filter(lead(door,1)) etc
                                  # 142 146 152 153 156 157 158 159 filter....tgt_found == 1 etc
                                  # so this should pass out this number of trials for block 8
    # would be removed from the variability analysis
    resps <- resps %>% filter(idx >= 17) # needs to be >= 
    # and now, keep the last of each door switch, as long as the door was marked as open at the time
    # this latter conditions removes times where participants skipped to a door that was 
    # accidentally marked as open
    # trials removed = 0 
    resps <- resps %>% filter(lead(door,1) != door & open_d == 1 | lead(onset,1) < onset & open_d == 1 | tgt_found == 1) 
    
    # now I re-do removing times where two doors were selected in a row, as filtering by open_d
    # above would have created some double door selections, as in times a door was open, eyes skipped 
    # somewhere else and then went back to the original door
    resps <- resps %>% filter(lead(door,1) != door | tgt_found == 1)
    ### assign probability of location label
    names(trials)[names(trials)=="prob"] = "tgt_prob" # rename to make life easier
    
    ### now combine the two by sub and t
    sub.dat = inner_join(resps, trials, by=c("sub", "t", "sess", "cond"))
    sub.dat
  } else {
    warning(sprintf('check data for sub-%s ses-%d', sub_str, ses))
  }
}

summarise_data_for_ps <- function(data){
  ## wrangles data generated by get_data so that it is summarised with the p for how
  # often participants looked at each door, per session and condition
  
  # Args:
  # -- data: data.frame, data frame based on data from get_data
  #
  # Returns:
  # sum_data: data.frame, containing the following fields:
  # sub [integer] 
  # drug [chr]
  # cond [int]
  # t (trial) [int] 
  # n number times each door was looked at [int]
  # N total number of selections for that trial [int]
  # t trial number [int]
  ###
  
  sum_data <- data %>% group_by(sub, drug, cond, t) %>%
                       count(door) %>%
                       ungroup()
  n_per_trial <- sum_data %>% group_by(sub, drug, cond, t) %>%
                       summarise(N = sum(n))
  sum_data <- inner_join(sum_data, n_per_trial, by = c("sub", "drug", "cond", "t")) %>%
                       mutate(p = n/N)
  sum_data
}

get_door_summary_for_ps <- function(data){
  ## wrangles data generated by get_data to produce a dataframe
  # that contains which of the 16 doors was a target on any given
  # trial
  # Args:
  # -- data: data.frame, data frame based on data from get_data
  #
  # Returns:
  # door_data: data.frame, containing the following fields:
  # sub [int]
  # drug [chr]
  # cond [int]
  # tgt_door [int] the door that was a target on that trial
  ###
  
  door_data <- data %>% group_by(sub, drug, cond, t) %>%
                        summarise(tgt_door = tgt_door[1])

}

insert_block_ns <- function(block_n, ntrials, t, blocks_begin, blocks_fin){
  # return a vector of length t with relevant block numbers
  # -- kwargs
  #   --block_n: [int] total number of blocks
  #   --t: [vector]
  #   --b: [vector, NAs] vector length of t
  #   --block_transitions: [vector] vector of trial numbers for where each block starts
  #   --block_fin: [vector] as above but for where each block ends
  b <- rep(NA, length(t)) # vector for blocks
  bNs <- seq(1, ntrials/block_n, 1) # give me the block numbers
  for (i in 1:length(blocks_begin)){
    b[blocks_begin[i]:blocks_fin[i]] <- bNs[i]
  } 
  b
}

apply_insert_block_Ns <- function(data, subN, sessN, condN, block_n = 10, ntrials = 80){
  # this function applies the function insert_block_Ns
  # to data from a specific sub, session and condition
  # Kwargs:
  # -- data - data frame, of the format produced by get_data
  # -- subN [integer] - subject number e.g. 1
  # -- sessN [string] - session type e.g. 'off' or 'on'
  # -- condN [integer] - condition number e.g. 1 or 2
  # -- block_n [integer] - how many trials per block? default: 10
  # -- n_trials [integer] - how many trials per condition? default = 80
  
  tmp <- data %>% filter(sub == subN & drug == sessN & cond == condN)
  t <- tmp$t # get the trial vector
  transitions <- c(1, which(diff(t)!=0)+1) # returns a vector of ntrials - 1, indicating the transitions between trials     (index, not trial numbers!) The index is the 1 before the change, i.e. where each trial ends - so adding 1 to the index puts you at the start of each trial. Addng 1 to the beginning completes the set of indexes for where N=80 trials begin
  
  blocks_begin <- transitions[seq(1, length(transitions),block_n)] # now I have the index for where each block begins
  blocks_fin <- c(blocks_begin[(blocks_begin-1)>0]-1, length(t)) # and now I subtract 1 from the blocks_begin transitions,
  # remove the first element which is now 0 and add the index for the final entry. This tells me the index for where each block ends
  tmp$b <- insert_block_ns(block_n, ntrials, t, blocks_begin, blocks_fin)
  tmp
}

assign_door_types <- function(tmp){
  # given data from one subject, and session, return the same data with
  # an added variable called 'door_type' coded cc: door was a target door 
  # for current context, oc: door was a target for other context and
  # n: door was a target for neither
  # -- tmp - data produced by apply_insert_block_Ns (or same format)
  # -- RETURNS: tmp - same data with door_type variable added
  tmp$door_type <- NA
  tmp$door_type[tmp$door_p > 0] <- "cc"
  doors <- unique(tmp$door)
  contexts <- unique(tmp$cond)
  
  doors_from_other_context <- function(tmp, this_context){
    # given a context (condition), get the doors that 
    # were relevant for the other context
    # NOTE: this should only be applied to 1 subject 
    # and one session at a time
    
    unique(tmp$door[tmp$cond != this_context & 
                      tmp$door_p > 0
    ])
  }
  
  other_context_door_labels <- lapply(contexts, doors_from_other_context, tmp=tmp)
  names(other_context_door_labels) <- contexts
  
  for (j in names(other_context_door_labels)){
    for(i in other_context_door_labels[[j]]) 
      tmp$door_type[tmp$door == i &
                      tmp$cond == as.numeric(j)] <- 'oc'
  }
  tmp$door_type[is.na(tmp$door_type)] <- "n"
  tmp
}
##########################################################################################################################################
############################ TRANSITION MATRICES ########################################################################################
##########################################################################################################################################

get_transition_matrices <- function(data, ndoors=16){
  # for a given dataset (typically, a trial), get a transition
  # matrix for that block of data
  #
  # Kwargs:
  # -- data: dataframe, data subset down to level of choice
  # -- ndoors: [integer] how many doors were there in the experiment?
  # Output:
  # -- out_mat: UN-NORMALISED MATRIX of transition counts for data 
  out_mat <- matrix(data = 0, nrow = ndoors, ncol = ndoors)
  
  trials <- data
  ntrials <- length(trials$sub)
  for (i in 2:ntrials){
    # rows = where you are, cols = where you just were
    out_mat[ data$door[i], data$door[i-1] ] = out_mat[ data$door[i], data$door[i-1] ] + 1
  }
  #out_mat <- out_mat /sum(out_mat)
  colnames(out_mat) <- paste(seq(1, ndoors, 1))
  rownames(out_mat) <- colnames(out_mat)
  out_mat
}

apply_get_transition_matrices_over_trials <- function(data){
  # take a set of trials, compute the transition counts for each 
  # trial, then when done, normalize the matrix to get probabilities
  # output is a transition matrix for the trials included in data
  # Kwargs:
  # -- data: dataframe, collection of trials for which to compute
  # transition counts. Developed for dataframe blocked_dat
  # Out:
  # -- tps: matrix, ndoors x ndoors, see get_transition_matrices
  
  # first get the trials in this collection of data
  ts <- unique(data$t)
  tmp <- lapply(ts, function(z) get_transition_matrices(data = data %>% filter(t == z)))
  tmp <- Reduce('+', tmp)
  tmp/sum(tmp)
}

apply_get_transition_matrices_over_blocks <- function(data, i=seq(1,8,1)){
  # this function takes data from one subject, one session and one condition, to get
  # the transiton matrices per block, returns a list of 8
  # Kwargs
  # -- data: data filtered down to one sub, session and condition. Developed for 
  #          the dataframe blocked_dat
  lapply(i, function(z) apply_get_transition_matrices_over_trials(data = data %>% filter(b == z)))
}

get_variance_from_tp <- function(tp){
  # given a transition probability matrix, collapse into a vector
  # and take the variance
  # Kargs:
  # -- tp: a numeric matrix
  # Out:
  # -- var_tp: variance of the matrix
  
  var_tp <- var(as.vector(tp))
  var_tp
}

score_transition_matrices_4_one_sub_and_session <- function(data, nblock = 8){
  # for one subject and session, take the variance of their 
  # transition matrices for each condition
  # output is a dataframe containing fields sub, drug, cond, block, var
  # Kwargs:
  # -- data - data filtered to contain only info from one sub from one session
  #           developed on blocked_dat
  # -- nblock - how many blocks of data are we putting together? (default = 8)
  # Outs:
  # -- df - dataframe of the above description
  
  # define indexing variables
  conds <- unique(data$cond)
  nconds <- length(conds)
  sub <- data$sub[1]
  drug <- data$drug[1]
  
  # get matrices for each cond
  # when the below function is applied to the data from sub 14, drug = l session, then when I pass in both
  # conditions, I get NA for cond 1, block 8
  # when I pass in only sub 14, drug l, cond 1, I get values
  # so something is going on with the apply
  tmp <- lapply(conds, function(j) apply_get_transition_matrices_over_blocks(data %>% filter(cond == j)))
  tmp <- lapply(tmp, function(x) lapply(x, get_variance_from_tp))
  tmp <- lapply(tmp, function(x) do.call(rbind, x))
  
  # make df
  tibble(sub = rep(sub, times=nblock*nconds),
         drug = rep(drug, times=nblock*nconds),
         cond = rep(conds, each=nblock),
         block = rep(1:nblock, times=nconds),
         v = do.call(rbind, tmp))
}

##########################################################################################################################################
############################ WRANGLES ON TRANSITION MATRICES ########################################################################################
##########################################################################################################################################

get_t_matrix_data_from_one_element <- function(t_mats, id, n_doors=16, n_block=8){
  # given an id (name of one element from the list t_mats),
  # get the matrices from that list and turn into a dataframe
  # for correlation analyses
  # Kwargs:
  #   -- t_mats - a list of transition probability matrices, with multiple matrices
  #                 assigned to each element of the list
  #   -- id [string]: name of the element you wish to get data from (i.e. sub-1-ses-off-cond-1)
  #   -- n_doors - total number of doors used in the experiment, default = 16
  #   -- n_block - total number of blocks for the analysis, default = 8
  # Returns:
  #   -- tibble containing sub/cond/sess info and the transition probabilities as a vector
  
  mat_info <- str_split(id, '-')[[1]][c(2,4,6)]
  names(mat_info) <- str_split(id, '-')[[1]][c(1,3,5)]
  tps <- t_mats[[id]]
  tps <- unlist(tps)
  if (length(tps)/(n_doors^2) == n_block){
    sub_info <- rep(mat_info['sub'], times=length(tps))
    sess_info <- rep(mat_info['ses'], times=length(tps))
    cond_info <- rep(mat_info['cond'], times=length(tps))
    block_info <- rep(seq(1, n_block, 1), each=n_doors^2)
  } else {
    stop("Something has gone amiss with the number of assumed doors, trials and blocks")
  }
  
  tibble(sub=sub_info,
         sess=sess_info,
         cond=cond_info,
         b=block_info,
         ps=tps) 
}

compute_autocorrelations_win_condition <- function(tps){
  # given one dataframe - i.e. the tibble output from get_t_matrix_data_from_one_element()
  # compute the auto correlations
  # Kwargs:
  #     --tps - tibble output from get_t_matrix_data_from_one_element()
  # Returns:
  #     tibble containing sub, sess, cond and the correlation outputs, starting with 2v1
  #            -- comp = which comparison? e.g. 1 = block 2 & block 1
  sub_num = unique(tps$sub)
  sess_id = unique(tps$sess)
  cond_id = unique(tps$cond)
  b_idx <- seq(min(tps$b)+1, max(tps$b), 1)
  cor_ps <- sapply(b_idx, function(x) cor(tps$ps[tps$b == x], tps$ps[tps$b == (x-1)]))
  tibble(sub = rep(sub_num, length(cor_ps)),
         sess = rep(sess_id, length(cor_ps)),
         cond = rep(cond_id, length(cor_ps)),
         comp = seq(1, length(cor_ps), by=1),
         cor_ps = cor_ps)
}

get_autocorrelations_of_transition_matrices_within_cond <- function(t_mats){
  # taking the list of matrices (t_mats), for each element of the list, 
  # turn the internal matrices to vectors and perform the auto-correlations
  # Kwargs
  #   --t_mats: a list of lists of matrices, assumes each element contains n matrices
  #                 as there are blocks
  #             assumes that the names of the elements follows the format:
  #                   sub-%d-ses-%s-cond-%d
  # Returns
  #   -- a tibble containing the within cond correlations for all participants x
  #           session x cond
  
  idx <- names(t_mats) # get the names of each element
  tps <- lapply(idx, get_t_matrix_data_from_one_element, t_mats=t_mats)
  do.call(rbind, lapply(tps, compute_autocorrelations_win_condition))
}

compute_correlations_btwn_condition <- function(t_mats){
  
  # given the list of list of matrices (of transition probabilities)
  # compute correlation between contexts for each block x sub x sess
  # Kwargs:
  #     --tps - tibble output from get_t_matrix_data_from_one_element()
  # Returns:
  #     tibble containing sub, sess, block and the correlation outputs
  
  idx <- names(t_mats) # get the names of each element
  tps <- lapply(idx, get_t_matrix_data_from_one_element, t_mats=t_mats)
  tps_dat <- do.call(rbind, tps)
  conds <- unique(tps_dat$cond)
  tps_dat %>% group_by(sub, sess, b) %>%
    summarise(r = cor(ps[cond == conds[1]], ps[cond == conds[2]])) %>%
    ungroup()
}