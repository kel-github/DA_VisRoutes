rm(list=ls())
###------------------------------------------------------
# load packages
###-----------------------------------------------------
library(brms)
library(tidyverse)
source('verbal.R')

new <- TRUE
verbal <- TRUE
faux <- FALSE

# these variable define the data that will be called in
# and how model results will be named when saved
# the elements of the two vectors should match up, so position one of each
# corresponds to the same data and what you want to do with that data
# this will form the loop that is used to fit the various models
data_names <- c('cacc')
msv_fnms <- c('cacc_mindwin_bis') # save names

# these vectors contain the model files that you want to run, and any appends that
# should be made to the msv_fnms above (again, make sure that the elements across
# the two vectors correspond. these vectors will
# be looped over within the data/naming loop above
rfs <- c("acc_mindwin_bis.R")
app <- c(NA)

for (i in 1:length(data_names)){
  
  ###------------------------------------------------------
  # load data
  ###-----------------------------------------------------
  load(sprintf('../data/derivatives/%s_dat4_model.Rda', data_names[i]))
  load('../data/derivatives/mind_scores.Rda')
  mind_sum$sub <- as.factor(mind_sum$sub)
  acc_dat <- inner_join(acc_dat, mind_sum, by="sub")
  acc_dat$m <- scale(acc_dat$m)
  load('../data/derivatives/bis_scores.Rda')
  bis_sum$sub <- as.factor(bis_sum$sub)
  acc_dat <- inner_join(acc_dat, bis_sum, by="sub")
  acc_dat$bis <- scale(acc_dat$bis)
  
  for (j in 1:length(rfs)){
    if (is.na(app[j])){
      dir_name <- paste(msv_fnms[i], sep="")
    } else {
      dir_name <- paste(msv_fnms[i], app[j], sep="")
    }
    mod_name <- dir_name
    source(rfs[j])
  }
}







