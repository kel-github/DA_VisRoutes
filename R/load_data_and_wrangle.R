#### K. Garner, 2022. Free to share, please cite
#### This code comes as is, with no guarantees
####------------------------------------------------------
# use this code to load and prepare data from the
# DA_VisRoutes study for the analysis as specified
# here: https://osf.io/2y6pk
# 
#
# assumes following folder structure:
# -- top/
#      R/code is here
#      data/ 
###-------------------------------------------------------

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
} else {
  # load the RData file instead
}
