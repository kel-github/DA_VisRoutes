## use this code to perform model comparisons of models run using source_....R
# written by K.Garner, 2022

rm(list=ls())

#-----------------------------------------------------------------------------
# load libraries
# ---------------------------------------------------------------------------
library(brms)
library(tidyverse)

#-----------------------------------------------------------------------------
# ACCURACY
# ---------------------------------------------------------------------------
load('../data/derivatives/acc_model-fxbdrg-bdrgsubrfx/acc_model-fxbdrg-bdrgsubrfx.Rda')
load("../data/derivatives/acc_winplusmind/acc_winplusmind.Rda")
load("../data/derivatives/acc_winplusmindbmint/acc_winplusmindbmint.Rda")
loo_compare(fxbdrg_rfxbdrg, mnd, mndb)
                # elpd_diff se_diff
# fxbdrg_rfxbdrg  0.0       0.0   
# mndb           -0.6       1.2   
# mnd            -1.2       0.7 
# no effect of mindfulness, stop here (for mindfulness)
rm(fxbdrg_rfxbdrg, mndb, mnd)
#-----------------------------------------------------------------------------
# CONTEXTUAL ACCURACY
# ---------------------------------------------------------------------------
load('../data/derivatives/cacc_model-fxbdrg-bdrgsubrfx/cacc_model-fxbdrg-bdrgsubrfx.Rda')
load("../data/derivatives/cacc_winplusmind/cacc_winplusmind.Rda")
load("../data/derivatives/cacc_winplusmindbmint/cacc_winplusmindbmint.Rda")
load("../data/derivatives/cacc_mndbdrug/cacc_mndbdrug.Rda") # cacc

loo_compare(fxbdrg_rfxbdrg, mnd, mndb, mnddrgb)
#                 elpd_diff se_diff
# fxbdrg_rfxbdrg  0.0       0.0   
# mndb           -1.0       1.4   
# mnd            -1.6       0.6   
# mnddrgb        -3.3       1.6 
# no effect of mindfulness here either (or at least, its not preferred)

