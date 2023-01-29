## use this code to perform model comparisons of models run using source_....R
# written by K.Garner, 2022

rm(list=ls())

#-----------------------------------------------------------------------------
# load libraries
# ---------------------------------------------------------------------------
library(brms)
library(tidyverse)

#----------------------------------------------------------------------------
# MINDFULNESS
# ---------------------------------------------------------------------------

#-----------------------------------------------------------------------------
# ACCURACY
# ---------------------------------------------------------------------------
load('../data/derivatives/acc_model-fxbdrg-bdrgsubrfx/acc_model-fxbdrg-bdrgsubrfx.Rda')
load("../data/derivatives/acc_winplusmind/acc_winplusmind.Rda")
load("../data/derivatives/acc_winplusmindbmint/acc_winplusmindbmint.Rda")
load("../data/derivatives/acc_winplusmind_bmndintdrgmndint/acc_winplusmind_bmndintdrgmndint.Rda")
load("../data/derivatives/acc_winplusmind_drgmndint/acc_winplusmind_drgmndint.Rda")
load("../data/derivatives/acc_winplusmind_bmndintdrgmndintbmnddrgint/acc_winplusmind_bmndintdrgmndintbmnddrgint.Rda")
loo_compare(fxbdrg_rfxbdrg, mnd, mndb, mndbdrgmnd, bdrgmdrgmi, mndbdrg3way)
                # elpd_diff se_diff
# mndbdrgmnd       0.0       0.0  
# mndbdrg3way      0.0       0.9  
# bdrgmdrgmi      -0.5       0.7  
# mndb           -10.9       6.6  
# fxbdrg_rfxbdrg -11.3       6.6  
# mnd            -11.9       6.7 

rm(fxbdrg_rfxbdrg, mndb, mnd, mndbdrgmnd, bdrgmdrgmi)
#-----------------------------------------------------------------------------
# CONTEXTUAL ACCURACY
# ---------------------------------------------------------------------------
load('../data/derivatives/cacc_model-fxbdrg-bdrgsubrfx/cacc_model-fxbdrg-bdrgsubrfx.Rda')
load("../data/derivatives/cacc_winplusmind/cacc_winplusmind.Rda")
load("../data/derivatives/cacc_winplusmindbmint/cacc_winplusmindbmint.Rda")
load("../data/derivatives/cacc_winplusmind_bmndintdrgmndint/cacc_winplusmind_bmndintdrgmndint.Rda")
load("../data/derivatives/cacc_winplusmind_drgmndint/cacc_winplusmind_drgmndint.Rda")


loo_compare(fxbdrg_rfxbdrg, mnd, mndb, mndbdrgmnd, bdrgmdrgmi)
#                 elpd_diff se_diff
# mnd             0.0       0.0   
# fxbdrg_rfxbdrg -0.6       1.1   
# mndb           -1.1       0.8   
# bdrgmdrgmi     -2.0       0.5   
# mndbdrgmnd     -2.6       0.9  

#----------------------------------------------------------------------------
# BIS
# ---------------------------------------------------------------------------
load('../data/derivatives/acc_model-fxbdrg-bdrgsubrfx/acc_model-fxbdrg-bdrgsubrfx.Rda')
load("../data/derivatives/acc_winplusbis/acc_winplusbis.Rda")
load("../data/derivatives/acc_winplusbisbbisint/acc_winplusbisbbisint.Rda")
load("../data/derivatives/acc_winplusbisbdbisint/acc_winplusbisbdbisint.Rda")
load("../data/derivatives/acc_winplusbisplus3way/acc_winplusbisplus3way.Rda")
loo_compare(fxbdrg_rfxbdrg, bis, bisb, bisbd, bisbdint)
# elpd_diff se_diff
# bisbd           0.0       0.0   
# bisbdint       -2.7       1.4   
# bisb           -4.5       6.1   
# fxbdrg_rfxbdrg -7.2       7.5   
# bis            -8.2       7.2  
rm(fxbdrg_rfxbdrg, bisb, bis, bisbd, bisbdint)

load('../data/derivatives/cacc_model-fxbdrg-bdrgsubrfx/cacc_model-fxbdrg-bdrgsubrfx.Rda')
load("../data/derivatives/cacc_winplusbis/cacc_winplusbis.Rda")
load("../data/derivatives/cacc_winplusbisbbisint/cacc_winplusbisbbisint.Rda")
loo_compare(fxbdrg_rfxbdrg, bis, bisb)
#               elpd_diff se_diff
# fxbdrg_rfxbdrg  0.0       0.0   
# bis            -0.5       0.5   
# bisb           -1.6       0.6   
