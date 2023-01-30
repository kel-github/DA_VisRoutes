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
load("../data/derivatives/acc_winplusmind_bmndintdrgmndint/acc_winplusmind_bmndintdrgmndint.Rda")
load("../data/derivatives/acc_mindwin_bis/acc_mindwin_bis.Rda")
loo_compare(mndbdrgmnd, acc_mindwin_bis)
# elpd_diff se_diff
# mndbdrgmnd       0.0       0.0   
# acc_mindwin_bis -0.7       0.5
rm(mndbdrgmnd, acc_mindwin_bis)

load("../data/derivatives/cacc_winplusmind/cacc_winplusmind.Rda")
load("../data/derivatives/cacc_mindwin_bis/cacc_mindwin_bis.Rda")
loo_compare(mnd, acc_mindwin_bis)
#               elpd_diff se_diff
# mnd              0.0       0.0   
# acc_mindwin_bis -0.7       0.5  
