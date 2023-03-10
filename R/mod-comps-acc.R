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
load("../data/derivatives/acc_winplusmind_bmnd/acc_winplusmind_bmnd.Rda")
load("../data/derivatives/acc_winplusmind_dmnd/acc_winplusmind_dmnd.Rda")
load("../data/derivatives/acc_winplusmindbmnd_dmnd/acc_winplusmindbmnd_dmnd.Rda")
load("../data/derivatives/acc_winplusmindbdmnd/acc_winplusmindbdmnd.Rda")
loo_compare(fxbdrg_rfxbdrg, mnd, mndb, mnddrgb, bdrgmdrgmi, mndbdrg3way)
#                 elpd_diff se_diff
# mndbdrg3way      0.0       0.0  
# mnddrgb          0.0       0.7  
# bdrgmdrgmi      -0.2       0.8  
# mndb           -11.8       6.4  
# mnd            -11.9       6.4  
# fxbdrg_rfxbdrg -12.0       6.4  

acc_mind_comp <- loo_compare(fxbdrg_rfxbdrg, mnd, mndb, mnddrgb, bdrgmdrgmi, mndbdrg3way)
print(comp, simplify=FALSE, digits = 2)
save(acc_mind_comp, file="../data/derivatives/acc_mind_loo.Rda")

rm(fxbdrg_rfxbdrg, mnd, mndb, mnddrgb, bdrgmdrgmi, mndbdrg3way)
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
load("../data/derivatives/acc_winplusmindbdmnd/acc_winplusmindbdmnd.Rda")
load("../data/derivatives/acc_mindwin_bis/acc_mindwin_bis.Rda")
loo_compare(mndbdrg3way, acc_mindwin_bis)
# elpd_diff se_diff
# mndbdrg3way      0.0       0.0   
# acc_mindwin_bis -0.2       0.5  
acc_mind_bis_comp <- loo_compare(mndbdrg3way, acc_mindwin_bis)
save(acc_mind_bis_comp, file="../data/derivatives/acc_mind_bis_comp.Rda")
rm(mndbdrg3way, acc_mindwin_bis)

load("../data/derivatives/cacc_winplusmind/cacc_winplusmind.Rda")
load("../data/derivatives/cacc_mindwin_bis/cacc_mindwin_bis.Rda")
loo_compare(mnd, acc_mindwin_bis)
#               elpd_diff se_diff
# mnd              0.0       0.0   
# acc_mindwin_bis -0.7       0.5  
