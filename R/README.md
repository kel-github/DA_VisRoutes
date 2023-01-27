K. Garner, 2022 

-------------------------------------- 
# Description of how files relate to the model space tested: 

All model outputs are saved in /data/derivatives  

-------------------------------------- 
Data: accuracy (acc) or contextual accuracy (cacc)

## Step 1: What combination of block and drug provide the best model fit?  

R file to run models: 'R/source_acc_models.R' or 'R/source_cacc_models.R'

name: '*_model-bonly' model: main effect of block (ffx) + subject intercept
name: '*_model-fxb_bsubrfx' model: main effect of block (ffx) plus random slope of block for each subject
name: '*_model-int-bsubrfx' model: intercept model + random slope of block per subject
name: '*_model_fxbdrg-brfx' model: main effect of block and drug, random block slopes
name: '*_model-fxbdrug-bdrgsubrfx': main effect of block and drug, random slope for block and random intercept for drug
name: '*_model-fxbdrgint-bdrgrfx': as above, but with an additional block x drug interaction on ffx
name: '*_model-fxbdrgint-brfx': as above, with no random intercept for drug

acc data winner: 'fxbdrg_rfxbdrg' which equates to '*_model-fxbdrug-bdrgsubrfx'
cacc data winner: 'fxbdrg_rfxbdrg' which equates to '*_model-fxbdrug-bdrgsubrfx'

## Step 2: Does adding mindfulness scores improve the model? 

R file to run models: 'R/source_acc_mind_models.R' 
R file to compare models: 'R/mod-comps-acc.R' 

name: '*_model-fxbdrg-bdrgsubrfx' model: winning model from above
name: '*_winplusmind model': winning model plus main effect of mindfulness score
name: '*_winplusmindbmint': above model, with a b*mindfulness interaction
name: '*_winplusminddmint': winplusmindmodel, with a drug*mindfulness interaction
name: '*_winplusmindbmintdmint': above model with a b*mindfulness interaction

STOP HERE AND FIND WINNING MODEL. IF NECESSARY, ADD 3-WAY INTERACTION

