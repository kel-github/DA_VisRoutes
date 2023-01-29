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
formula: ~ b + drug + (b:drug|sub)
cacc data winner: 'fxbdrg_rfxbdrg' which equates to '*_model-fxbdrug-bdrgsubrfx'
formula: ~ b + drug + (b:drug|sub)

## Step 2: Does adding mindfulness scores improve the model? 

R file to run models: 'R/source_acc_mind_models.R' 
R file to compare models: 'R/mod-comps-acc.R' 

name: '*_model-fxbdrg-bdrgsubrfx' model: winning model from above
name: '*_winplusmind model': winning model plus main effect of mindfulness score 
~ b + drug + m + (b:drug|sub)
name: '*_winplusmindbmint': above model, with a b*mindfulness interaction
~ b + drug + m + b:m + (b:drug|sub) 
name: '*_winplusminddmint': winplusmindmodel, with a drug*mindfulness interaction
~ b + drug + m + drug:m + (b:drug|sub) 
name: '*_winplusmindbmintdmint': above model with a b*mindfulness interaction
~ b + drug + m + b:m + drug:m + (b:drug|sub) 

acc winner: mndbrgmnd which equates to '~ b + drug + m + b:m + drug:m + (b:drug|sub) '
cacc winner: mnd which equates to: 'acc_winplusmind'

- as most complex model won for acc, now need to run one with the three-way interaction
name: 'acc_winplusmind_bmndintdrgmndintbmnddrgint'
~ b + drug + m + b:m + drug:m + drug:b:m + (b:drug|sub) 

## Step 3: Adding BIS to the winning models

R file to run models: R/source_acc_bis_models.R
R file to compare models: R/mod-comps-acc.R

Adding BIS a main fx (only) to see if it improves predictiveness of model/soaks up variance from other effects

name: 'acc_mndwin_bis'
name: 'acc_mndwin_bis'
name: 'cacc_mndwin_bis'
name: 'cacc_mndwin_bis'
