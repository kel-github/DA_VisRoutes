## written by K.Garner 2023
## code to analyse manipulation check data from the DA study
rm(list=ls())
library(tidyverse)

## Participant blinding
## read in the participant blinding data, wrangle into shape and then 
## plot the data against a binomial distribution

## possible outcomes
## correct, correct              - p = .25
## correct, incorrect            - p = .5
## incorrect, incorrect          - p = .25
## the theoretical model is p

# read in the participant guess data and clean
gss <- read.csv("../data/derivatives/participant_DA_guess.csv", header=TRUE)
names(gss) <- c("time", "sub", "ans")
gss <- gss %>% filter(sub <= 40)
# code the answers nicely
gss <- gss %>% mutate(code = if_else(ans == "The placebo (vitamin)", -1, 
                             if_else(ans == "The active dopamine drug (Madopar)", 1, 0)))
# remove old participants (excluded), tidy up the remaining participant labels
# and add the participant sessions
gss %>% arrange(sub, time)



## Mood ratings
