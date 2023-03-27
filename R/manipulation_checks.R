## written by K.Garner 2023
## code to analyse manipulation check data from the DA study
rm(list=ls())
library(tidyverse)

## plot settings
w = 12
h = 12


## Participant blinding
## read in the participant blinding data, wrangle into shape and then 
## plot the data against a binomial distribution

## possible outcomes
## correct, correct              - p = .25
## correct, incorrect            - p = .5
## incorrect, incorrect          - p = .25
## the theoretical model is p

# read in the participant guess data and clean
# to get the below csv, I took the responses to the google form:
# UQ Eyetracking participant blinding (Responses) and saved the
# resulting xls file as a csv called the below
gss <- read.csv("../data/derivatives/participant_DA_guess.csv", header=TRUE)
names(gss) <- c("time", "sub", "ans")

# code the answers nicely
gss <- gss %>% mutate(code = if_else(ans == "The placebo (vitamin)", -1, 
                             if_else(ans == "The active dopamine drug (Madopar)", 1, 0)))
# remove old participants (excluded), tidy up the remaining participant labels
# and add the participant sessions
gss <- gss %>% filter(!sub %in% c("21", "21 redo ", "21 take2" ))
# a bit of extra wrangling to remove remaining excluded subjects
# removing the extra sub 16 session (p dropped out)
gss <- gss %>% filter(time != "7/22/2021 11:48:19")
gss$sub[gss$sub == "16 (redo)"] <- "16"
gss <- gss %>% filter(sub != "17.2")
gss$sub[gss$sub == "22.2"] <- "22"
gss$sub[gss$sub == "33 redo 2 "] <- "33"

# make the sub num numeric so it makes sense to sort
gss$sub <- as.numeric(gss$sub)
gss <- gss %>% arrange(sub, time)
# add session
gss <- gss %>% filter(sub <= 40)
nsub <- length(unique(gss$sub))
gss$sess <- rep(c(1,2), times=nsub)

# now join with the DA conditions
drug_info <- read_csv('../data/derivatives/drug-assignment.csv', col_names=T) 
drug_info <- drug_info %>%
  pivot_longer(cols=starts_with('S_'), names_to = 'sess', 
               names_prefix='S_', values_to = 'drug')
drug_info$sess <- as.numeric(drug_info$sess)
gss <- inner_join(gss, drug_info, by=c('sub', 'sess'))
# recode the DA condition for comparison with the guesses
gss <- gss %>% mutate(dcode=if_else(drug == "placebo", -1, 1))
# filter out subjects who said don't know (N=17)
dk <- unique(gss$sub[gss$code == 0])
gss <- gss %>% filter(!sub %in% dk)
# transform -1s to 0s for ease of thought during binomial modelling
gss$code[gss$code == -1] <- 0
gss$dcode[gss$dcode == -1] <- 0

# recode participants as cc, ci, or ii where c = correct and i = incorrect
counts <- gss %>% mutate(correct = as.numeric(code == dcode)) %>% group_by(sub) %>%
                 summarise(sumd = sum(correct)) %>% ungroup() %>%
                 summarise(cc = sum(sumd == 2),
                           ic = sum(sumd == 1),
                           ii = sum(sumd == 0))

prob_test <- dmultinom(counts, prob=c(.25, .5, .25))

# now plot theoretical against the null
ptst_4plt <- rbind(counts/sum(counts), c(.25, .5, .25))
rownames(ptst_4plt) <- c("observed", "expected")

ptst_4plt <- as.matrix(ptst_4plt)

pdf(sprintf("../images/blinding_obsvexp_fig.pdf"),
    width = w/2.54, height = h/2.54) 
barplot(height = ptst_4plt, beside=TRUE, 
        legend=rownames(ptst_4plt), col=c("#377eb8", "#4daf4a"))
dev.off()

## Mood ratings
# read in the participant data and clean
# to get the below csv, I took the responses to the google form:
#UQ Eyetracking participant Mood and BP scale (Responses) and saved the
# resulting xls file as a csv called the below
md <- read.csv("../data/derivatives/mood_BP.csv", header=TRUE)
# tidy up the data 
names(md) <- c("time", "sub", "sess", "tp", "drowsy", "excited", "feeble",
                                            "clear", "clumsy", "energy",
                                            "discont", "tranquil", "qwit",
                                            "relax", "dream", "pro",
                                            "sad", "ami", "bored",
                                             "greg", "sys", "dias")
