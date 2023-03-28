## written by K.Garner 2023
## code to analyse manipulation check data from the DA study
rm(list=ls())
library(tidyverse)
library(BayesFactor)
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
# remove old participants (excluded), tidy up the remaining participant labels
# and add the participant sessions
md <- md %>% filter(!sub %in% c("21", "21 redo ", "21 take2", "21 (redo)", "21 redo"))
# a bit of extra wrangling to remove remaining excluded subjects
# removing the extra sub 16 session (p dropped out)
md <- md %>% filter(!time %in% c("9/6/2021 12:57:30", "9/6/2021 13:32:09")) # rm excl sub 22 s1
md$sub[md$sub == "22.2"] <- "22"
md <- md %>% filter(!time %in% c("7/22/2021 9:31:30", "7/22/2021 10:09:07", "7/22/2021 11:47:07")) # remove excl
# sub 16 who did not complete s2
md$sub[md$sub == "16 (redo)"] <- "16"
# do the same for sub 32 and tidy up
md <- md %>% filter(sub != "33")
md <- md %>% filter(!time %in% c("11/3/2021 14:35:04", "11/3/2021 15:34:05"))
md$sub[md$sub == "33 redo 2"| md$sub == "33 redo" | md$sub == "33 redo 2 " | md$sub == "33 redo "] <- "33"
md$sub <- as.numeric(md$sub)
md <- md %>% filter(sub < 41)
md$sess[1:5] <- c(1,1,1,2,2) # missing for some reason

# now add the dopamine data
drug_info <- read_csv('../data/derivatives/drug-assignment.csv', col_names=T) 
drug_info <- drug_info %>%
  pivot_longer(cols=starts_with('S_'), names_to = 'sess', 
               names_prefix='S_', values_to = 'drug')
drug_info$sess <- as.numeric(drug_info$sess)

md <- inner_join(md, drug_info, by=c('sub', 'sess'))
md <- md %>% arrange(sub, sess, time)

# work out for whom there is missing data
sum <- md %>% group_by(sub) %>% summarise(N = length(tp))
incomplete_subs <- unique(sum$sub[sum$N != 6]) 
md <- md %>% filter(!sub %in% incomplete_subs)
md$sys <- as.numeric(md$sys)
md$dias <- as.numeric(md$dias)

# tidy for aov analysis
md$sub <- as.factor(md$sub)
md$tp <- as.factor(md$tp)
md$sess <- as.factor(md$sess)
md$drug <- as.factor(md$drug)
# now compute total mood score
# The dependent variables for mood was determined as a total score for the three factors 
# Alertness, Contentedness and Calmness, while the items 4,6,8,9,10,12,14, and 16 were 
# reversed scales (Bond & Lader, 1974).
reverse_idx <- names(md)[5:(ncol(md)-3)]
reverse_idx <- reverse_idx[c(4, 6, 8, 9, 10, 12, 14, 16)]
for (i in reverse_idx) md[,i] <- 11-md[,i]
md <- md %>% mutate(mood = rowSums(across(drowsy:greg)))

# calculate mean BVP https://www.ncbi.nlm.nih.gov/pmc/articles/PMC4974855/
# (DBP) + 1/3 [systolic blood pressure (SBP) – DBP]
md <- md %>% mutate(bvp = dias + (1/3)*(sys-dias))
### run Bayes ANOVA on mood, sys and dias

bvp <- anovaBF(bvp ~ drug*tp + sub, data=md, whichRandom = 'sub', progress=TRUE)
plot(bvp)
# bvp suggests MEs of timepoint and drug. get ME of drug
bvp_drug <- Rmisc::summarySEwithin(data=md, measurevar = "bvp", withinvars="drug", idvar="sub")
bvp_drug <- bvp_drug %>% mutate(lower=bvp-ci, upper=bvp+ci)

# mood shows that timepoint is winning model, nothing else to see here
mood <- anovaBF(mood ~ drug*tp + sub, data=md, whichRandom = "sub", progress=TRUE)
plot(mood)

# save the appropriate datas for referencing in the doc
save(mood, bvp, md, gss, counts, file="../data/derivatives/manip_checks.Rda")
