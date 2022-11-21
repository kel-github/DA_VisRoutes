## written by K. Garner, 2022
##################################################################
## score mindfulness questionnaire
## Baer et al (2008). Construct validity of the five facet mindfulness 
## questionnaire in meditating and non-meditating samples: 
## doi: 10.1177/1073191107313003
#################################################################

library(tidyverse)

# get raw data
mind_dat <- read.csv("../data/qresps/mindfulness.csv", header = FALSE)
mind_dat[,1] <- NULL
names(mind_dat)[1] <- "sub"

# make numeric
mind_dat[mind_dat == "Never or very rarely true"] = 1
mind_dat[mind_dat == "Rarely true"] = 2
mind_dat[mind_dat == "Sometimes true"] = 3
mind_dat[mind_dat == "Often true"] = 4
mind_dat[mind_dat == "Very often or always true"] = 5
mind_dat <- apply(mind_dat, 2, as.numeric)

# now reverse score
reversals <- c(12, 16, 22, 5, 8, 13, 18, 23, 28, 34, 38, 3, 10, 14, 17, 25, 30, 35, 39)+1
mind_dat[,reversals] <- 6- mind_dat[,reversals]

# get an average score per subject
mind_sum <- data.frame(sub = mind_dat[,1],
                       m = apply(mind_dat[,2:ncol(mind_dat)], 1, mean, na.rm = T))

save(mind_sum, file="../data/derivatives/mind_scores.Rda")

###############################################################################################
# now do Barrett Impulsiveness Scale
bis_dat <- read.csv('../data/qresps/bis.csv', header = F)
bis_dat[,1] <- NULL
names(bis_dat)[1] <- "sub"

items <- unique(bis_dat[,2])
bis_dat[bis_dat == "Rarely"] <- 1
bis_dat[bis_dat == "Occasionally"] <- 2
bis_dat[bis_dat == "Often"] <- 3
bis_dat[bis_dat == "Almost always/Always"] <- 4
bis_dat <- apply(bis_dat, 2, as.numeric)

# https://www.psytoolkit.org/survey-library/impulsiveness-barratt.html
reversals <- c(1, 7, 8, 9, 10, 12, 13, 15, 20, 29, 30) + 1 
bis_dat[, reversals] <- 5-bis_dat[,reversals]

bis_sum <- data.frame(sub=bis_dat[,1],
                      bis=apply(bis_dat[,2:ncol(bis_dat)], 1, sum, na.rm=T))
save(bis_sum, file="../data/derivatives/bis_scores.Rda")
