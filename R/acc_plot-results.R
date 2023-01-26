## plot the accuracy data, model fits and parameters
####################################################
rm(list=ls())
library(vioplot)
library(brms)
library(Rmisc)
library(tidyverse)
library(modeest)
source('fig_label.R')

##############################################################
# WHAT ARE YOU PLOTTING?
##############################################################
acc <- FALSE # if false, you are plotting contectual acc
if (!acc){
  # first load accuracy data
  load('../data/derivatives/cacc_dat4_model.Rda')
  # now load model
  load('../data/derivatives/cacc_winplusmind/cacc_winplusmind.Rda')
  mod <- mnd
  dat_ylim <- c(.45, .75)
  dat_yseq <- seq(.45, .75, .05)
  dat_ylabs <- c(".45","","","","","",".75")
  figinfo = 'cacc'
  w <- 12 # in cm
  h <- 8 # in cm
} else {
  load('../data/derivatives/acc_dat4_model.Rda')
  load('../data/derivatives/acc_winplusbisbdbisint/acc_winplusbisbdbisint.Rda')
  mod <- bisbd
  dat_ylim <- c(.45, .75)
  dat_yseq <- seq(.45, .75, .05)
  dat_ylabs <- c(".45","","","","","",".75")
  figinfo = 'acc'
}
##############################################################
# PLOT SETTINGS
##############################################################
placebo_col <- "#1b9e77"
dopa_col <- "#d95f02"
samples_col <- "#7570b3"
t_col <- function(color, percent = 50, name = NULL) {
      #      color = color name
      #    percent = % transparency
      #       name = an optional name for the color
  
      ## Get RGB values for named color
      rgb.val <- col2rgb(color)
  
      ## Make new color using input color as base and alpha set by transparency
      t.col <- rgb(rgb.val[1], rgb.val[2], rgb.val[3],
                  max = 255,
                  alpha = (100 - percent) * 255 / 100,
                  names = name)
      t.col
}
placebo_col <- t_col(placebo_col)
dopa_col <-t_col(dopa_col)
samples_col <- t_col(samples_col)
###############################################################
# DATA WRANGLES 
###############################################################
acc_dat$b <- rep(c(0:7), times=nrow(acc_dat)/8)
# get predicted values from the model
est <- coef(mod, robust = TRUE)$sub[, "Estimate", ] %>% as.data.frame() %>%
  mutate(sub = unique(acc_dat$sub))
# create a summary dataframe for wrangling and plotting
sum_dat <- inner_join(acc_dat, est, by="sub")

if (acc){
  # put the data back together, given the winning model for acc
  # which was one that 
  sum_dat <- rbind(sum_dat %>% filter(drug == "levodopa") %>% 
                     mutate(log_odds = Intercept + b.y*b.x + `b:druglevodopa`*b.x + bis.y*bis.x + `b:bis`*bis.x*b.x),
                   sum_dat %>% filter(drug == "placebo") %>%
                     mutate(log_odds = Intercept + drugplacebo + b.y*b.x + `b:drugplacebo`*b.x + bis.y*bis.x + `b:bis`*bis.x*b.x + `drugplacebo:bis`*bis.x)) %>% 
    mutate(p= 1/(1+exp(-log_odds))) %>%
    mutate(obs = tt/td) %>%
    mutate(resid = obs - p)

  
} else {
  sum_dat <- rbind(sum_dat %>% filter(drug == "levodopa") %>% 
                     mutate(log_odds = Intercept + b.y * b.x + `b:druglevodopa`*b.x + m.x*m.y),
                   sum_dat %>% filter(drug == "placebo") %>%
                     mutate(log_odds = Intercept + drugplacebo + b.y*b.x + `b:drugplacebo`*b.x + m.x*m.y)) %>% 
    mutate(p= 1/(1+exp(-log_odds))) %>%
    mutate(obs = tt/td) %>%
    mutate(resid = obs-p)
    
    # first, plot residuals by predicted
    sum_dat %>% ggplot(aes(x=p, y=resid, colour=sub)) + geom_point()
    # the residuals plot suggests that the model is underpredicting
    # for a bunch of subs @.7 and over-predicting for possibly the same 
    # subs
    sum_dat %>% ggplot(aes(x=p, y=resid, colour=sub)) + geom_point() +
      facet_wrap(~drug)
    # this seems to be more the case for placebo, but it is hapenning
    # in the opposite direction for subs per condition
    # now going to plot observed vesus predicted per sub
    sum_dat %>% ggplot(aes(x=b.x, y=obs, colour=drug, group=drug)) + 
      geom_point() + geom_line(inherit.aes=F, aes(x=b.x, y=p, colour=drug, group=drug)) +
      facet_wrap(~sub)
    
    sum_dat %>% ggplot(aes(x=b.x, y=p, group=drug, colour=drug)) + geom_line() + facet_wrap(~sub, scales="free")
}
###############################################################
# DATA PLOTS 
###############################################################
# as there was a main effect of drug, that did not interact with
# block, I shall produce two plots. The first will be a drug x block
# plot, with real data as dots and the model's estimate as a line

med_sum <- sum_dat %>% group_by(drug, b.x) %>% summarise(obs = median(obs),
                                              p = median(p))

mu_bdrug_dat <- summarySEwithin(data=sum_dat, measurevar=c("obs"),
                                          withinvars=c("drug", "b.x"),
                                          idvar="sub") 
mu_bdrug_pred <- summarySEwithin(data=sum_dat, measurevar="p",
                                          withinvars=c("drug", "b.x"),
                                          idvar="sub")
mu_bdrug_dat$b.x <- as.numeric(mu_bdrug_dat$b.x)
mu_bdrug_pred$b.x <- as.numeric(mu_bdrug_pred$b.x)
###############################################################
#### set up for full plot
###############################################################
pdf(sprintf("../images/%s_fig.pdf", figinfo),
    width = w/2.54, height = h/2.54) 

if (!acc){
  plot.mat = matrix(c(1, 1, 2, 2), nrow = 1,
                    byrow = T)
} else {  
plot.mat = matrix(c(1, 1, 2, 2, 3, 3, 4, 4),
                  nrow = 2, byrow = T)
}
layout(plot.mat)

par(las=1, mgp=c(2,1,0))
with(mu_bdrug_dat %>% filter(drug == "placebo"), 
                          plot(b.x, obs,
                               type = "p",
                               pch = 19,
                               ylim=dat_ylim,
                               col=placebo_col,
                               bty="n",
                               xlab = "block",
                               ylab = "acc",
                               axes =F))
axis(side=1, at=1:8, tick=TRUE, labels=c("1", "","","","","","","8"))
axis(side=2, at=dat_yseq, tick=TRUE, labels=dat_ylabs, las=2) #, labels=c())
with(mu_bdrug_dat %>% filter(drug == "placebo"), 
                              arrows(x0 = b.x, 
                              y0 = obs - se,
                              x1 = b.x,
                              y1 = obs + se,
                              code = 3,
                              angle = 90,
                              length = .025,
                              col=placebo_col))
with(mu_bdrug_dat %>% filter(drug == "levodopa"), 
                              points(b.x, obs,
                              type = "p",
                              pch = 19,
                              col=dopa_col))
with(mu_bdrug_dat %>% filter(drug == "levodopa"), 
                             arrows(x0 = b.x, 
                              y0 = obs - se,
                              x1 = b.x,
                              y1 = obs + se,
                              code = 3,
                              angle = 90,
                              length = .025,
                              col=dopa_col))
legend(x=6.5, y=.55, legend = c("l", "p"),
       col = c(dopa_col,placebo_col), 
       pch = 19, bty = "n", cex = 1)
# now add lines from the model
with(mu_bdrug_pred %>% filter(drug == "placebo"),
               points(b.x, p, type="l",
                      lty=2, col=placebo_col,
                      lwd=1.5))
with(mu_bdrug_pred %>% filter(drug == "levodopa"),
                      points(b.x, p, type="l",
                      lty=2, col=dopa_col,
                      lwd=1.5))
fig_label("A", cex = 2)

########################################
# PANEL 3: DENSITY OF THE PARAMETER ESTIMATE
#######################################
variables(mod)
# for acc, I want the block, drug, and drug x bis parameters

fxdrg_draws <- posterior_samples(mod, pars="b_drugplacebo")
plot(density(fxdrg_draws$b_drugplacebo),
     col=samples_col,main="", xlab="log odds",
     ylab="d", bty="n", xlim=c(-0.15, 0.15), axes=F)
axis(side=1, at = c(-0.15, 0, 0.15), labels=c("-0.15", "0", "0.15"))
axis(side=2, at=c(0, 25), labels=c("0", "25"), las=2)
polygon(density(fxdrg_draws$b_drugplacebo), border=samples_col, col=samples_col)
fig_label("B", cex = 2)

if(acc){
  # and now I plot the block x drug interaction
  fxbdrg_draws <- posterior_samples(mod, pars="b_b:drugplacebo")
  plot(density(fxbdrg_draws$`b_b:drugplacebo`),
       col=samples_col,main="", xlab="log odds",
       ylab="d", bty="n", xlim=c(-0.15, 0.15), axes=F)
  axis(side=1, at = c(-0.15, 0, 0.15), labels=c("-0.15", "0", "0.15"))
  axis(side=2, at=c(0, 10), labels=c("0", "10"), las=2)
  polygon(density(fxbdrg_draws$`b_b:drugplacebo`), border=samples_col, col=samples_col)
  fig_label("C", cex = 2)
}
dev.off()
