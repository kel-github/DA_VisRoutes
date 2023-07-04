## plot the accuracy data, model fits and parameters
####################################################
rm(list=ls())
library(brms)
library(Rmisc)
library(tidyverse)
source('fig_label.R')

##############################################################
# WHAT ARE YOU PLOTTING?
##############################################################
load('../data/derivatives/acc_dat4_model.Rda')
load('../data/derivatives/acc_winplusmindbdmnd/acc_winplusmindbdmnd.Rda')
mod <- mndbdrg3way
dat_ylim <- c(.45, .75)
dat_yseq <- seq(.45, .75, .05)
dat_ylabs <- c(".45","","","","","",".75")
figinfo = 'acc'
cor_ylim <- c(-0.3, 0.25)
cor_yseq <- seq(-0.3, 0.25, .05)
cor_ylabs <- c("-0.3","","","","","","","","","","","0.25")
cor_xlim <- c(-2.5, 3)
cor_xseq <- seq(-2.5, 3, .5)
cor_xlabs <- c("-2.5","","","","","0","","","","","","3")
w = 12
h = 12

##############################################################
# PLOT SETTINGS
##############################################################
placebo_col <- "#1b9e77"
dopa_col <- "#d95f02"
samples_col <- "#7570b3"
cor_data_col <- "#e7298a"
  
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
cor_data_col <- t_col(cor_data_col)

###############################################################
# DATA WRANGLES 
###############################################################
acc_dat$b <- rep(c(0:7), times=nrow(acc_dat)/8)
# get predicted values from the model
est <- coef(mod, robust = TRUE)$sub[, "Estimate", ] %>% as.data.frame() %>%
  mutate(sub = unique(acc_dat$sub))
# create a summary dataframe for wrangling and plotting
sum_dat <- inner_join(acc_dat, est, by="sub")


# put the data back together, given the winning model for acc
# to understand the accuracy data I need to:
# 1. plot block x drug
# 2. plot the effect of drug, for people scoring high on mindfulness
# 3. plot the effect of drug for people scoring low on mindfulness
# 4. plot the parameter posteriors for drug x mindfulness, block, and drug 
# 5. plot the remaining posterior distributions for supplemental
sum_dat <- rbind(sum_dat %>% filter(drug == "levodopa") %>% 
                     mutate(log_odds = Intercept + b.y*b.x + 
                              `b:druglevodopa`*b.x + 
                               m.y*m.x + 
                              `b:m`*m.x*b.x),
                   sum_dat %>% filter(drug == "placebo") %>%
                     mutate(log_odds = Intercept + b.y*b.x +  
                              drugplacebo + 
                              `b:drugplacebo`*b.x + 
                               m.y*m.x + 
                              `b:m`*m.x*b.x + 
                              `drugplacebo:m`*m.x +
                              `b:drugplacebo:m`*b.x*m.x)) %>% 
            mutate(p= 1/(1+exp(-log_odds))) %>%
            mutate(obs = tt/td) %>%
            mutate(resid = obs - p)

# quick residuals check
sum_dat %>% ggplot(aes(x=p, y=resid, colour=sub)) + geom_point() # pretty happy 
# with that

###############################################################
# DATA 4 PLOTS 
###############################################################

### first I will summarise all the predicted data, for the first block x drug 
### plot

mu_bdrug_dat <- summarySEwithin(data=sum_dat, measurevar=c("obs"),
                                          withinvars=c("drug", "b.x"),
                                          idvar="sub") 
mu_bdrug_pred <- summarySEwithin(data=sum_dat, measurevar="p",
                                          withinvars=c("drug", "b.x"),
                                          idvar="sub")
mu_bdrug_dat$b.x <- as.numeric(mu_bdrug_dat$b.x)
mu_bdrug_pred$b.x <- as.numeric(mu_bdrug_pred$b.x)

## to show the mindfulness x drug interaction, I will show the difference
## between drug and placebo, collapsed across block, correlated with mindfulness
mnddrgi <- sum_dat %>% group_by(sub, b.x) %>%
                        summarise(diff = p[drug == "levodopa"] - p[drug == "placebo"]) %>%
                        group_by(sub) %>%
                        summarise(mu_diff = mean(diff))
mnddat <- sum_dat[,c("sub", "m.x")] %>% distinct()
mnddrgi <- inner_join(mnddrgi, mnddat, by = "sub")

# save the summary stats for reporting
save(sum_dat, mu_bdrug_pred, mu_bdrug_dat, mnddrgi, file="../data/derivatives/acc_descriptives.Rda")

###############################################################
#### set up for full plot
###############################################################
pdf(sprintf("../images/%s_fig.pdf", figinfo),
    width = w/2.54, height = h/2.54) 

plot.mat = matrix(c(1, 1, 1, 2, 2, 2, 1, 1, 1, 2, 2, 2, 3, 3, 4, 4, 5, 5),
                  nrow = 3, byrow = T)
layout(plot.mat)

par(las=1, mgp=c(2,1,0), mar=c(3,3,3,2))
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
legend(x=6.5, y=.55, legend = c("DA", "P"),
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
# PANEL 2: INTERACTION BETWEEN MINDFULNESS
# AND EFFECT OF DRUG
#######################################
with(mnddrgi, 
     plot(m.x, mu_diff,
          type = "p",
          pch = 19,
          ylim=cor_ylim,
          xlim=cor_xlim,
          col=cor_data_col,
          bty="n",
          xlab = "m",
          ylab = "acc (DA - P)",
          axes =F))
axis(side=1, at=cor_xseq, tick=TRUE, labels=cor_xlabs)
axis(side=2, at=cor_yseq, tick=TRUE, labels=cor_ylabs, las=2) #, labels=c())
abline(lm(mu_diff ~ m.x, data = mnddrgi), col = "grey17")
fig_label("B", cex = 2)
########################################
# PANELS 4, 5, 6: DENSITY OF THE PARAMETER ESTIMATES FOR BLOCK, DRUG
# AND DRUG x MIND
#######################################
variables(mod)
# for acc, I want the block, drug, and drug x bis parameters

fxb_draws <- posterior_samples(mod, pars="b_b")
plot(density(fxb_draws$b_b),
     col=samples_col, main="", xlab="log odds",
     ylab="d", bty="n", xlim=c(-0.1, .4), axes=F)
axis(side=1, at = c(-0.1, 0, .4), labels=c("", "0", "0.4"))
axis(side=2, at= c(0, 25), labels=c("0", "25"), las=2)
polygon(density(fxb_draws$b_b), border=samples_col, col=samples_col)
title("b")
fig_label("C", cex=2)

fxmedrg_draws <- posterior_samples(mod, pars="drugplacebo")
plot(density(fxmedrg_draws$b_drugplacebo),
     col=samples_col, main="", xlab="",
     ylab="", bty="n", xlim=c(-.2, 0.3), axes=F)
axis(side=1, at = c(-0.1, 0, 0.3), labels=c("", "0", "0.3"))
#axis(side=2, at=c(0, 25), labels=c("0", "25"), las=2)
polygon(density(fxmedrg_draws$b_drugplacebo), border=samples_col, col=samples_col)
title("DA")
fig_label("D", cex = 2)

# posterior_samples is deprecated, use as_draws next time
fxdrg_draws <- posterior_samples(mod, pars="b_drugplacebo:m")
plot(density(fxdrg_draws$`b_drugplacebo:m`),
     col=samples_col,main="", xlab="",
     bty="n", xlim=c(-0.2, 0.1), axes=F)
axis(side=1, at = c(-0.2, 0, 0.1), labels=c("-0.2", "0", ""))
polygon(density(fxdrg_draws$b_drugplacebo), border=samples_col, col=samples_col)
title("DA*m")
fig_label("E", cex = 2)

dev.off()
