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

# first load data
load('../data/derivatives/cacc_dat4_model.Rda')
# now load model
load('../data/derivatives/cacc_winplusmind/cacc_winplusmind.Rda')
mod <- mnd
dat_ylim <- c(.45, .75)
dat_yseq <- seq(.45, .75, .05)
dat_ylabs <- c(".45","","","","","",".75")
figinfo = 'cacc'
w <- 12 # in cm
h <- 12 # in cm

dat_ylim <- c(.45, .75)
dat_yseq <- seq(.45, .75, .05)
dat_ylabs <- c(".45","","","","","",".75")

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
# 2. plot the correlation between mindfulness and accuracy
# 3. plot the parameter posteriors for drug 
# 4. plot posteriors for block
# 5. report the remaining posterior distributions in table
sum_dat <- rbind(sum_dat %>% filter(drug == "levodopa") %>% 
                     mutate(log_odds = Intercept + b.y*b.x + `b:druglevodopa`*b.x + m.y*m.x),
                 sum_dat %>% filter(drug == "placebo") %>%
                     mutate(log_odds = Intercept + b.y*b.x +  drugplacebo + `b:drugplacebo`*b.x + m.y*m.x)) %>% 
    mutate(p= 1/(1+exp(-log_odds))) %>%
    mutate(obs = tt/td) %>%
    mutate(resid = obs - p)

# quick residuals check
sum_dat %>% ggplot(aes(x=p, y=resid, colour=sub)) + geom_point() # pretty happy 

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

# save the summary stats for reporting
save(sum_dat, mu_bdrug_pred, mu_bdrug_dat, file="../data/derivatives/cacc_descriptives.Rda")

###############################################################
#### set up for full plot
###############################################################
pdf(sprintf("../images/%s_fig.pdf", figinfo),
    width = w/2.54, height = h/2.54) 

plot.mat = matrix(c(1, 2, 1, 3),
                  nrow = 2, byrow = T)

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
                               ylab = "c-acc",
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
legend(x=5.5, y=.55, legend = c("DA", "P"),
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
# PANELS 2, 3: DENSITY OF THE PARAMETER ESTIMATES FOR BLOCK, DRUG
# AND DRUG x MIND
#######################################
variables(mod)
# for acc, I want the block, drug, and drug x bis parameters

# posterior_samples is deprecated, use as_draws next time
fxdrg_draws <- posterior_samples(mod, pars="b_drugplacebo")
plot(density(fxdrg_draws$b_drugplacebo),
     col=samples_col, main="", xlab="log odds",
     ylab="d", bty="n", xlim=c(-0.15, 0.25), ylim=c(0,20), axes=F)
axis(side=1, at = c(-0.15, 0, 0.25), labels=c("-0.15", "0", "0.15"))
axis(side=2, at=c(0, 20), labels=c("0", "20"), las=2)
polygon(density(fxdrg_draws$b_drugplacebo), border=samples_col, col=samples_col)
title("DA vs P")
fig_label("B", cex = 2)

fxb_draws <- posterior_samples(mod, pars="b_b")
plot(density(fxb_draws$b_b),
     col=samples_col, main="", xlab="",
     ylab="", bty="n", xlim=c(-.15, 0.3), ylim=c(0,15), axes=F)
axis(side=1, at = c(-0.15, 0, .3), labels=c("-0.15", "0", "0.3"))
axis(side=2, at=c(0, 15), labels=c("0", "15"), las=2)
polygon(density(fxb_draws$b_b), border=samples_col, col=samples_col)
title("b")
fig_label("C", cex = 2)

dev.off()
