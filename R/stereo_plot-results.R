## plot the accuracy data, model fits and parameters
####################################################
rm(list=ls())
library(vioplot)
library(brms)
#library(Rmisc)
library(modeest)
library(tidyverse)
source('fig_label.R')

##############################################################
# WHAT ARE YOU PLOTTING?
##############################################################
# load data
###-----------------------------------------------------
load('../data/derivatives/dat4_seq_model.Rda')
load('../data/derivatives/mind_scores.Rda')
sub_var_dat$sub <- as.factor(sub_var_dat$sub)
mind_sum$sub <- as.factor(mind_sum$sub)
sub_var_dat <- inner_join(sub_var_dat, mind_sum, by="sub")
sub_var_dat$drug <- as.factor(sub_var_dat$drug) # makes no difference if fct or chr
sub_var_dat$m <- scale(sub_var_dat$m)

# now load model
load('../data/derivatives/stereo_winplusmind_bdmindint/stereo_winplusmind_bdmindint.Rda')

mod <- mndbd_bdm
dat_ylim <- c(-9.5, -8.25)
dat_yseq <- seq(-9.5, -8.25, .25)
dat_ylabs <- c("-9.5","","","","","-8.25")
figinfo = 's'
w <- 12 # in cm
h <- 12 # in cm


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
# get predicted values from the model
est <- coef(mod, robust = TRUE)$sub[, "Estimate", ] %>% as.data.frame() %>%
            mutate(sub = unique(sub_var_dat$sub))
# create a summary dataframe for wrangling and plotting
sum_dat <- inner_join(sub_var_dat, est, by="sub")

# put the data back together, given the winning model 
# to understand the data I need to:
# 1. plot block x drug
# 2. plot block diff x mindfulness score
# 3. plot the parameter posteriors for block (with 95% CI lines)
# 3. plot posteriors for drug
# 4. plot for block x mindfulness
# 5. report the remaining posterior distribution in the text

# so, the 
sum_dat <- rbind(sum_dat %>% filter(drug == "levodopa") %>% 
                     mutate(pred_v = Intercept + b.y*b.x + 
                                                 m.y*m.x + 
                                                `b:m`*b.x*m.x),
                 sum_dat %>% filter(drug == "placebo") %>%
                     mutate(pred_v = Intercept + b.y*b.x +  
                                                 drugplacebo + 
                                                 m.y*m.x + 
                                                 `b:m`*b.x*m.x +
                                                 `drugplacebo:m`*m.x +
                                                 `b:drugplacebo:m`*b.x*m.x)) %>% 
    mutate(resid = log(v) - pred_v)

# quick residuals check
sum_dat %>% ggplot(aes(x=pred_v, y=resid, colour=sub)) + geom_point() # pretty happy 

# going to generate model predictions for block x drug interaction
# for the mean mindfulness score, as well as + or - 1 standard deviation from the mean



#### summarising data for future correlations with mindfulness

bm <- sum_dat %>% group_by(sub, b.x, drug) %>% summarise(v = mean(v, na.rm=T)) %>%
                   group_by(sub, drug) %>%
                   summarise(st = mean(v[b.x==min(b.x)]),
                             en = mean(v[b.x==max(b.x)])) %>%
                   group_by(sub, drug) %>%
                   summarise(b = en-st)
bm <- inner_join(bm, mind_sum, by="sub")
bm$m <- scale(bm$m)

###############################################################
# DATA 4 PLOTS 
###############################################################

### first I will summarise all the predicted data, for the first block x drug 
### to give people an idea of the basic level of behaviour
### plot
library(Rmisc)
sum_dat$b.x <- rep(c(1:8), times=length(sum_dat$b.x)/8)
#sum_dat <- sum_dat[!is.na(sum_dat$v),]
sum_dat$v <- log(sum_dat$v)
mu_bdrug_dat <- summarySEwithin(data=sum_dat, measurevar=c("v"),
                                          withinvars=c("drug", "b.x"),
                                          idvar="sub") 
mu_bdrug_pred <- summarySEwithin(data=sum_dat, measurevar="pred_v",
                                          withinvars=c("drug", "b.x"),
                                          idvar="sub")
mu_bdrug_dat$b.x <- as.numeric(mu_bdrug_dat$b.x)
mu_bdrug_pred$b.x <- as.numeric(mu_bdrug_pred$b.x)

###############################################################
#### set up for full plot
###############################################################
pdf(sprintf("../images/%s_fig.pdf", figinfo),
    width = w/2.54, height = h/2.54) 

plot.mat = matrix(c(1, 2, 1, 3),
                  nrow = 2, byrow = T)

layout(plot.mat)

par(las=1, mgp=c(2,1,0))
with(mu_bdrug_dat %>% filter(drug == "placebo"), 
                          plot(b.x, v,
                               type = "p",
                               pch = 19,
                               ylim=dat_ylim,
                               col=placebo_col,
                               bty="n",
                               xlab = "block",
                               ylab = "log(v)",
                               axes =F))
axis(side=1, at=1:8, tick=TRUE, labels=c("1", "","","","","","","8"))
axis(side=2, at=dat_yseq, tick=TRUE, labels=dat_ylabs, las=2) #, labels=c())
with(mu_bdrug_dat %>% filter(drug == "placebo"), 
                              arrows(x0 = b.x, 
                              y0 = v - se,
                              x1 = b.x,
                              y1 = v + se,
                              code = 3,
                              angle = 90,
                              length = .025,
                              col=placebo_col))
with(mu_bdrug_dat %>% filter(drug == "levodopa"), 
                              points(b.x, v,
                              type = "p",
                              pch = 19,
                              col=dopa_col))
with(mu_bdrug_dat %>% filter(drug == "levodopa"), 
                             arrows(x0 = b.x, 
                              y0 = v - se,
                              x1 = b.x,
                              y1 = v + se,
                              code = 3,
                              angle = 90,
                              length = .025,
                              col=dopa_col))
legend(x=1, y=.0004, legend = c("DA", "P"),
       col = c(dopa_col,placebo_col), 
       pch = 19, bty = "n", cex = 1)
# now add lines from the model
with(mu_bdrug_pred %>% filter(drug == "placebo"),
               points(b.x, pred_v, type="l",
                      lty=2, col=placebo_col,
                      lwd=1.5))
with(mu_bdrug_pred %>% filter(drug == "levodopa"),
                      points(b.x, pred_v, type="l",
                      lty=2, col=dopa_col,
                      lwd=1.5))
fig_label("A", cex = 2)

########################################
# PANEL 2, INTERACTION BETWEEN
# BLOCK 
#######################################



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
