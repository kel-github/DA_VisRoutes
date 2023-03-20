## plot the accuracy data, model fits and parameters
####################################################
rm(list=ls())
library(brms)
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
load('../data/derivatives/stereo_winplusmind_dmindint/stereo_winplusmind_dmindint.Rda')

mod <- mndbd_dm
dat_ylim <- c(-9.5, -8.25)
dat_yseq <- seq(-9.5, -8.25, .25)
dat_ylabs <- c("-9.5","","","","","-8.25")

## correlation settings
cor_ylim <- c(-1.2, 0.6)
cor_yseq <- seq(-1.2, 0.57, .2)
cor_ylabs <- c("-1.2","","","","","","","","0.6")
cor_xlim <- c(-2.5, 3)
cor_xseq <- seq(-2.5, 3, .5)
cor_xlabs <- c("-2.5","","","","","0","","","","","","3")

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
# 2. plot drug effect (overall) x mindfulness score
# 3. plot the parameter posteriors for drug * mind (with 95% CI lines)
# 3. plot posteriors for mind
# 4. plot for block
# 5. report the remaining posterior distribution in the text

# so, the 
sum_dat <- rbind(sum_dat %>% filter(drug == "levodopa") %>% 
                     mutate(pred_v = Intercept + b.y*b.x + 
                                                 m.y*m.x + 
                                                `b:druglevodopa`),
                 sum_dat %>% filter(drug == "placebo") %>%
                     mutate(pred_v = Intercept + b.y*b.x +  
                                                 drugplacebo + 
                                                 m.y*m.x + 
                                                 `drugplacebo:m`*m.x +
                                                 `b:drugplacebo`)) %>% 
    mutate(resid = v - pred_v)


# quick residuals check
sum_dat %>% ggplot(aes(x=pred_v, y=resid, colour=sub)) + geom_point() # pretty happy 

# going to generate model predictions for block x drug interaction
# for the mean mindfulness score, as well as + or - 1 standard deviation from the mean

#### summarising data for future correlations with mindfulness
dm <- sum_dat %>% group_by(sub, drug) %>%
                    summarise(pv = mean(pred_v)) %>%
                    group_by(sub) %>%
                    summarise(v = pv[drug == "levodopa"] - pv[drug=="placebo"])
dm <- dm %>% inner_join(mind_sum, dm, by="sub")
dm$m <- scale(dm$m)



###############################################################
# DATA 4 PLOTS 
###############################################################
### first I will summarise all the predicted data, for the first block x drug 
### to give people an idea of the basic level of behaviour
### plot
library(Rmisc)
#sum_dat$b.x <- rep(c(1:8), times=length(sum_dat$b.x)/8)
#sum_dat <- sum_dat[!is.na(sum_dat$v),]
mu_bdrug_dat <- summarySEwithin(data=sum_dat, measurevar=c("v"),
                                          withinvars=c("drug", "b.x"),
                                          idvar="sub") 
mu_bdrug_pred <- summarySEwithin(data=sum_dat, measurevar="pred_v",
                                          withinvars=c("drug", "b.x"),
                                          idvar="sub")

mu_bdrug_dat$b.x <- rep(unique(sum_dat$b.x), 
                        times=length(mu_bdrug_dat$b.x)/length(unique(sum_dat$b.x)))
mu_bdrug_pred$b.x <- rep(unique(sum_dat$b.x), 
                         times=length(mu_bdrug_pred$b.x)/length(unique(sum_dat$b.x)))

# save the summary stats for reporting
save(sum_dat, mu_bdrug_pred, mu_bdrug_dat, dm, file="../data/derivatives/stereo_descriptives.Rda")


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
                          plot(b.x, v,
                               type = "p",
                               pch = 19,
                               ylim=dat_ylim,
                               col=placebo_col,
                               bty="n",
                               xlab = "log(b)",
                               ylab = "log(v)",
                               axes =F))
axis(side=1, at=unique(mu_bdrug_dat$b.x), tick=TRUE, 
                  labels=c("0", "","","","","","","2.1"))
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
legend(x=0, y=-8.25, legend = c("DA", "P"),
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
# DRUG AND MINDFULNESS
#######################################
with(dm, 
     plot(m, v,
          type = "p",
          pch = 19,
          ylim=cor_ylim,
          xlim=cor_xlim,
          col=cor_data_col,
          bty="n",
          xlab = "m",
          ylab = "v (DA - P)",
          axes =F))
axis(side=1, at=cor_xseq, tick=TRUE, labels=cor_xlabs)
axis(side=2, at=cor_yseq, tick=TRUE, labels=cor_ylabs, las=2) #, labels=c())
abline(lm(v ~ m, data = dm), col = "grey17") # is this the best way to show the 
# correlation in the model = probably not. I should maybe get the model to estimate
# v across the values of m, take the diff and plot that line.
# will prob get the same thing though, as these are predicted values?
fig_label("B", cex = 2)


########################################
# PANELS 2, 3: DENSITY OF THE PARAMETER ESTIMATES FOR BLOCK, DRUG
# AND DRUG x MIND
#######################################
variables(mod)
# for stereo, I want the block, drug, and drug x bis parameters
fxb_draws <- posterior_samples(mod, pars="b_b")
plot(density(fxb_draws$b_b),
     col=samples_col, main="", xlab=expression(beta),
     ylab="d", bty="n", xlim=c(-0.15, 0.5), ylim=c(0,10), axes=F)
axis(side=1, at = c(-0.15, 0, .5), labels=c("", "0", "0.5"))
axis(side=2, at=c(0, 10), labels=c("0", "10"), las=2)
polygon(density(fxb_draws$b_b), border=samples_col, col=samples_col)
title("b")
fig_label("C", cex = 2)

fxm_draws <- posterior_samples(mod, pars="b_m")
plot(density(fxm_draws$b_m),
     col=samples_col, main="", xlab="",
     ylab="", bty="n", xlim=c(-.4, 0.15), ylim=c(0,10), axes=F)
axis(side=1, at = c(-0.4, 0, .15), labels=c("-0.4", "0", ""))
axis(side=2, at=c(0, 10), labels=c("0", "10"), las=2)
polygon(density(fxm_draws$b_m), border=samples_col, col=samples_col)
title("m")
fig_label("D", cex = 2)

# posterior_samples is deprecated, use as_draws next time
fxdrg_draws <- posterior_samples(mod, pars="drugplacebo:m")
plot(density(fxdrg_draws$b_drugplacebo),
     col=samples_col, main="", xlab="",
     ylab="", bty="n", xlim=c(-0.15, 0.25), ylim=c(0,10), axes=F)
axis(side=1, at = c(-0.15, 0, 0.25), labels=c("", "0", ".25"))
axis(side=2, at=c(0, 10), labels=c("0", "10"), las=2)
polygon(density(fxdrg_draws$b_drugplacebo), border=samples_col, col=samples_col)
title("DA*m")
fig_label("E", cex = 2)

dev.off()
