## plot the accuracy data, model fits and parameters
####################################################
rm(list=ls())
library(tidybayes)
library(Rmisc)
library(tidyverse)
library(vioplot)
library(brms)
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

# first load accuracy data
load('../data/derivatives/accuracy.Rda')

acc_dat <- door_acc_sum %>% group_by(sub, drug, b) %>%
  summarise(tt = sum(tt),
            td = sum(td))
acc_dat <- acc_dat[!is.na(acc_dat$b),]
# scale the block factor
acc_dat$b <- scale(acc_dat$b)
acc_dat$sub <- as.factor(acc_dat$sub)
acc_dat$drug <- as.factor(acc_dat$drug)

# now load model
load('../data/derivatives/acc_model-fxdrg-bsubrfx/acc_model-fxdrg-bsubrfx.Rda')
get_variables(fxdrg_bsubrfx)

# get predicted values from the model
est <- coef(fxdrg_bsubrfx)$sub[, "Estimate", ] %>% as.data.frame() %>%
  mutate(sub = unique(acc_dat$sub))

# create a summary dataframe for wrangling and plotting
sum_dat <- inner_join(acc_dat, est, by="sub")

sum_dat <- rbind(sum_dat %>% filter(drug == "levodopa") %>% 
                   mutate(log_odds = Intercept + b.y * b.x),
                 sum_dat %>% filter(drug == "placebo") %>%
                   mutate(log_odds = Intercept + drugplacebo + b.y*b.x)) %>% 
                mutate(p= 1/(1+exp(-log_odds))) %>%
                mutate(obs = tt/td)

###############################################################
# DATA PLOTS 
###############################################################
# as there was a main effect of drug, that did not interact with
# block, I shall produce two plots. The first will be a drug x block
# plot, with real data as dots and the model's estimate as a line

mu_bdrug_dat <- summarySEwithin(data=sum_dat, measurevar="obs",
                                withinvars=c("drug", "b.x"),
                                idvar="sub")
mu_bdrug_pred <- summarySEwithin(data=sum_dat, measurevar="p",
                                 withinvars=c("drug", "b.x"),
                                 idvar="sub")

mu_bdrug_dat$b.x <- as.numeric(mu_bdrug_dat$b.x)
mu_bdrug_pred$b.x <- as.numeric(mu_bdrug_pred$b.x)

par(las=1)
with(mu_bdrug_dat %>% filter(drug == "placebo"), 
                          plot(b.x, obs,
                               type = "p",
                               pch = 19,
                               ylim=c(.4, .7),
                               col=placebo_col,
                               bty="n",
                               xlab = "block",
                               ylab = "acc",
                               axes =F))
axis(side=1, at=1:8, tick=TRUE, labels=c("1", "","","","","","","8"))
axis(side=2, at=seq(.4, .7, .05), tick=TRUE, labels=c(".4", "","","","","",".7"), las=2) #, labels=c())
with(mu_bdrug_dat %>% filter(drug == "placebo"), 
                              arrows(x0 = b.x, 
                              y0 = obs - ci,
                              x1 = b.x,
                              y1 = obs + ci,
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
                              y0 = obs - ci,
                              x1 = b.x,
                              y1 = obs + ci,
                              code = 3,
                              angle = 90,
                              length = .025,
                              col=dopa_col))
legend(x=5, y=.5, legend = c("l", "p"),
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

########################################
# PANEL 2: DIFF ACROSS BLOCK 
#######################################
dat_4_vio <- sum_dat %>% group_by(sub, drug) %>%
               summarise(acc=mean(obs))
par(bty = "n")
vioplot(dat_4_vio$acc ~ dat_4_vio$drug, 
        col = c(dopa_col, placebo_col),
        xlab = "drug", ylab = "acc", yaxt = "n",
        ylim = c(.35, 1))
axis(side=1, at = c(1,2), labels=c("l", "p"))
axis(side=2, at = seq(.35, 1, .05), tick=TRUE,
        labels=c(".35", "", "", "", "", "", "", "", "", "", "", "", "", "1"),
     las=2)

########################################
# PANEL 3: DENSITY OF THE PARAMETER ESTIMATE
#######################################
fxdrg_draws <- posterior_samples(fxdrg_bsubrfx, pars="b_drugplacebo")
plot(density(fxdrg_draws$b_drugplacebo),
     col=samples_col,main="", xlab=expression(beta),
     ylab="d", bty="n", xlim=c(-0.15, 0.15), axes=F)
axis(side=1, at = c(-0.15, 0, 0.15), labels=c("-0.15", "0", "0.15"))
axis(side=2, at=c(0, 30), labels=c("0", "30"), las=2)
polygon(density(fxdrg_draws$b_drugplacebo), border=samples_col, col=samples_col)
# then I'll average over block and plot the main effect of condition