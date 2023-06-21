  ## plot the accuracy data, model fits and parameters
  ####################################################
  rm(list=ls())
  library(Rmisc)
  library(tidyverse)
  source('fig_label.R')
  
  ##############################################################
  # WHAT ARE YOU PLOTTING?
  ##############################################################
  load('../data/derivatives/accuracy.Rda')
  
  type_dat <- door_acc_sum %>% filter(sub != 21 & !is.na(b)) %>% select(sub, drug, cond, b, cc, oc, n) %>% 
    mutate(N = cc + oc + n, 
           pc = cc/N,
           po = oc/N,
           pn = n/N) %>% select(sub, drug, b, pc, po, pn) %>%
           pivot_longer(cols=c(pc, po, pn), names_to = "type", values_to = "p") %>%
           group_by(sub, drug, type) %>%
           summarise(mu = mean(p))
  type_dat$drug <- as.factor(type_dat$drug)
  type_dat$type <- as.factor(type_dat$type)
  
  dat_ylim <- c(.1, .52)
  dat_yseq <- seq(.1, .52, .05)
  dat_ylabs <- c(".10","","","","","","","",".50")
  figinfo = 'type'
  w = 8
  h = 8
  adjust = 0.3
  
  ##############################################################
  # COLOUR SETTINGS
  ##############################################################
  placebo_col <- "#1b9e77"
  dopa_col <- "#d95f02"
  
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
  
  ###############################################################
  # DATA 4 PLOTS 
  ###############################################################
  
  ### first I will summarise all the predicted data, for the first block x drug 
  ### plot
  
  mu_bdrug_dat <- summarySEwithin(data=type_dat, measurevar=c("mu"),
                                            withinvars=c("drug", "type"),
                                            idvar="sub") 
  
  # save the summary stats for reporting
  save(type_dat, mu_bdrug_dat, file="../data/derivatives/type_descriptives.Rda")
  
  ###############################################################
  # testing boxplot
  ##############################################################
  boxplot(mu ~ drug*type, data=type_dat,
          col = c(dopa_col, placebo_col), frame=FALSE)
  points(1:6, mu_bdrug_dat$mu[c(1,4,2,5,3,6)], 
         col = c(dopa_col, placebo_col), pch = 21, cex=2)
  arrows(x0 = 1:6, y0=rep(0.4, times=6), x1 = 1:6, y1 = rep(0.6, times=6))
  ###############################################################
  #### set up for full plot
  ###############################################################
  pdf(sprintf("../images/%s_fig.pdf", figinfo),
      width = w/2.54, height = h/2.54) 
  
  plot.mat = matrix(c(1, 1, 1, 1),
                    nrow = 2, byrow = T)
  layout(plot.mat)
  
  par(las=1, mgp=c(2,1,0), mar=c(3,3,3,2))
  
  with(mu_bdrug_dat %>% filter(drug == "placebo"), 
                            plot(1:3-adjust, mu,
                                 type = "p",
                                 pch = 19,
                                 cex = 1.5,
                                 ylim=dat_ylim,
                                 col=placebo_col,
                                 bty="n",
                                 xlab = "type",
                                 ylab = "p",
                                 axes =F,
                                 xlim = c(0,3.5)))
  axis(side=1, at=1:3, tick=TRUE, labels=c("cc", "n", "oc"))
  axis(side=2, at=dat_yseq, tick=TRUE, labels=dat_ylabs, las=2) #, labels=c())
  with(mu_bdrug_dat %>% filter(drug == "placebo"), 
                                arrows(x0 = 1:3 - adjust, 
                                y0 = mu - se,
                                x1 = 1:3 - adjust,
                                y1 = mu + se,
                                code = 3,
                                angle = 90,
                                length = .025,
                                col=placebo_col))
  with(mu_bdrug_dat %>% filter(drug == "levodopa"), 
                                points(1:3 + adjust, mu,
                                type = "p",
                                pch = 19,
                                cex = 1.5,
                                col=dopa_col))
  with(mu_bdrug_dat %>% filter(drug == "levodopa"), 
                               arrows(x0 = 1:3 + adjust, 
                                y0 = mu - se,
                                x1 = 1:3 + adjust,
                                y1 = mu + se,
                                code = 3,
                                angle = 90,
                                length = .025,
                                col=dopa_col))
  legend(x=0.5, y=.25, legend = c("DA", "P"),
         col = c(dopa_col,placebo_col), 
         pch = 19, bty = "n", cex = 1)
  
  dev.off()
