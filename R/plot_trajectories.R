rm(list=ls())
library(brms)
library(tidyverse)
library(gridExtra)
source('fig_label.R')

figinfo = 's'
w <- 20 # in cm
h <- 20 # in cm

#########################################################
# stuff for the door/routine plots
#########################################################
# now add 2 x 5 search routines on the bottom row
load('../../modelroutines/data/dat_4_viz.RData') # if not working, check file structure

# get some trajectories for plotting
trajectories <- blocked_dat %>% filter(sub %in% c(1,20)) %>%
  filter(cond == 1) %>%
  filter(drug == "levodopa")

xs <- rep(c(1, 2, 3, 4), times = 4)
ys <- rep(c(4, 3, 2, 1), each = 4)

draw_trajectory <- function(dat, trial, xs, ys){
  # t [int] - trial to plot
  
  # set up the door locations
  dat$door_x <- xs[dat$door]
  dat$door_y <- ys[dat$door]
  dat$t <- as.factor(dat$t)
  #### set up the door dataframe for colouring things in
  doors <- data.frame(xs = rep(c(1:4), times=4),
                      ys = rep(c(4:1), each=4))
  doors$tgt <- "ntgt"
  doors$tgt[unique(dat$tgt_door)] <- "tgt"
  doors$tgt[unique(unique(dat$door[dat$door_type == 'oc']))] <- "otgt"
  
  p <- dat %>% filter(t == trial) %>% ggplot(aes(x=door_x, y=door_y)) +
    geom_path(arrow = arrow(length=unit(0.2, "cm")), alpha=0.75) +
    geom_point(alpha=0.75) + xlim(0.5, 4.5) + ylim(0.5, 4.5) +
    geom_point(inherit.aes = FALSE, 
               data = doors,
               aes(x = xs,
                   y = ys,
                   colour = tgt), size = 4, alpha = .3) +
    theme(axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank()) +
    theme(axis.title.y=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks.y=element_blank()) +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_blank()) +
    theme(legend.position = 'none')
  p
}

#########
# first get the trials I want to plot
############
sub1_trials <- with(trajectories, unique(t[sub == 1 & b == 8]))
sub1_trials <- sub1_trials[5:length(sub1_trials)]
sub20_trials <- with(trajectories, unique(t[sub == 20 & b == 8]))
sub20_trials <- sub20_trials[5:length(sub20_trials)]

sub1_ps <-lapply(sub1_trials, draw_trajectory,
                 dat=trajectories %>% filter(sub == 1),
                 xs=xs, ys=ys)
sub20_ps <- lapply(sub20_trials, draw_trajectory,
                   dat=trajectories %>% filter(sub == 20),
                   xs=xs, ys=ys)
all_ps <- c(sub1_ps, sub20_ps)
nsubs = 2

tracs <- grid.arrange(all_ps[[1]], all_ps[[2]],
              all_ps[[3]], all_ps[[4]],
              all_ps[[5]], all_ps[[6]],
              all_ps[[7]], all_ps[[8]],
              all_ps[[9]], all_ps[[10]],
              all_ps[[11]], all_ps[[12]],
              nrow=nsubs, ncol=length(all_ps)/nsubs,
              widths = rep(2.5, length(all_ps)/nsubs), 
              heights = rep(2.5, nsubs),
              left = "sub",
              top = "trials")
ggsave("../images/trajectories2subs_4talk.pdf", tracs, 
       width = w+.5, height = (w/6*2)+.5, units="cm")
