## K. Garner, 2022
## perform modelling of accuracy data as specified in 
## https://osf.io/2y6pk
####------------------------------------------------------
# use this code to apply the bayesian multi-level modelling
# of the accuracy data
# 
# assumes following folder structure:
# -- top/
#      R/code is here
#      data/ 
#        derivatives/ # summary data is here
###-------------------------------------------------------

###------------------------------------------------------
# have you run this model before? define in sourcing file
###-----------------------------------------------------
# new <- TRUE
# verbal <- TRUE
dir.create(sprintf('../data/derivatives/%s', dir_name), showWarnings=FALSE)
mod_name <- dir_name

if (new){

  if (faux){  
    ###-----------------------------------------------------
    # first, generate some data to check the model covers
    # the correct parameters
    ###-----------------------------------------------------
    nsubs <- 40
    intercept = 2
    b <- scale(1:8)
    nb <- 8
    b <- rep(b, times=nsubs)
    betab <- rnorm(nsubs*nb, mean=.5, sd=.2)
    sub_int <- rep(rnorm(nsubs, mean=.2, sd=.4), each=nb)
    log_odds <- intercept + betab*b + sub_int
    # now turn mu into p
    p <- 1/(1+exp(-log_odds))
    # now sample binomial distribution
    trials <- sample(acc_dat$td, size=length(b))
    tt <- mapply(function(x,y) rbinom(1,size=x,prob=y), trials, p)
    faux_dat <- tibble(sub = as.factor(rep(1:nsubs, each=nb)),
                       b = b,
                       tt = tt, 
                       td = trials)
    
    faux_bsubrfx <- brm(formula = tt | trials(td) ~ (b|sub),
                        data = faux_dat,
                        warmup = 2000, iter = 10000,
                        family = binomial,
                        save_pars = save_pars(all=TRUE)) # for model comparisons 
    
    summary(faux_bsubrfx) # happy with this recovery
  }
  
  bsubrfx <- brm(formula = tt | trials(td) ~ (b|sub),
                            data = acc_dat,
                            warmup = 2000, iter = 10000,
                            family = binomial,
                            save_pars = save_pars(all=TRUE)) # for model comparisons 
  
  bsubrfx <- add_criterion(bsubrfx, "loo", moment_match=TRUE, reloo=TRUE)
  # now save!
  save(bsubrfx, file = sprintf('../data/derivatives/%s/%s.Rda', dir_name, mod_name))
  
} else {
  
  load(file = sprintf('../data/derivatives/%s/%s.Rda', dir_name, mod_name))
}

if (verbal){
  # info
  verbal_output(bsubrfx, dir_name = dir_name)
  
  # visualising data using:
  # https://bookdown.org/ajkurz/DBDA_recoded/dichotomous-predicted-variable.html#interpreting-the-regression-coefficients
  # see end of section 22.2.1
  est <- coef(bsubrfx)$sub[, "Estimate", ] %>% as.data.frame() %>%
    mutate(sub = unique(acc_dat$sub))
  check_dat <- inner_join(acc_dat, est, by = "sub")
  check_dat <- check_dat %>%
                   mutate(log_odds = Intercept + b.y*b.x) %>% 
                   mutate(p= 1/(1+exp(-log_odds))) %>%
                   mutate(obs = tt/td)
  
  pdf(file=sprintf('../data/derivatives/%s/predobs_resid.pdf', dir_name))
  check_dat %>% ggplot(aes(x=b.x, y=obs, group=drug, colour=drug)) +
    geom_point() + geom_point(aes(x=b.x, y=p, group=drug, colour=drug), shape=2, inherit.aes = FALSE) +
    facet_wrap(~sub)
  
  # plot the residuals to check nowt too crazy is happening
  check_dat %>% mutate(resid=p-obs) %>% 
    ggplot(aes(x=b.x, y=resid, group=drug, colour=drug)) +
    geom_point()
  dev.off()
}

rm(bsubrfx, check_dat, est)