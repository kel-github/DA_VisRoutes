# print out the plots required for each model
verbal_output <- function(mod, dir_name){
  # mod: brms fitted object
  # dir_name: where do the plots go?

  pdf(file=sprintf('../data/derivatives/%s/ps_and_chains.pdf', dir_name))
    plot(mod, ask=FALSE)
  dev.off()
  
  # now look at some posterior predictive checks
  pdf(file=sprintf('../data/derivatives/%s/pp_check.pdf', dir_name))
    pp_check(mod) # looks pretty good
  dev.off()
}