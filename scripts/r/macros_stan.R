################################################################################
##
## [ PROJ ] Digital redlining / HOLC
## [ FILE ] macros_stan.R
## [ AUTH ] Skinner, Levy, Burtch (@btskinner)
## [ INIT ] 18 May 2021
##
################################################################################

stan_seed <- 8643
stan_adapt_delta <- .99
stan_max_depth <- 15L
stan_num_cores <- 4
stan_num_chains <- 4
stan_num_threads <- 1
stan_num_warmup <- 1000L
stan_num_samples <- stan_num_warmup

## -----------------------------------------------------------------------------
## END SCRIPT
################################################################################
