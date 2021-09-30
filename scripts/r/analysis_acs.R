################################################################################
##
## [ PROJ ] Digital redlining / HOLC
## [ FILE ] analysis_acs.R
## [ AUTH ] Skinner, Levy, Burtch (@btskinner)
## [ INIT ] 10 February 2021
##
################################################################################

## libraries
libs <- c("tidyverse", "cmdstanr", "rstan")
sapply(libs, require, character.only = TRUE)

## directories
args <- commandArgs(trailingOnly = TRUE)
root <- ifelse(length(args) == 0, file.path('..', ".."), args)
dat_dir <- file.path(root, "data")
cln_dir <- file.path(dat_dir, "clean")
est_dir <- file.path(root, "estimates")
scr_dir <- file.path(root, "scripts", "r")
sta_dir <- file.path(root, "scripts", "stan")

## external macros
source(file.path(scr_dir, "macros.R"))
source(file.path(scr_dir, "macros_stan.R"))

## external functions
source(file.path(scr_dir, "functions.R"))

## -----------------------------------------------------------------------------
## get / munge data
## -----------------------------------------------------------------------------

df <- readRDS(file.path(cln_dir, "holc_acs_analysis.RDS")) %>%
    ## drop E zones
    filter(holc_grade != "E") %>%
    ## convert HOLC zones to numbers
    mutate(holc_zone = case_when(
               holc_grade == "A" ~ 1,
               holc_grade == "B" ~ 2,
               holc_grade == "C" ~ 3,
               holc_grade == "D" ~ 4
           ))

## get state and city number crosswalks for stan
df_stnum <- df %>%
    distinct(stabbr) %>%
    arrange(stabbr) %>%
    mutate(stnum = row_number())

df_ctnum <- df %>%
    distinct(city) %>%
    arrange(city) %>%
    mutate(ctnum  = row_number())

## join back in
df <- df %>%
    left_join(df_stnum, by = "stabbr") %>%
    left_join(df_ctnum, by = "city")

## -----------------------------------------------------------------------------
## compile model
## -----------------------------------------------------------------------------

mod <- cmdstan_model(stan_file = file.path(sta_dir, "model_acs.stan"),
                     cpp_options = list(stan_threads = TRUE))

## -----------------------------------------------------------------------------
## set up data
## -----------------------------------------------------------------------------

## v32 = "v34"       # overall: broadband | computer

## v38 = "v40"       # income bands w/ broadband: <$10k
## v42 = "v44"       # ...[10k 20k)
## v46 = "v48"       # ...[20k 35k)
## v50 = "v52"       # ...[35k 50k)
## v54 = "v56"       # ...[50k 75k)
## v58 = "v60"       # ...75k+

## v131 = "v133"     # broadband | computer: white
## v137 = "v139"     # ...black
## v143 = "v145"     # ...amerind/alnat
## v149 = "v151"     # ...asian
## v155 = "v157"     # ...nhpi
## v161 = "v163"     # ...other race alone
## v167 = "v169"     # ...two or more races
## v173 = "v175"     # ...white non-hispanic
## v179 = "v181"     # ...hispanic/latino

## numerators
nums <- c(paste0("v", c(34,40,44,48,52,56,60,139,145,151,157,163,169,175,181),
                 "_wc"))
## denominators (be sure to match order of < nums >)
dens <- c(paste0("v", c(32,38,42,46,50,54,58,137,143,149,155,161,167,173,179),
                 "_wc"))

## init list for stan data lists
df_list <- vector("list", length(nums))

## loop through each set of nums/dens
for (i in seq_along(df_list)) {
    ## temporary data frame (collapse for faster model)
    tmp <- df %>%
        group_by(cenreg, stnum, ctnum, holc_zone) %>%
        summarise(y = sum(!!sym(nums[i])),
                  total = sum(!!sym(dens[i])),
                  .groups = "drop") %>%
        ungroup
    ## add to list
    df_list[[i]] <- list(
        L = tmp %>% distinct(cenreg) %>% nrow,
        M = tmp %>% distinct(stnum) %>% nrow,
        P = tmp %>% distinct(ctnum) %>% nrow,
        N = nrow(tmp),
        K_zone = tmp %>% distinct(holc_zone) %>% nrow,
        y = tmp %>% pull(y) %>% as.integer,
        total = tmp %>% pull(total) %>% as.integer,
        region = tmp %>% pull(cenreg),
        state = tmp %>% pull(stnum),
        city = tmp %>% pull(ctnum),
        zone = tmp %>% pull(holc_zone) %>% as.integer
    )
}

## add names using numerator
names(df_list) <- nums

## -----------------------------------------------------------------------------
## fit
## -----------------------------------------------------------------------------

## walk through each pair of num/den
for (i in 1:length(df_list[1])) {
    ## fit
    fit <- mod$sample(df_list[[i]],
                      seed = stan_seed,
                      adapt_delta = stan_adapt_delta,
                      max_treedepth = stan_max_depth,
                      iter_warmup = stan_num_warmup,
                      iter_sampling = stan_num_samples,
                      parallel_chains = stan_num_cores,
                      chains = stan_num_chains,
                      threads_per_chain = stan_num_threads)
    ## extract and save
    fit$save_output_files(dir = est_dir,
                          basename = names(df_list[i]),
                          timestamp = FALSE,
                          random = FALSE)
}

## -----------------------------------------------------------------------------
## make predictions
## -----------------------------------------------------------------------------

## work through outcomes
pred_list <- vector("list", length = length(nums))

for (i in nums) {
    ## get draws
    draws <- read_stan_csv(list.files(file.path(est_dir), i, full.names = TRUE))
    ## get parameters
    pars <- extract(draws)
    ## preds
    preds <- matrix(NA_real_,
                    nrow = nrow(pars[["beta_zone"]]),
                    ncol = ncol(pars[["beta_zone"]]))
    ## loop through HOLC zone parameters
    for (j in 1:4) {
        preds[,j] <- inv_logit(pars[["alpha"]]
                               + pars[["beta_zone"]][,j]
                               + rowMeans(pars[["alpha_city"]])
                               + rowMeans(pars[["alpha_state"]])
                               + rowMeans(pars[["alpha_region"]]))
    }
    colnames(preds) <- paste0("beta_", 1:4)
    ## reshape to long tibble
    df <- as_tibble(preds) %>%
        pivot_longer(cols = everything(),
                     names_to = "var",
                     values_to = "val") %>%
        mutate(group = i) %>%
        select(group, var, val) %>%
        arrange(var)
    ## add to list
    pred_list[[i]] <- df
}

## combine
preds <- bind_rows(pred_list) %>%
    mutate(group_f = case_when(
               grepl("v34", group) ~ "Overall",
               grepl("v40", group) ~ "<$10k",
               grepl("v44", group) ~ "[$10k, $20k)",
               grepl("v48", group) ~ "[$20k, $35k)",
               grepl("v52", group) ~ "[$35k, $50k)",
               grepl("v56", group) ~ "[$50k, $75k)",
               grepl("v60", group) ~ "$75k+",
               grepl("v85", group) ~ "Less than HS",
               grepl("v91", group) ~ "Some college",
               grepl("v97", group) ~ "Bachelor's +",
               grepl("v105", group) ~ "Employed",
               grepl("v111", group) ~ "Unemployed",
               grepl("v117", group) ~ "Not in labor force",
               grepl("v137|v139", group) ~ "Black",
               grepl("v143|v145", group) ~ "AI/AK",
               grepl("v149|v151", group) ~ "Asian",
               grepl("v155|v157", group) ~ "NH/PI",
               grepl("v161|v163", group) ~ "Other race",
               grepl("v167|v169", group) ~ "Multiple races",
               grepl("v173|v175", group) ~ "White",
               grepl("v179|v181", group) ~ "Hispanic",
               TRUE ~ NA_character_
           )
           )

## save
saveRDS(preds, file.path(cln_dir, "predictions_acs.RDS"))

## =============================================================================
## END SCRIPT
################################################################################
