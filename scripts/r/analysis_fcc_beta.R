################################################################################
##
## [ PROJ ] Digital redlining / HOLC
## [ FILE ] analysis_fcc_beta.R
## [ AUTH ] Skinner, Levy, Burtch (@btskinner)
## [ INIT ] 10 February 2021
##
################################################################################

## libraries
libs <- c("tidyverse", "cmdstanr", "rstan", "sf")
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

## simple random samples
df <- readRDS(file.path(cln_dir, "holc_fcc_analysis.RDS")) %>%
    ## drop zone E
    filter(holc_grade != "E") %>%
    ## munge
    mutate(holc_zone = case_when(
               holc_grade == "A" ~ 1,
               holc_grade == "B" ~ 2,
               holc_grade == "C" ~ 3,
               holc_grade == "D" ~ 4
           ),
           ## combine month/year
           period = case_when(
               year == "2014" & month == "Dec" ~ 1,
               year == "2015" & month == "Jun" ~ 2,
               year == "2015" & month == "Dec" ~ 3,
               year == "2016" & month == "Jun" ~ 4,
               year == "2016" & month == "Dec" ~ 5,
               year == "2017" & month == "Jun" ~ 6,
               year == "2017" & month == "Dec" ~ 7,
               year == "2018" & month == "Jun" ~ 8,
               year == "2018" & month == "Dec" ~ 9,
               year == "2019" & month == "Jun" ~ 10)
           )

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

mod <- cmdstan_model(stan_file = file.path(sta_dir, "model_fcc_beta.stan"),
                     cpp_options = list(stan_threads = TRUE))

## -----------------------------------------------------------------------------
## set up data
## -----------------------------------------------------------------------------

## technologies
techs <- paste0(c("docsis30", "docsis31", "adsl", "adsl2", "vdsl",
                  "fiber"), "_access")

## init list for stan data lists
df_list <- vector("list", length(techs))

## loop through each set of nums/dens
for (i in seq_along(df_list)) {
    ## temporary data frame (collapse for faster model)
    tmp <- df %>%
        filter(tech == techs[i]) %>%
        mutate(acc = acc / 100)
    ## drop early years for docsis31
    if (techs[i] == "docsis31_access") {
        tmp <- tmp %>%
            filter(period > 4) %>%
            mutate(period = period - 4)
    }
    ## add to list
    df_list[[i]] <- list(
        L = tmp %>% distinct(cenreg) %>% nrow,
        M = tmp %>% distinct(stnum) %>% nrow,
        P = tmp %>% distinct(ctnum) %>% nrow,
        T = tmp %>% distinct(period) %>% nrow,
        N = nrow(tmp),
        K_zone = tmp %>% distinct(holc_zone) %>% nrow,
        y = tmp %>% pull(acc),
        period = tmp %>% pull(period) %>% as.integer,
        region = tmp %>% pull(cenreg),
        state = tmp %>% pull(stnum),
        city = tmp %>% pull(ctnum),
        zone = tmp %>% pull(holc_zone) %>% as.integer
    )
}

## add names using numerator
names(df_list) <- techs

## -----------------------------------------------------------------------------
## fit
## -----------------------------------------------------------------------------

## walk through each technology type
for (i in 1:length(df_list)) {
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
                          basename = paste0(names(df_list[i]), "_beta"),
                          timestamp = FALSE,
                          random = FALSE)
}

## -----------------------------------------------------------------------------
## make predictions
## -----------------------------------------------------------------------------

## work through outcomes
pred_list <- vector("list", length = length(techs))

for (i in techs) {
    ## get actual name
    nm <- paste0(i, "_beta")
    ## get draws
    draws <- read_stan_csv(list.files(file.path(est_dir), nm, full.names = TRUE))
    ## get parameters
    pars <- extract(draws)
    ## init tmp list
    tmp_list <- vector("list", length = 10)
    ## loop through periods
    p <- ifelse(nm == "docsis31_access_beta", 6, 10)
    for (j in 1:p) {
        ## preds
        preds <- matrix(NA_real_,
                        nrow = nrow(pars[["beta_zone"]]),
                        ncol = ncol(pars[["beta_zone"]]))
        ## loop through HOLC zone parameters
        for (k in 1:4) {
            preds[,k] <- inv_logit(pars[["alpha"]]
                                   + pars[["beta_zone"]][,k]
                                   + pars[["alpha_period"]][,j]
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
            mutate(group = nm,
                   period = j) %>%
            select(group, period, var, val) %>%
            arrange(var)
        ## add to tmp list
        tmp_list[[j]] <- df
    }
    ## combine periods
    pred_list[[i]] <- bind_rows(tmp_list)
}

## combine
preds <- bind_rows(pred_list) %>%
    mutate(period_f = case_when(
               str_detect(group, "31") & period == 1 ~ "2016-12-01",
               str_detect(group, "31") & period == 2 ~ "2017-06-01",
               str_detect(group, "31") & period == 3 ~ "2017-12-01",
               str_detect(group, "31") & period == 4 ~ "2018-06-01",
               str_detect(group, "31") & period == 5 ~ "2018-12-01",
               str_detect(group, "31") & period == 6 ~ "2019-06-01",
               period == 1 ~ "2014-12-01",
               period == 2 ~ "2015-06-01",
               period == 3 ~ "2015-12-01",
               period == 4 ~ "2016-06-01",
               period == 5 ~ "2016-12-01",
               period == 6 ~ "2017-06-01",
               period == 7 ~ "2017-12-01",
               period == 8 ~ "2018-06-01",
               period == 9 ~ "2018-12-01",
               period == 10 ~ "2019-06-01",
               TRUE ~ NA_character_
           ) %>% as.Date
           )

## save
saveRDS(preds, file.path(cln_dir, "predictions_fcc_beta.RDS"))

## =============================================================================
## END SCRIPT
################################################################################
