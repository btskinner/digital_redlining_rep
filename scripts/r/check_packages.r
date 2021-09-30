################################################################################
##
## [ PROJ ] Digital redlining
## [ FILE ] check_packages.r
## [ AUTH ] Benjamin Skinner, Hazel Levy, & Taylor Burtch
## [ INIT ] 18 May 2021
##
################################################################################

## directories
args <- commandArgs(trailingOnly = TRUE)
root <- ifelse(length(args) == 0, file.path("..", ".."), args)
scr_dir <- file.path(root, "scripts", "r")

## packages required for this project
req_packages <- c("tidyverse",
                  "rstan",
                  "data.table",
                  "readxl",
                  "knitr",
                  "gtools",
                  "crosswalkr",
                  "sf",
                  "patchwork",
                  "ggthemes",
                  "lubridate",
                  "xtable",
                  "devtools")

## packages that are not installed
mask <- (req_packages %in% installed.packages()[,"Package"])
miss_packages <- req_packages[!mask]

## install any missing
if (length(miss_packages)) {
    message("Installing missing packages")
    install.packages(miss_packages)
} else {
    message("All required packages found")
}

## check for cmdstanr
if (!("cmdstanr" %in% installed.packages()[,"Package"])) {
    ## install cmdstanr
    install.packages("cmdstanr",
                     repos = c("https://mc-stan.org/r-packages/",
                               getOption("repos")))
    ## install cmdstan under the hood
    cmdstanr::install_cmdstan(cores = getOption("mc.cores", 2))
}

## -----------------------------------------------------------------------------
## END SCRIPT
################################################################################
