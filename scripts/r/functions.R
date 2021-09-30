################################################################################
##
## [ PROJ ] Digital redlining / HOLC
## [ FILE ] functions.R
## [ AUTH ] Skinner, Levy, Burtch (@btskinner)
## [ INIT ] 31 July 2020
##
################################################################################

## log division (assuming all positive and non-missing)
log_divide <- function(num, den) {
    exp(log(num) - log(den))
}

## convert state abbreviation to collapsed state name
stabbr_to_name <- function(stabbr,
                           cw = crosswalkr::stcrosswalk,
                           stabbr_col = "stabbr",
                           stname_col = "stname") {

    stabbr <- toupper(stabbr)
    stname <- cw[stname_col][cw[stabbr_col] == stabbr]
    str_replace_all(stname, "[:space:]", "")
}

## inverse logit function
inv_logit <- function(x) { exp(-log(1 + exp(-x))) }

## -----------------------------------------------------------------------------
## END SCRIPT
################################################################################
