################################################################################
##
## [ PROJ ] Broadband
## [ FILE ] get_data.R
## [ AUTH ] Benjamin Skinner (@btskinner) & Taylor Burtch
## [ INIT ] 25 July 2020
##
################################################################################

## libraries
libs <- c("tidyverse")
sapply(libs, require, character.only = TRUE)

## directory paths
args <- commandArgs(trailingOnly = TRUE)
root <- ifelse(length(args) == 0, file.path("..", ".."), args)
dat_dir <- file.path(root, "data")
bbd_dir <- file.path(dat_dir, "broadband")
acs_dir <- file.path(dat_dir, "acs")
tig_dir <- file.path(dat_dir, "tiger")
hol_dir <- file.path(dat_dir, "holc")
scr_dir <- file.path(root, "scripts", "r")

## macros
cenftp <- "ftp://ftp2.census.gov"

stfips <- crosswalkr::stcrosswalk[["stfips"]] %>%
    str_pad(2, pad = "0")

stnames <- crosswalkr::stcrosswalk[["stname"]] %>%
    str_replace_all(" ", "")

## ---------------------------
## functions
## ---------------------------

source(file.path(scr_dir, "functions.R"))

adjust_fcc_1 <- function(url, file) {
    url1 <- "http://transition.fcc.gov/form477/BroadbandData/Fixed"
    url2 <- "http://www.fcc.gov/form477/BroadbandData/Fixed"
    url3 <- "https://transition.fcc.gov/form477/BroadbandData/Fixed"
    case_when(
        str_detect(file, "Dec2014") ~ file.path(url1, "Dec14/Version%203"),
        str_detect(file, "Dec2015") ~ file.path(url1, "Dec15/Version%204"),
        str_detect(file, "Dec2016") ~ file.path(url1, "Dec16/Version%202"),
        str_detect(file, "Dec2017") ~ file.path(url2, "Dec17/Version%203"),
        str_detect(file, "Dec2018") ~ file.path(url2, "Dec18/Version%202"),
        str_detect(file, "Jun2015") ~ file.path(url1, "Jun15/Version%205"),
        str_detect(file, "Jun2016") ~ file.path(url1, "Jun16/Version%204"),
        str_detect(file, "Jun2017") ~ file.path(url1, "Jun17/Version%203"),
        str_detect(file, "Jun2018") ~ file.path(url1, "Jun18/Version%201"),
        str_detect(file, "Jun2019") ~ file.path(url3, "Jun19/Version%201"),
    )
}

adjust_fcc_2 <- function(url, file) {
    url1 <- "https://www.fcc.gov/form477/BroadbandData/Fixed"
    url2 <- "https://transition.fcc.gov/form477/BroadbandData/Fixed"
    url3 <- "http://transition.fcc.gov/form477/BroadbandData/Fixed"
    case_when(
        str_detect(file, "Dec2014") ~ file.path(url3, "Dec14/Version%203"),
        str_detect(file, "Dec2015") ~ file.path(url3, "Dec15/Version%204"),
        str_detect(file, "Dec2016") ~ file.path(url3, "Dec16/Version%202"),
        str_detect(file, "Dec2017") ~ file.path(url1, "Dec17/Version%203"),
        str_detect(file, "Dec2018") ~ file.path(url1, "Dec18/Version%202"),
        str_detect(file, "Jun2015") ~ file.path(url3, "Jun15/Version%205"),
        str_detect(file, "Jun2016") ~ file.path(url3, "Jun16/Version%204"),
        str_detect(file, "Jun2017") ~ file.path(url2, "Jun17/Version%203"),
        str_detect(file, "Jun2018") ~ file.path(url2, "Jun18/Version%201"),
        str_detect(file, "Jun2019") ~ file.path(url1, "Jun19/Version%201"),
    )
}


check_dir <- function(files, dir) {
    ## 1. compare <files> with those in <dir>
    ## 2. return <files> not already in <dir>
    in_dir <- list.files(dir)
    setdiff(files, in_dir)
}

get_data <- function(files, base_url, dir, method = "auto", mode = "wb") {
    ## check for files locally; if already downloaded, skip
    files <- check_dir(files, dir)
    if (length(files) == 0) {
        message("All files already downloaded")

    } else {
        ## download files
        message("Downloading:")
        message(" <=== ", base_url)
        message(" ===> ", dir)
        walk(files,
             ~ {
                 message("    - ", .x)
                 ext_file <- .x
                 loc_file <- .x
                 if (grepl("form477", base_url)) {
                     if (grepl("without", ext_file)) {
                         base_url <- adjust_fcc_1(base_url, ext_file)
                         message(base_url)
                     } else {
                         base_url <- adjust_fcc_2(base_url, ext_file)
                         message(base_url)
                     }
                 }
                 download.file(url = file.path(base_url, ext_file),
                               destfile = file.path(dir, loc_file),
                               method = method,
                               quiet = TRUE,
                               mode = mode)
             })
    }
}

## -----------------------------------------------------------------------------
## TIGER/Line shapefiles
## -----------------------------------------------------------------------------

## -- TRACT --------
## files; url
tiger_files <- paste("tl_2019", stfips, "tract.zip", sep = "_")
tiger_url <- file.path(cenftp, "geo", "tiger", "TIGER2019", "TRACT")

## download data
get_data(tiger_files, tiger_url, tig_dir)

## -- BG --------
## files; url
tiger_files <- paste("tl_2019", stfips, "bg.zip", sep = "_")
tiger_url <- file.path(cenftp, "geo", "tiger", "TIGER2019", "BG")

## download data
get_data(tiger_files, tiger_url, tig_dir)

## -----------------------------------------------------------------------------
## ACS data
## -----------------------------------------------------------------------------

## ---------------------------
## 2015-2019 5-year estimates
## ---------------------------

## --------------
## area estimates
## --------------

## files; url
acs_files <- c(paste0(stnames,
                      "_All_Geographies_Not_Tracts_Block_Groups",
                      ".zip"),
               paste0(stnames,
                      "_Tracts_Block_Groups_Only",
                      ".zip")) %>% sort

acs_url <- file.path(cenftp, "programs-surveys", "acs", "summary_file",
                     "2019", "data", "5_year_by_state")

## download data
get_data(acs_files, acs_url, acs_dir)

## --------------
## templates
## --------------

## files; url
acs_files <- "2019_5yr_Summary_FileTemplates.zip"

acs_url <- file.path(cenftp, "programs-surveys", "acs", "summary_file",
                     "2019", "data")

## download data
get_data(acs_files, acs_url, acs_dir)
unzip(file.path(acs_dir, acs_files),
      exdir = file.path(acs_dir, "2019_5yr_Summary_FileTemplates"))

## --------------
## mini geo
## --------------

## files; url
acs_files <- paste0(c(tolower(state.abb), "dc", "us", "pr"), ".xlsx")

acs_url <- file.path(cenftp, "programs-surveys", "acs", "summary_file",
                     "2019", "documentation", "geography", "5yr_year_geo")

## download data
get_data(acs_files, acs_url, acs_dir)

## -----------------------------------------------------------------------------
## FCC 477 broadband data
## -----------------------------------------------------------------------------

## files; url stub (since it changes)
fcc_files <- c("US-Fixed-without-Satellite-Dec2014.zip",
               "US-Fixed-without-Satellite-Dec2015.zip",
               "US-Fixed-without-satellite-Dec2016.zip",
               "US-Fixed-without-satellite-Dec2017.zip",
               "US-Fixed-without-Satellite-Dec2018.zip",
               "US-Fixed-without-Satellite-Jun2015.zip",
               "US-Fixed-without-Satellite-Jun2016.zip",
               "US-Fixed-without-Satellite-Jun2017.zip",
               "US-Fixed-without-Satellite-Jun2018.zip",
               "US-Fixed-without-Satellite-Jun2019.zip")
fcc_url <- "form477"

## download data
get_data(fcc_files, fcc_url, bbd_dir)

## files; url stub (since it changes)
fcc_files <- c("US-Fixed-with-Satellite-Dec2014.zip",
               "US-Fixed-with-Satellite-Dec2015.zip",
               "US-Fixed-with-satellite-Dec2016.zip",
               "US-Fixed-with-satellite-Dec2017.zip",
               "US-Fixed-with-Satellite-Dec2018.zip",
               "US-Fixed-with-Satellite-Jun2015.zip",
               "US-Fixed-with-Satellite-Jun2016.zip",
               "US-Fixed-with-Satellite-Jun2017.zip",
               "US-Fixed-with-Satellite-Jun2018.zip",
               "US-Fixed-with-Satellite-Jun2019.zip")
fcc_url <- "form477"

## download data
get_data(fcc_files, fcc_url, bbd_dir)

## -----------------------------------------------------------------------------
## HOLC shapefiles
## -----------------------------------------------------------------------------

hol_file <- "fullshpfile.zip"
hol_url <- file.path("https://dsl.richmond.edu/panorama/redlining/static")

## download data
get_data(hol_file, hol_url, hol_dir)

## unzip and move files
unzip(file.path(hol_dir, hol_file), exdir = hol_dir, junkpaths = TRUE)

## -----------------------------------------------------------------------------
## END SCRIPT
################################################################################
