################################################################################
##
## [ PROJ ] Digital redlining / HOLC
## [ FILE ] make_data_clean.R
## [ AUTH ] Skinner, Levy, Burtch (@btskinner)
## [ INIT ] 21 August 2020
##
################################################################################

## libraries
libs <- c("data.table", "tidyverse", "crosswalkr")
sapply(libs, require, character.only = TRUE)

## directory paths
args <- commandArgs(trailingOnly = TRUE)
root <- ifelse(length(args) == 0, file.path("..", ".."), args)
dat_dir <- file.path(root, "data")
acs_dir <- file.path(dat_dir, "acs")
cln_dir <- file.path(dat_dir, "clean")
crw_dir <- file.path(dat_dir, "crosswalks")
scr_dir <- file.path(root, "scripts", "r")

## external functions
source(file.path(scr_dir, "functions.R"))

## external macros
source(file.path(scr_dir, "macros.R"))

## data.table options
options(datatable.integer64 = "character")

## ---------------------------
## character
## ---------------------------

get_seq <- function(year,
                    sum_level,
                    seq_num,
                    stabbrs,
                    from_dir,
                    to_dir,
                    cw_dir) {

    ## message
    message(year, ": Sequence ", seq_num, " at ", sum_level, " summary level")

    ## init list to store each state-level data table
    dt_list <- vector("list", length = length(stabbrs))

    ## loop through each state
    for (i in seq_along(stabbrs)) {

        ## -----------------------
        ## primary estimates/moe
        ## -----------------------

        ## get state abbreviation and name
        st <- stabbrs[i]
        stf <- stabbr_to_name(st)

        ## message
        message("  - ", st)

        ## need census tracks or block groups ? first file : second file
        stub <- case_when(
            sum_level %in% c("140", "150") ~ "_Tracts_Block_Groups_Only.zip",
            TRUE ~ "_All_Geographies_Not_Tracts_Block_Groups.zip"
        )

        ## set up file paths for reading
        ## zip_file := dir + state name + stub
        ## ending := sum_level padded to 7 0s (e.g., 140 --> 0000140) + txt
        ## est_file := main estimate file
        ## moe_file := accompanying margin of error file
        zip_file <- file.path(from_dir, paste0(stf, stub))
        ending <- paste0(sprintf("%07d", as.numeric(seq_num) * 1000), ".txt")
        emstub <- paste0(substr(year, 6, 9), "5")
        est_file <- paste0("e", emstub, st, ending)
        moe_file <- paste0("m", emstub,  st, ending)

        ## read in estimates and margin of errors
        ## - need to use system "unzip -p <zip file> <inner file>"
        ##   1. unzip -p := pipe unzipped <inner file> from <zip file> to stdin
        ##   2. data.table::fread() will read from piped stdin
        ## - there's no header (we add from above)
        ## - character class based on file
        ##   - V6 (logrecno for join) has leading 0s, so we read in as character
        dt_e <- fread(cmd = paste("unzip -p", zip_file, est_file),
                      header = FALSE,
                      colClasses = list(character = c("V6")))
        dt_m <- fread(cmd = paste("unzip -p", zip_file, moe_file),
                      header = FALSE,
                      drop = c(paste0("V", 1:5)),
                      colClasses = list(character = c("V6")))

        ## lower column names (e.g., V6 --> v6)
        colnames(dt_e) <- tolower(colnames(dt_e))
        colnames(dt_m) <- tolower(colnames(dt_m))

        ## add "_moe" to all moe columns except v6, which is join key
        colnames(dt_m)[-1] <- paste0(colnames(dt_m)[-1], "_moe")

        ## -----------------------
        ## associated geographies
        ## -----------------------

        dt_geo <- readxl::read_excel(file.path(from_dir, paste0(st, ".xlsx"))) %>%
            rename_all(tolower) %>%
            rename(logrecno = `logical record number`,
                   geoid = `geography id`) %>%
            mutate(geo_level = str_sub(geoid, 1, 3),
                   component_code = str_sub(geoid, 4, 5)) %>%
            select(logrecno, geoid, geo_level, component_code) %>%
            as.data.table()

        ## -----------------------
        ## join everything
        ## -----------------------

        ## 1. join estimates with their margins of error
        ## 2. join geographies
        ## 3. filter to geographic summary level (e.g., state, census track)
        ## 4. drop first part of full GEOID and store in "fips":
        ##    - summary level (3)
        ##    - geo component (2)
        ##    - "US" (2)
        ## 5. drop unnecessary columns
        dt <- dt_e[dt_m, on = "v6"
                   ][dt_geo, on = c("v6" = "logrecno")
                     ][geo_level == sum_level & component_code == "00",
                       ][, fips := str_replace(geoid, "^.+US(.+)$", "\\1")
                         ][, -c("geoid", "component_code")]

        ## -----------------------
        ## last st munge / store
        ## -----------------------

        ## reorder columns to put fips and summary level first
        setcolorder(dt, c("fips", "geo_level"))

        ## store in list
        dt_list[[i]] <- dt
    }

    ## -----------------------
    ## bind
    ## -----------------------

    ## bind all state data.tables into one
    dt <- rbindlist(dt_list)

    ## -----------------------
    ## new column names
    ## -----------------------

    ## new column names to add sequence number to "v#" columns
    ## colnames(dt) <- str_replace_all(colnames(dt),
    ##                                 "^(v[0-9]+)(_moe)?$",
    ##                                 paste0("s", seq_num, "_", "\\1\\2"))

    ## -----------------------
    ## keep names in list
    ## -----------------------

    ## var list
    year_short <- str_replace(year, "20([0-9]+)_20([0-9]+)", "\\1\\2")
    var_list <- get(paste0("nd_", year_short, "_s", seq_num))
    ## variables to keep
    keep_vars <- c("fips",
                   ## paste0("s", seq_num, "_", c(unlist(var_list),
                   ##                             names(var_list)))
                   c(unlist(var_list), names(var_list))
                   ) %>%
        unique %>%
        gtools::mixedsort(.)
    ## select variables (two dots to use "keep_vars" object)
    dt <- dt[, ..keep_vars]

    ## -----------------------
    ## make crosswalk
    ## -----------------------

    ## get sequence file header to make crosswalk
    header_zip <- file.path(from_dir,
                            paste0(substr(year, 6, 9),
                            "_5yr_Summary_FileTemplates.zip"))
    header_file <- paste0("seq", seq_num, ".xlsx")
    header_tmp <- unzip(header_zip, header_file, exdir = tempdir())

    ## munge column header names
    ## 1. are the same for all states, so outside of the loop
    ## 2. replace "%" and spaces with underscores
    ## 3. add s<seq_no>
    cw <- bind_rows(
        ## these two rows first
        tibble(var = c("fips", "geo_level"),
               label = c("FIPS Code", "Geographic summary level")),
        ## now add rows from header file
        readxl::read_excel(header_tmp, skip = 1) %>%
        names %>%
        tolower %>%
        ## clean up names to remove odd characters and spaces
        str_replace_all(" ?% ?", "_") %>%
        str_replace_all("[:space:]+", "_") %>%
        str_replace_all(":", "") %>%
        ## make into a tibble
        tibble(label = .) %>%
        ## add var column; row_number() works if we don't drop cols above
        mutate(var = paste0("v", row_number())) %>%
        select(var, label)
        )

    ## -----------------------
    ## save
    ## -----------------------

    ## write data to disk
    fn <- paste("acs", year, "seq", seq_num, "sumlvl", sum_level, "us.RDS",
                sep = "_")
    save_file <- file.path(to_dir, fn)
    saveRDS(dt, save_file)

    ## write crosswalk to disk: csv
    fn <- paste("acs", year, "seq", seq_num, "sumlvl", sum_level, "us.csv",
                sep = "_")
    save_file <- file.path(cw_dir, fn)
    write_csv(cw, save_file)

    ## write crosswalk to disk: md
    fn <- paste("acs", year, "seq", seq_num, "sumlvl", sum_level, "us.md",
                sep = "_")
    save_file <- file.path(cw_dir, fn)
    writeLines(cw %>%
               mutate(label = str_replace_all(label, "_", " ")) %>%
               knitr::kable(),
               save_file)
}

## -----------------------------------------------------------------------------
## put data together
## -----------------------------------------------------------------------------

## state abbreviations, lowered
stabbrs <- crosswalkr::stcrosswalk %>% pull(stabbr) %>% tolower

## years / sequence number files
## NB: the sequence file number changes over the years, so we use this
## faux tuple structure to pull the correct file for the correct year
year_seq <- list(c("2015_2019", 135))

## summary levels
sum_lvls <- c("140")

## walk through files
for (i in seq_along(year_seq)) {
    for (j in sum_lvls) {
        for (k in year_seq[[i]][-1]) {
            get_seq(year_seq[[i]][1],      # data year
                    j,                     # summary level
                    k,                     # sequence file number
                    stabbrs,               # state abbreviations
                    acs_dir,               # input file location
                    cln_dir,               # output file location
                    crw_dir)               # output crosswalk location
        }
    }
}

## -----------------------------------------------------------------------------
## END SCRIPT
################################################################################
