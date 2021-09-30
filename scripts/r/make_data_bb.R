################################################################################
##
## [ PROJ ] Digital redlining / HOLC
## [ FILE ] make_data_bb.R
## [ AUTH ] Skinner, Levy, Burtch (@btskinner)
## [ INIT ] 23 July 2020
##
################################################################################

## libraries
libs <- c("data.table", "tidyverse")
sapply(libs, require, character.only = TRUE)

## directory paths
args <- commandArgs(trailingOnly = TRUE)
root <- ifelse(length(args) == 0, file.path("..", ".."), args)
dat_dir <- file.path(root, "data")
bbd_dir <- file.path(dat_dir, "broadband")
cln_dir <- file.path(dat_dir, "clean")

## -----------------------------------------------------------------------------
## FCC 477 broadband data
## -----------------------------------------------------------------------------

## files list
files <- list.files(bbd_dir,
                    pattern = ".+-with-.+.zip$",
                    full.names = TRUE)

## preallocate lists
dt_bg_list <- vector("list", length(files))

## ----------------------------
## loop through raw files
## ----------------------------

for (i in seq_along(files)) {

    ## file
    file <- files[i]

    ## set up year/month variables
    regex_pattern <- "^.+-[Ss]atellite-(.{3}+)([0-9]{4}+)\\.zip$"
    f_month <- str_replace(file, regex_pattern, "\\1")
    f_year <- str_replace(file, regex_pattern, "\\2")

    ## message
    message("Working with: ", f_month, " ", f_year)

    ## read with data.table b/c fast
    dt <- fread(cmd = paste("unzip -p", file),
                ## only need these columns
                select = c("Provider_Id",
                           "BlockCode",
                           "Consumer",
                           "TechCode",
                           "MaxAdDown",
                           "MaxAdUp"),
                ## some state FIPS have leading 0s so this keeps them
                colClasses = list(character = c("BlockCode")))

    ## lower column names
    names(dt) <- tolower(names(dt))

    ## subset to totally unique rows
    dt <- unique(dt)

    ## filter only to consumer rows
    dt <- dt[consumer == 1,
             ## drop consumer column
             ][, -c("consumer")]

    ## set var name; set length to cut geoid
    var <- "bg"
    gid <- 12

    ## make census tract/bg from block code
    ## tract: state (2) + county (3) + tract (6)
    ## bg: state (2) + county (3) + tract (6) + bg (1)
    tmp <- dt[, (var) := substr(blockcode, 1, gid)
              ## drop blockcode column
              ][, -c("blockcode")]

    ## broadband measures by geolevel and tech code
    ## - min, max, and median of maximum advertised download/upload speeds
    dt_meas <- tmp[,
                   as.list(
                       unlist(
                           lapply(.SD,
                                  function(x) {
                                      list(min = min(x, na.rm = TRUE),
                                           max = max(x, na.rm = TRUE),
                                           med = median(x, na.rm = TRUE),
                                           mean = mean(x, na.rm = TRUE))}
                                  )
                       )
                   ),
                   by = c(var, "techcode"),
                   .SDcols = c("maxaddown", "maxadup")]

    ## provider options 1: by census block and tech code
    dt_opt1 <- tmp[,
                   .(prov_tech_count = uniqueN(provider_id)),
                   by = c(var, "techcode")]

    ## provider options 2: by census block
    dt_opt2 <- tmp[,
                   .(prov_area_count = uniqueN(provider_id)),
                   by = c(var)]

    ## join together
    tmp <- dt_opt1[dt_meas,
                   on = c(var, "techcode")
                   ][dt_opt2,
                     on = c(var)
                     ][,
                       ## add year/month columns for later append
                       c("year", "month") := list(rep(f_year, nrow(dt_opt1)),
                                                  rep(f_month, nrow(dt_opt1)))]

    ## replace . with _ in column names
    names(tmp) <- str_replace(names(tmp), "\\.", "_")

    ## new column order
    setcolorder(tmp, c(var,
                       "year",
                       "month",
                       "techcode",
                       "prov_tech_count",
                       "prov_area_count",
                       "maxaddown_min",
                       "maxaddown_med",
                       "maxaddown_mean",
                       "maxaddown_max",
                       "maxadup_min",
                       "maxadup_med",
                       "maxadup_mean",
                       "maxadup_max"))

    ## save and remove temporary file
    dt_bg_list[[i]] <- tmp
    rm(tmp)
}

## bind
dt_bg <- rbindlist(dt_bg_list)

## arrange by tract/bg --> year --> month (descending) --> techcode
dt_bg[order(bg, year, -month, techcode)]

## write to disk
saveRDS(dt_bg, file.path(cln_dir, "broadband_bg.RDS"))

## remove object
rm(list = c("dt", "dt_bg", "dt_bg_list", "dt_meas", "dt_opt1", "dt_opt2"))
gc()

## -----------------------------------------------------------------------------
## END SCRIPT
################################################################################
