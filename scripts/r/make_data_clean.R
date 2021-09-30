################################################################################
##
## [ PROJ ] Digital redlining / HOLC
## [ FILE ] make_data_clean.R
## [ AUTH ] Skinner, Levy, Burtch (@btskinner)
## [ INIT ] 21 August 2020
##
################################################################################

## libraries
libs <- c("data.table", "tidyverse", "readxl", "sf", "crosswalkr")
sapply(libs, require, character.only = TRUE)

## directory paths
args <- commandArgs(trailingOnly = TRUE)
root <- ifelse(length(args) == 0, file.path("..", ".."), args)
dat_dir <- file.path(root, "data")
acs_dir <- file.path(dat_dir, "acs")
bbd_dir <- file.path(dat_dir, "broadband")
cln_dir <- file.path(dat_dir, "clean")
hol_dir <- file.path(dat_dir, "holc")
tig_dir <- file.path(dat_dir, "tiger")
scr_dir <- file.path(root, "scripts", "r")

## external functions
source(file.path(scr_dir, "functions.R"))

## external macros
source(file.path(scr_dir, "macros.R"))

## data.table options
options(datatable.integer64 = "character")

## which ACS data to use
acs_dat <- "acs_2015_2019_seq_135_sumlvl_140_us.RDS"

## -----------------------------------------------------------------------------
## COMBINE DATA
## -----------------------------------------------------------------------------

## -------------------------------------
## broadband access
## -------------------------------------

## Compute broadband access by tech code within blockgroup

## read in broadband data
bb <- readRDS(file.path(cln_dir, paste0("broadband_bg.RDS")))

## expand
bb_expand <- bb %>%
    expand(bg, year, month) %>%
    filter(!(year == 2014 & month == "Jun"),
           !(year == 2019 & month == "Dec"))

## compute access
## 0 := no access at all in block group
## 1 := at least one provider of technology in blockgroup
bb_access <- map2(techlist,
                  names(techlist),
                  ~ bb_expand %>%
                      left_join(bb %>%
                                filter(techcode %in% .x) %>%
                                mutate(access = 1) %>%
                                select(bg, year, month, access),
                                by = c("bg", "year", "month")) %>%
                      mutate(access = ifelse(is.na(access), 0, access)) %>%
                      rename(!!sym(paste0(.y, "_access")) := access)) %>%
    reduce(left_join, by = c("bg", "year", "month")) %>%
    filter(as.numeric(str_sub(bg, 1, 2)) < 60) %>%
    mutate(stfips = as.integer(str_sub(bg, 1, 2))) %>%
    left_join(crosswalkr::stcrosswalk, by = "stfips")

## save
saveRDS(bb_access, file.path(cln_dir, "bb_access_bg.RDS"))

## -----------------------------------------------------------------------------
## GEOGRAPHY (SHAPEFILES)
## -----------------------------------------------------------------------------

## -------------------------------------
## HOLC
## -------------------------------------

## Clean up HOLC shapefiles

## holc files
df_holc <- st_read(file.path(hol_dir, "holc_ad_data.shp")) %>%
    ## ...drop area descriptions since they have lots of text
    select(-area_descr) %>%
    ## ...project to common CRS
    st_transform(st_crs(common_crs)) %>%
    ## ...set edge buffer to 0
    st_buffer(dist = 0) %>%
    ## ...add unique id since some polygon ids are missing
    mutate(id = row_number()) %>%
    ## ...compute area for each polygon
    mutate(holc_area = as.numeric(st_area(geometry)))

## save
saveRDS(df_holc, file.path(cln_dir, "sf_holc.RDS"))

## -------------------------------------
## TIGER
## -------------------------------------

## Combine state TIGER shapefiles (tract and blockgroup) into single
## national file

## init length (2) list vector for tract and blockgroup data
shp_list <- list("tr" = NA, "bg" = NA)

## loop through each census level
for (i in c("tr", "bg")) {
    ## get subdirectories (one for each state)
    stdirs <- list.dirs(tig_dir, full.names = FALSE, recursive = FALSE)
    ## pattern
    pattern <- ifelse(i == "tr", "_tract$", "_bg$")
    ## i == "tr"? "_tract" : "_bg"
    stdirs <- grep(pattern, stdirs, value = TRUE)
    ## init empty list
    file_list <- vector("list", length = length(stdirs))
    ## loop through subdirectories
    for (j in seq_along(stdirs)) {
        ## get state-specific subdirectory
        stdir <- stdirs[j]
        ## file name is just subdirectory name + ".shp"
        file <- paste0(stdir, ".shp")
        ## read in shapefile
        file_list[[j]] <- st_read(file.path(tig_dir, stdir, file)) %>%
            ## ...project to common CRS
            st_transform(st_crs(common_crs)) %>%
            ## ...lower all column names
            rename_all(tolower) %>%
            ## ...create area measure
            mutate(area = as.numeric(st_area(geometry))) %>%
            select(geoid, area, geometry)
    }
    ## bind state-specific files into one w/zero buffer
    shp_list[[i]] <- do.call(rbind, file_list) %>% st_buffer(dist = 0)
}

## save
saveRDS(shp_list[["bg"]], file.path(cln_dir, "sf_bg_us.RDS"))
saveRDS(shp_list[["tr"]], file.path(cln_dir, "sf_tr_us.RDS"))

## -------------------------------------
## intersections: blocks over HOLC
## -------------------------------------

## intersection of blockgroups with HOLC maps
df_int_bg <- st_intersection(df_holc, shp_list[["bg"]]) %>%
    ## ...compute intersection areas
    mutate(iarea = as.numeric(st_area(geometry))) %>%
    ## - weight is the proportion of HOLC zone overlapped
    ## ...group by unique id
    group_by(id) %>%
    ## ...area weights
    mutate(area_w = log_divide(iarea, holc_area)) %>%
    ## ...ungroup
    ungroup() %>%
    ## ...left join broadband speeds
    left_join(readRDS(file.path(cln_dir, "broadband_bg.RDS")),
              by = c("geoid" = "bg"))

## save
saveRDS(df_int_bg, file.path(cln_dir, "sf_holc_bg.RDS"))

## -------------------------------------
## intersections: HOLC over tracts
## -------------------------------------

df_int_tr <- st_intersection(shp_list[["tr"]], df_holc) %>%
    ## ...compute intersection areas
    mutate(iarea = as.numeric(st_area(geometry))) %>%
    ## - weight is the proportion of census tract overlapped
    ## ...group by unique id
    group_by(geoid) %>%
    ## ...area weights
    mutate(area_w = log_divide(iarea, holc_area)) %>%
    ## ...ungroup
    ungroup() %>%
    ## ...left join ACS measures
    left_join(readRDS(file.path(cln_dir, acs_dat)) %>%
              as_tibble,
              by = c("geoid" = "fips"))

## save
saveRDS(df_int_tr, file.path(cln_dir, "sf_tr_holc.RDS"))

## -----------------------------------------------------------------------------
## ANALYSIS
## -----------------------------------------------------------------------------

## ---------------------------
## ACS
## ---------------------------

## setting up data
df <- df_int_tr %>%
    ## ...dropping geometry b/c we don't need it at the moment
    st_drop_geometry() %>%
    ## add in crosswalk variables
    mutate(stfips = as.integer(str_sub(geoid, 1, 2))) %>%
    ## join stcrosswalk variables
    left_join(crosswalkr::stcrosswalk, by = "stfips") %>%
    ## arrange columns
    relocate(stfips:cendivnm, geoid)

## get count column names
count_vars <- grep("^v[0-9]+$", names(df), value = TRUE)

## get ACS values within HOLC zones
holc_acs <- map(count_vars,
                ~ df %>%
                    mutate(!!paste0(.x, "_wc") := area_w * !!sym(.x)) %>%
                    select(geoid, id, !!paste0(.x, "_wc"))) %>%
    reduce(left_join, by = c("geoid", "id")) %>%
    group_by(id) %>%
    summarise(across(ends_with("_wc"),
                     ~ sum(.x) %>% ceiling)) %>%
    left_join(df %>%
              distinct(id, .keep_all = TRUE) %>%
              select(c(id, city:holc_area, stfips:cendivnm)),
              by = "id") %>%
    relocate(c(id, city:holc_area, stfips:cendivnm))

## save
saveRDS(holc_acs, file.path(cln_dir, "holc_acs_analysis.RDS"))

## ---------------------------
## blockgroup-level
## ---------------------------

## setting up data
df <- df_int_bg %>%
    ## ...dropping geometry b/c we don't need it at the moment
    st_drop_geometry() %>%
    ## ...left join access measures
    left_join(bb_access,
              by = c("geoid" = "bg", "year", "month"))

## get *_access column names
acc_vars <- grep(".+_access", names(df), value = TRUE)

## get within group summaries (within city)
holc_fcc <- map(acc_vars,
                ~ df %>%
                    ## convert 0/1 to 0/100 for prop --> pct
                    mutate(!!sym(.x) := !!sym(.x) * 100) %>%
                    group_by(year, month,
                             city, stabbr, stname, cenreg,
                             cenregnm,
                             holc_grade) %>%
                    summarise(acc = weighted.mean(!!sym(.x),
                                                  area_w,
                                                  na.rm = TRUE),
                              .groups = "drop") %>%
                    mutate(tech = .x)) %>%
    bind_rows %>%
    arrange(year, desc(month), tech, city, holc_grade) %>%
    select(city, stabbr, stname, cenreg, cenregnm,
           holc_grade, year, month, tech, acc)

## save
saveRDS(holc_fcc, file.path(cln_dir, "holc_fcc_analysis.RDS"))

## -----------------------------------------------------------------------------
## END SCRIPT
################################################################################
