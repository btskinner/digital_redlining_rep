################################################################################
##
## [ PROJ ] Digital redlining / HOLC
## [ FILE ] macros.R
## [ AUTH ] Skinner, Levy, Burtch (@btskinner)
## [ INIT ] 13 August 2020
##
################################################################################

## -----------------------------------------------------------------------------
## ACS percentages
## -----------------------------------------------------------------------------

## Each list item is used to compute a percentage from the ACS household data
## on broadband/computer usage. Each is structured:
##
## list(< denominator > = "< numerator >", ...)
##
## The means that the first returned value is computed using v8/v7
## columns from the ACS files

## 2015_2019: seq135
nd_1519_s135 <- list(v7 = "v8",         # devices (any)
                     v7 = "v9",         # ...desktop/laptop
                     v7 = "v12",        # ...smartphone only
                     v7 = "v17",        # ...no devices
                     v18 = "v19",       # internet subscription
                     v18 = "v20",       # ...dial up only
                     v18 = "v21",       # ...broadband
                     v18 = "v23",       # ...cell service only
                     v18 = "v30",       # ...no subscription
                     v31 = "v32",       # devices by internet subscription (any)
                     v31 = "v33",       # ...dial up only
                     v31 = "v34",       # ...broadband
                     v31 = "v35",       # ...no internet (but device)
                     v31 = "v36",       # ...no devices
                     v38 = "v40",       # income bands w/ broadband: <$10k
                     v42 = "v44",       # ...[10k, 20k)
                     v46 = "v48",       # ...[20k, 35k)
                     v50 = "v52",       # ...[35k, 50k)
                     v54 = "v56",       # ...[50k, 75k)
                     v58 = "v60",       # ...75k+
                     v63 = "v64",       # computer: <18 yo
                     v69 = "v70",       # ...18-64 yo
                     v75 = "v76",       # ...65+ yo
                     v64 = "v66",       # broadband | computer: <18 yo
                     v70 = "v72",       # ...18-64 yo
                     v76 = "v78",       # ...65+ yo
                     v82 = "v83",       # computer: less than HS (25+)
                     v88 = "v89",       # ...some college
                     v94 = "v95",       # ...bachelor's +
                     v83 = "v85",       # broadband | computer: less than HS (25+)
                     v89 = "v91",       # ...some college
                     v95 = "v97",       # ...bachelor's +
                     v102 = "v103",     # computer: employed
                     v108 = "v109",     # ...unemployed
                     v114 = "v115",     # ...not in labor force
                     v103 = "v105",     # broadband | computer: employed
                     v109 = "v111",     # ...unemployed
                     v115 = "v117",     # ...not in labor force
                     v130 = "v131",     # computer: white
                     v136 = "v137",     # ...black
                     v142 = "v143",     # ...amerind/alnat
                     v148 = "v149",     # ...asian
                     v154 = "v155",     # ...nhpi
                     v160 = "v161",     # ...other race alone
                     v166 = "v167",     # ...two or more races
                     v172 = "v173",     # ...white, non-hispanic
                     v178 = "v179",     # ...hispanic/latino
                     v131 = "v133",     # broadband | computer: white
                     v137 = "v139",     # ...black
                     v143 = "v145",     # ...amerind/alnat
                     v149 = "v151",     # ...asian
                     v155 = "v157",     # ...nhpi
                     v161 = "v163",     # ...other race alone
                     v167 = "v169",     # ...two or more races
                     v173 = "v175",     # ...white, non-hispanic
                     v179 = "v181")     # ...hispanic/latino

## acs groups
acs_groups <- list(
    ## income bands: broadband access
    "in_bbd" = paste0("s136_v", c(40,44,48,52,56,60), "_pct"),
    ## education: broadband by computer ownership
    "ed_cbb" = paste0("s136_v", c(85,91,97), "_pct"),
    ## employment: broadband by computer ownership
    "em_cbb" = paste0("s136_v", c(105,111,117), "_pct"),
    ## race/ethnicity: computer ownership
    "re_com" = paste0("s136_v", c(137,143,149,155,161,167,173,179), "_pct"),
    ## race/ethnicity: broadband by computer ownership
    "re_cbb" = paste0("s136_v", c(139,145,151,157,163,169,175,181), "_pct")
)

## shorter version of above
acs_groups_short <- list(
    ## income bands: broadband access
    "in_bbd" = paste0("v", c(40,44,48,52,56,60)),
    ## education: broadband by computer ownership
    "ed_cbb" = paste0("v", c(85,91,97)),
    ## employment: broadband by computer ownership
    "em_cbb" = paste0("v", c(105,111,117)),
    ## race/ethnicity: computer ownership
    "re_com" = paste0("v", c(137,143,149,155,161,167,173,179)),
    ## race/ethnicity: broadband by computer ownership
    "re_cbb" = paste0("v", c(139,145,151,157,163,169,175,181))
)

## convert ACS values to labels for graphing
acs_labs <- function(vars) {
    out_list <- list()
    for (i in seq_along(vars)) {
        out_list[[vars[i]]] <- case_when(
            grepl("v34", vars[[i]]) ~ "Overall",
            grepl("v40", vars[[i]]) ~ "<$10k",
            grepl("v44", vars[[i]]) ~ "[10k, 20k)",
            grepl("v48", vars[[i]]) ~ "[20k, 35k)",
            grepl("v52", vars[[i]]) ~ "[35k, 50k)",
            grepl("v56", vars[[i]]) ~ "[50k, 75k)",
            grepl("v60", vars[[i]]) ~ "75k+",
            grepl("v85", vars[[i]]) ~ "Less than HS",
            grepl("v91", vars[[i]]) ~ "Some college",
            grepl("v97", vars[[i]]) ~ "Bachelor's +",
            grepl("v105", vars[[i]]) ~ "Employed",
            grepl("v111", vars[[i]]) ~ "Unemployed",
            grepl("v117", vars[[i]]) ~ "Not in labor force",
            grepl("v137|v139", vars[[i]]) ~ "Black",
            grepl("v143|v145", vars[[i]]) ~ "AI/AK",
            grepl("v149|v151", vars[[i]]) ~ "Asian",
            grepl("v155|v157", vars[[i]]) ~ "NHPI",
            grepl("v161|v163", vars[[i]]) ~ "Other race",
            grepl("v167|v169", vars[[i]]) ~ "Multiple races",
            grepl("v173|v175", vars[[i]]) ~ "White",
            grepl("v179|v181", vars[[i]]) ~ "Hispanic",
            TRUE ~ NA_character_
        )
    }
    out_list
}

## convert ACS values to labels for graphing
acs_group_labs <- function(x, capitalize = FALSE) {
    case_when(
        x == "in_bbd" ~ ifelse(capitalize, "Income", "income"),
        x == "ed_cbb" ~ ifelse(capitalize, "Education", "education"),
        x == "em_cbb" ~ ifelse(capitalize, "Employment", "employment"),
        x == "re_com" ~ ifelse(capitalize, "Race/ethnicity", "race/ethnicity"),
        x == "re_cbb" ~ ifelse(capitalize, "Race/ethnicity", "race/ethnicity"),
        TRUE ~ NA_character_
    )
}

## plot list: variable number as the name: list(var = "Title of plot")
acs_plot_title_list <- list(
    v8 = "Households with one or more types of computing device",
    v9 = "Households with a desktop or laptop computer",
    v12 = "Households with smart phones as only computing device",
    v17 = "Households with no computing device",
    v19 = "Households with an internet connection",
    v20 = "Households with only a dial up connection",
    v21 = "Households with any type of broadband connection",
    v23 = "Households with cellular data plan only",
    v30 = "Households with no internect connection",
    v32 = "Households with a computer and any connection",
    v33 = "Households with a computer but only a dial up connection",
    v34 = "Households with a computer and broadband connection",
    v35 = "Households with a computer but no internet connection",
    v36 = "Households with no computing devices",
    v40 = "Households making < $10k that have a broadband connection",
    v44 = "Households making $10k-$19,999 that have a broadband connection",
    v48 = "Households making $20k-$34,999 that have a broadband connection",
    v52 = "Households making $35k-$49,999 that have a broadband connection",
    v56 = "Households making $50k-$74,999 that have a broadband connection",
    v60 = "Households making $75k+ that have a broadband connection",
    v64 = "Households with a computer:<18 years old",
    v66 = "Households with a computer and broadband connection:<18 years old",
    v70 = "Households with a computer:18-64 years old",
    v72 = "Households with a computer and broadband connection:18-64 years old",
    v76 = "Households with a computer:65+ years old",
    v78 = "Households with a computer and broadband connection:65+ years old",
    v83 = "Households with a computer:Less than high school",
    v85 = "Households with a computer and broadband connection:Less than high school",
    v89 = "Households with a computer:Some college",
    v91 = "Households with a computer and broadband connection:Some college",
    v95 = "Households with a computer:Bachelors+",
    v97 = "Households with a computer and broadband connection:Bachelors+",
    v103 = "Households with a computer:Employed",
    v105 = "Households with a computer and broadband connection:Employed",
    v109 = "Households with a computer:Unemployed",
    v111 = "Households with a computer and broadband connection:Unemployed",
    v115 = "Households with a computer:Not in labor force",
    v117 = "Households with a computer and broadband connection:Not in labor force",
    v131 = "Households with a computer:White (including Hispanic)",
    v133 = "Households with a computer and broadband connection:White (including Hispanic)",
    v137 = "Households with a computer:Black",
    v139 = "Households with a computer and broadband connection:Black",
    v143 = "Households with a computer:American Indian/Alaskan Native",
    v145 = "Households with a computer and broadband connection:American Indian/Alaskan Native",
    v149 = "Households with a computer:Asian",
    v151 = "Households with a computer and broadband connection:Asian",
    v155 = "Households with a computer:Native Hawaiian/Pacific Islander",
    v157 = "Households with a computer and broadband connection:Native Hawaiian/Pacific Islander",
    v161 = "Households with a computer:Other race",
    v163 = "Households with a computer and broadband connection:Other race",
    v167 = "Households with a computer:Two or more races",
    v169 = "Households with a computer and broadband connection:Two or more races",
    v173 = "Households with a computer:White",
    v175 = "Households with a computer and broadband connection:White",
    v179 = "Households with a computer:Hispanic",
    v181 = "Households with a computer and broadband connection:Hispanic"
)

## -----------------------------------------------------------------------------
## Broadband (FCC 477)
## -----------------------------------------------------------------------------

## technology list
techlist <- list(adsl = 10,
                 adsl2 = 11,
                 vdsl = 12,
                 xdsl = 20,
                 copperother = 30,
                 modemother = 40,
                 docsis20 = 41,         # NB: this is DOCSIS 2.0 or below
                 docsis30 = 42,
                 docsis31 = 43,
                 fiber = 50,
                 sat = 60,
                 terwire = 70,
                 powerline = 90,
                 allother = 0)

## technology list
tech_labs <- function(x) {
    case_when(
        str_detect(x, "adsl_") ~ "Asymmetric DSL",
        str_detect(x, "adsl2_") ~ "ADSL2/ADSL2+",
        str_detect(x, "vdsl") ~ "VDSL",
        str_detect(x, "xdsl") ~ "Symmetric DSL",
        str_detect(x, "copperother") ~ "Other copper wireline",
        str_detect(x, "modemother") ~ "Other cable modem",
        str_detect(x, "docsis20") ~ "Cable modem: DOCSIS 1, 1.1, 2.0",
        str_detect(x, "docsis30") ~ "Cable modem: DOCSIS 3.0",
        str_detect(x, "docsis31") ~ "Cable modem: DOCSIS 3.1",
        str_detect(x, "fiber") ~ "Optical/Fiber",
        str_detect(x, "sat") ~ "Satellite",
        str_detect(x, "terwire") ~ "Terrestial fixed wireless",
        str_detect(x, "powerline") ~ "Electric powerline",
        str_detect(x, "allother") ~ "Other",
        TRUE ~ NA_character_)
}

tech_labs_num <- function(x) {
    case_when(
        x == 10 ~ "Asymmetric DSL",
        x == 11 ~ "ADSL2/ADSL2+",
        x == 12 ~ "VDSL",
        x == 20 ~ "Symmetric DSL",
        x == 30 ~ "Other copper wireline",
        x == 40 ~ "Other cable modem",
        x == 41 ~ "Cable modem: DOCSIS 1, 1.1, 2.0",
        x == 42 ~ "Cable modem: DOCSIS 3.0",
        x == 43 ~ "Cable modem: DOCSIS 3.1",
        x == 50 ~ "Optical/Fiber",
        x == 60 ~ "Satellite",
        x == 70 ~ "Terrestial fixed wireless",
        x == 90 ~ "Electric powerline",
        x == 0 ~ "Other",
        TRUE ~ NA_character_)
}

fcc_labs <- function(x) {
    case_when(
        str_detect(x, "prov_tech_count_") ~ "Number of providers within technology",
        str_detect(x, "maxaddown_min_") ~ "Maximum download speed (minimum)",
        str_detect(x, "maxaddown_max_") ~ "Maximum download speed (maximum)",
        str_detect(x, "maxaddown_med_") ~ "Maximum download speed (median)",
        str_detect(x, "maxaddown_mean_") ~ "Maximum download speed (mean)",
        str_detect(x, "maxadup_min_") ~ "Maximum upload speed (minimum)",
        str_detect(x, "maxadup_max_") ~ "Maximum upload speed (maximum)",
        str_detect(x, "maxadup_med_") ~ "Maximum upload speed (median)",
        str_detect(x, "maxadup_mean_") ~ "Maximum upload speed (mean)",
        TRUE ~ NA_character_)
}


## -----------------------------------------------------------------------------
## HOLC
## -----------------------------------------------------------------------------

## holc area color values (to match those used on original maps)
holc_vals <- c("A" = rgb(126, 171, 119, 255, maxColorValue = 255),
               "B" = rgb(146, 202, 222, 255, maxColorValue = 255),
               "C" = rgb(255, 219, 88, 255, maxColorValue = 255),
               "D" = rgb(211, 146, 146, 255, maxColorValue = 255))

## -----------------------------------------------------------------------------
## geography
## -----------------------------------------------------------------------------

## common crs (NAD83 / Conus Albers)
common_crs <- 5070

## -----------------------------------------------------------------------------
## END SCRIPT
################################################################################
