################################################################################
##
## [ PROJ ] Digital redlining / HOLC
## [ FILE ] make_tables.R
## [ AUTH ] Skinner, Levy, Burtch (@btskinner)
## [ INIT ] 29 September 2020
##
################################################################################

## libraries
libs <- c("tidyverse", "xtable")
sapply(libs, require, character.only = TRUE)

## directory paths
args <- commandArgs(trailingOnly = TRUE)
root <- ifelse(length(args) == 0, file.path("..", ".."), args)
dat_dir <- file.path(root, "data")
cln_dir <- file.path(dat_dir, "clean")
tab_dir <- file.path(root, "tables")
scr_dir <- file.path(root, "scripts", "r")

## external functions
source(file.path(scr_dir, "functions.R"))

## macros
source(file.path(scr_dir, "macros.R"))

## -----------------------------------------------------------------------------
## read in data
## -----------------------------------------------------------------------------

## analysis file
holc <- readRDS(file.path(cln_dir, "holc_acs_analysis.RDS"))

## predictions
preds_acs <- readRDS(file.path(cln_dir, "predictions_acs.RDS"))
preds_fcc <- readRDS(file.path(cln_dir, "predictions_fcc_beta.RDS"))

## -----------------------------------------------------------------------------
## descriptive statistics
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

df <- holc %>%
    select(holc_grade,
           paste0("v",
                  c("32","34","38","40","42","44","46","48","50","52","54",
                    "56","58","60","137","139","143","145","149","151","155",
                    "157","161","163","167","169","173","175","179","181"),
                  "_wc")) %>%
    group_by(holc_grade) %>%
    summarise(across(, ~ sum(.x))) %>%
    filter(holc_grade != "E") %>%
    pivot_longer(cols = starts_with("v"),
                 names_to = "var",
                 values_to = "val") %>%
    mutate(var = case_when(
               str_detect(var, "32") ~ "Overall_total",
               str_detect(var, "34") ~ "Overall_bb",
               str_detect(var, "38") ~ "<$10k_total",
               str_detect(var, "40") ~ "<$10k_bb",
               str_detect(var, "42") ~ "[$10k, $20k)_total",
               str_detect(var, "44") ~ "[$10k, $20k)_bb",
               str_detect(var, "46") ~ "[$20k, $35k)_total",
               str_detect(var, "48") ~ "[$20k, $35k)_bb",
               str_detect(var, "50") ~ "[$35k, $50k)_total",
               str_detect(var, "52") ~ "[$35k, $50k)_bb",
               str_detect(var, "54") ~ "[$50k, $75k)_total",
               str_detect(var, "56") ~ "[$50k, $75k)_bb",
               str_detect(var, "58") ~ "$75k+_total",
               str_detect(var, "60") ~ "$75k+_bb",
               str_detect(var, "137") ~ "Black_total",
               str_detect(var, "139") ~ "Black_bb",
               str_detect(var, "143") ~ "AI/AK_total",
               str_detect(var, "145") ~ "AI/AK_bb",
               str_detect(var, "149") ~ "Asian_total",
               str_detect(var, "151") ~ "Asian_bb",
               str_detect(var, "155") ~ "NH/PI_total",
               str_detect(var, "157") ~ "NH/PI_bb",
               str_detect(var, "161") ~ "Other race_total",
               str_detect(var, "163") ~ "Other race_bb",
               str_detect(var, "167") ~ "Multiple races_total",
               str_detect(var, "169") ~ "Multiple races_bb",
               str_detect(var, "173") ~ "White_total",
               str_detect(var, "175") ~ "White_bb",
               str_detect(var, "179") ~ "Hispanic_total",
               str_detect(var, "181") ~ "Hispanic_bb"
           )) %>%
    separate(col = var,
             into = c("group", "measure"),
             sep = "_") %>%
    pivot_wider(id_cols = c("holc_grade", "group"),
                names_from = "measure",
                values_from = "val") %>%
    mutate(frac = paste0(bb, "/", total),
           pct = round(bb / total * 100,2)) %>%
    select(holc_grade, group, frac, pct, bb, total) %>%
    arrange(holc_grade, group)

## -------------------------------------
## overall
## -------------------------------------

df_ov <- df %>% filter(str_detect(group, "Overall", negate = FALSE))

ov_mat <- cbind(cbind(df_ov %>% filter(holc_grade == "A") %>% select(frac, pct)),
                cbind(df_ov %>% filter(holc_grade == "B") %>% select(frac, pct)),
                cbind(df_ov %>% filter(holc_grade == "C") %>% select(frac, pct)),
                cbind(df_ov %>% filter(holc_grade == "D") %>% select(frac, pct)))

## -------------------------------------
## r/e
## -------------------------------------

## subset
df_re <- df %>% filter(str_detect(group, "\\$|Overall", negate = TRUE))

## set up matrix
re_mat <- matrix(NA_character_, nrow = 8, ncol = 4 * 2 + 1)

## get names
re_mat[,1] <- df_re %>% distinct(group) %>% pull

## A
re_mat[,2:3] <- df_re %>%
    filter(holc_grade == "A") %>%
    select(frac, pct) %>%
    as.matrix
## B
re_mat[,4:5] <- df_re %>%
    filter(holc_grade == "B") %>%
    select(frac, pct) %>%
    as.matrix
## C
re_mat[,6:7] <- df_re %>%
    filter(holc_grade == "C") %>%
    select(frac, pct) %>%
    as.matrix
## D
re_mat[,8:9] <- df_re %>%
    filter(holc_grade == "D") %>%
    select(frac, pct) %>%
    as.matrix

## -------------------------------------
## income
## -------------------------------------

## subset
df_in <- df %>%
    filter(str_detect(group, "\\$", negate = FALSE)) %>%
    mutate(group = factor(group,
                          levels = c("<$10k",
                                     "[$10k, $20k)",
                                     "[$20k, $35k)",
                                     "[$35k, $50k)",
                                     "[$50k, $75k)",
                                     "$75k+"))) %>%
    arrange(holc_grade, group)

## set up matrix
in_mat <- matrix(NA_character_, nrow = 6, ncol = 4 * 2 + 1)

## get names
in_mat[,1] <- df_in %>% distinct(group) %>% pull %>% levels

## A
in_mat[,2:3] <- df_in %>%
    filter(holc_grade == "A") %>%
    select(frac, pct) %>%
    as.matrix
## B
in_mat[,4:5] <- df_in %>%
    filter(holc_grade == "B") %>%
    select(frac, pct) %>%
    as.matrix
## C
in_mat[,6:7] <- df_in %>%
    filter(holc_grade == "C") %>%
    select(frac, pct) %>%
    as.matrix
## D
in_mat[,8:9] <- df_in %>%
    filter(holc_grade == "D") %>%
    select(frac, pct) %>%
    as.matrix

## -------------------------------------
## regions and counts
## -------------------------------------

reg <- holc %>%
    filter(holc_grade != "E") %>%
    count(holc_grade, cenregnm)

tot <- reg %>%
    group_by(holc_grade) %>%
    summarise(n = sum(n)) %>%
    mutate(cenregnm = "$N$")

reg <- bind_rows(reg, tot)

## set up matrix
rc_mat <- matrix(NA_character_, nrow = 5, ncol = 4 * 2 + 1)

## get names
rc_mat[,1] <- reg %>% distinct(cenregnm) %>% pull

## A
rc_mat[,2] <- reg %>%
    filter(holc_grade == "A") %>%
    select(n) %>%
    as.matrix
## B
rc_mat[,4] <- reg %>%
    filter(holc_grade == "B") %>%
    select(n) %>%
    as.matrix
## C
rc_mat[,6] <- reg %>%
    filter(holc_grade == "C") %>%
    select(n) %>%
    as.matrix
## D
rc_mat[,8] <- reg %>%
    filter(holc_grade == "D") %>%
    select(n) %>%
    as.matrix
## replace NA with ""
rc_mat[is.na(rc_mat)] <- ""

## -------------------------------------
## bind everything and output
## -------------------------------------

mat <- rbind(c("Overall", ov_mat),
             c("{\\itshape Race/ethnicity}", rep("", 8)),
             re_mat,
             c("{\\itshape Income}", rep("",8)),
             in_mat,
             c("{\\itshape Region}", rep("",8)),
             rc_mat)

## bold first row
mat[1,] <- gsub("(.+)", "{\\\\bfseries \\1}", mat[1,])

## add horizontal spaces
mat[c(3:10,12:17,19:22),1] <- paste0("\\hspace{1em}",
                                    mat[c(3:10,12:17,19:22),1])

## escape characters
mat[12:17,1] <- str_replace_all(mat[12:17,1], "\\$", "\\\\$")

## notes
notes <- c("Data for this table come from the ACS 2015-2019 summary files. ",
           "Numbers in the top panel represent estimates of the number and ",
           "percentage of persons in ",
           "households with access to a computer and broadband by HOLC ",
           "neighborhood zone. The bottom panel gives the counts of unique ",
           "neighborhoods in the sample, within census region and overall. ",
           "{\\itshape AI/AK}: American Indian / Alaskan Native; ",
           "{\\itshape NH/PI}: Native Hawaiian / Pacific Islander.")

## header
header <- c("\\begin{table}[!ht]",
            "\\scriptsize",
            "\\centering",
            "\\caption{Descriptive statistics of sample}",
            "\\label{tbl:desc}",
            "\\begin{tabularx}{\\linewidth}{Xrcrcrcrc}",
            "\\toprule",
            paste0("&\\multicolumn{2}{c}{A}&\\multicolumn{2}{c}{B}",
                   "&\\multicolumn{2}{c}{C}&\\multicolumn{2}{c}{D}",
                   "\\\\"),
            "\\cmidrule(lr){2-3}\\cmidrule(lr){4-5}",
            "\\cmidrule(lr){6-7}","\\cmidrule(lr){8-9}",
            paste0("&",
                   paste(rep(c("Broadband/Total", "\\%"), 4), collapse = "&"),
                   "\\\\"))

## primary contents
contents <- print(xtable(mat),
                  booktabs = TRUE,
                  sanitize.text.function = function(x){x},
                  include.colnames = FALSE,
                  include.rownames = FALSE,
                  only.contents = TRUE,
                  print.results = FALSE,
                  hline.after = c(0, nrow(mat) - 6,  nrow(mat) - 1, nrow(mat)),
                  comment = FALSE)
## footer
footer <- c(paste0("\\multicolumn{9}{p{.98\\linewidth}}"),
            "{\\scriptsize{\\itshape Notes.} ",
            notes,
            "}",
            "\\end{tabularx}",
            "\\end{table}")

## save
writeLines(c(header, contents, footer),
           con = file.path(tab_dir, paste0("desc.tex")))

## -----------------------------------------------------------------------------
## model outcomes
## -----------------------------------------------------------------------------

## -------------------------------------
## ACS
## -------------------------------------

## compute median and 95% CIs
df_all <- preds_acs %>%
    group_by(group_f, var) %>%
    summarise(v = quantile(val, c(0.025, 0.5, 0.975)),
              q = c(0.025, 0.5, 0.975),
              .groups = "drop") %>%
    mutate(v = v * 100) %>%
    pivot_wider(names_from = "var",
                values_from = "v")

## 1: race/ethnicity
## 2: income
for (i in 1:2) {
    ## i == 1? drop group_f w/ "$" (income) : keep only income group_f
    negate_bool <- ifelse(i == 1, TRUE, FALSE)
    ## subset data for this table
    df <- df_all %>% filter(str_detect(group_f, "\\$", negate = negate_bool))
    if (i == 2) {
        df <- df %>%
            mutate(group_f = factor(group_f,
                                    levels = c("<$10k",
                                               "[$10k, $20k)",
                                               "[$20k, $35k)",
                                               "[$35k, $50k)",
                                               "[$50k, $75k)",
                                               "$75k+"))) %>%
            arrange(group_f, q)
    }
    ## get unique groups
    gn <- df %>% distinct(group_f) %>% nrow
    ## init table matrix
    mat <- matrix(NA_character_,
                  nrow = 4 * 2,
                  ncol = df %>% filter(q == .5) %>% nrow + 1)
    ## set up odd/even masks
    odd <- seq(1, nrow(mat), 2)
    evn <- seq(2, nrow(mat), 2)
    ## add main estimates (median)
    mat[odd,2:(gn + 1)] <- df %>%
        filter(q == .5) %>%
        select(starts_with("b")) %>%
        as.matrix %>%
        round(., digits = 1) %>%
        t()
    ## add 95% CI below in square brackets: [2.5%, 97.5%]
    mat[evn,2:(gn + 1)] <- paste0("[",
                           df %>%
                           filter(q == .025) %>%
                           select(starts_with("b")) %>%
                           as.matrix %>%
                           round(., digits = 1),
                           ", ",
                           df %>%
                           filter(q == .975) %>%
                           select(starts_with("b")) %>%
                           as.matrix %>%
                           round(., digits = 1),
                           "]") %>%
        matrix(ncol = 4) %>%
        t()
    ## add names column
    mat[odd,1] <- paste0("\\hspace{1em}", LETTERS[1:4])
    mat[evn,1] <- ""
    ## add header row
    mat <- rbind(c("{\\itshape HOLC zone}", rep("", gn)), mat)
    ## table notes
    notes <- c("Each column presents results from a separate regression model. ",
               "The median value of the full posterior distribution is presented, ",
               "with 95\\% credible intervals in square brackets below. Percentage ",
               "estimates were computed for each HOLC zone parameter ",
               "($\\beta^{zone}_{A,B,C,D}$) using average parameter values across ",
               "region, state, and city.")
    ## add extra note for race/ethnicity table
    if (i == 1) {
        notes <- c(notes,
                   " {\\itshape AI/AK}: American Indian / Alaskan Native; ",
                   "{\\itshape NH/PI}: Native Hawaiian / Pacific Islander.")
    }
    ## header
    byg <- ifelse(i == 1, "race/ethnicity", "income")
    lbl <- ifelse(i == 1, "re_cbb", "inc_cbb")
    header <- c("\\begin{table}[!ht]",
                "\\scriptsize",
                "\\centering",
                paste0("\\caption{Percentage of persons in households ",
                       "with access to a computer and broadband across HOLC ",
                       "neighborhood zones: by ", byg, "}"),
                paste0("\\label{tbl:mod_", lbl, "}"),
                paste0("\\begin{tabularx}{\\linewidth}{X*{", gn, "}{c}}"),
                "\\toprule",
                paste0("&",
                       paste(df %>%
                             distinct(group_f) %>%
                             pull %>%
                             str_replace_all(., "\\$", "\\\\$"), collapse = "&"),
                       "\\\\"))
    ## primary contents
    contents <- print(xtable(mat),
                      booktabs = TRUE,
                      sanitize.text.function = function(x){x},
                      include.colnames = FALSE,
                      include.rownames = FALSE,
                      only.contents = TRUE,
                      print.results = FALSE,
                      comment = FALSE)
    ## footer
    footer <- c(paste0("\\multicolumn{", gn + 1, "}{p{.98\\linewidth}}"),
                "{\\scriptsize{\\itshape Notes.} ",
                notes,
                "}",
                "\\end{tabularx}",
                "\\end{table}")

    ## save
    writeLines(c(header, contents, footer),
               con = file.path(tab_dir, paste0("mod_", lbl, ".tex")))
}

## -----------------------------------------------------------------------------
## END SCRIPT
################################################################################
