################################################################################
##
## [ PROJ ] Digital redlining / HOLC
## [ FILE ] make_figures.R
## [ AUTH ] Skinner, Levy, Burtch (@btskinner)
## [ INIT ] 29 September 2020
##
################################################################################

## libraries
libs <- c("tidyverse", "data.table", "sf", "patchwork", "ggthemes", "lubridate")
sapply(libs, require, character.only = TRUE)

## directory paths
args <- commandArgs(trailingOnly = TRUE)
root <- ifelse(length(args) == 0, file.path("..", ".."), args)
dat_dir <- file.path(root, "data")
cln_dir <- file.path(dat_dir, "clean")
fig_dir <- file.path(root, "figures")
scr_dir <- file.path(root, "scripts", "r")

## external functions
source(file.path(scr_dir, "functions.R"))

## macros
source(file.path(scr_dir, "macros.R"))

## font
## NB: https://medium.com/@fulowa/latex-font-in-a-ggplot-9120caaa5250
font_family <- ""

## -----------------------------------------------------------------------------
## read in data
## -----------------------------------------------------------------------------

## holc
df_holc <- readRDS(file.path(cln_dir, "sf_holc.RDS"))

## holc w/ FCC 477 data
df_holc_bg <- readRDS(file.path(cln_dir, "sf_holc_bg.RDS")) %>%
    st_drop_geometry()

## blockgroup for just Evansville, IN
df_bg_evv <- readRDS(file.path(cln_dir, "sf_bg_us.RDS")) %>%
    filter(str_sub(geoid, 1, 5) == "18163")

## -----------------------------------------------------------------------------
## locations on national map
## -----------------------------------------------------------------------------

## make map
p <- ggplot(st_as_sf(maps::map("state", fill = TRUE, plot = FALSE)) %>%
            st_transform(st_crs(common_crs))) +
    geom_sf(fill = "white", size = 0.25) +
    geom_sf(data = df_holc %>%
                group_by(city) %>%
                summarise(geometry = st_union(geometry), .groups = "keep") %>%
                st_centroid(),
            size = 0.75) +
    coord_sf(datum = st_crs(common_crs)) +
    theme_map(base_family = font_family)

## save
ggsave("holc_national_map.pdf",
       plot = p,
       path = fig_dir,
       width = 8.5,
       height = 6,
       units = "in",
       dpi = "retina")

## -----------------------------------------------------------------------------
## plot overlap example
## -----------------------------------------------------------------------------

## get area around Evansville, IN
df_evv <- st_intersection(df_bg_evv, df_holc %>% filter(city == "Evansville"))

## get bounding box
lims <- st_bbox(df_evv) + c(-1000, -1000, 1000, 1000)

## plot 1: census block groups around Evansville, IN
p1 <- ggplot(df_bg_evv) +
    geom_sf(alpha = 0.5,
            linetype = "dashed",
            size = 0.2) +
    coord_sf(datum = st_crs(common_crs),
             xlim = c(lims[1], lims[3]),
             ylim = c(lims[2], lims[4])) +
    labs(title = "(A) Census block groups") +
    theme_map(base_size = 8,
              base_family = font_family)

## plot 2: HOLC areas for Evansville, IN
p2 <- ggplot(df_holc %>% filter(city == "Evansville")) +
    geom_sf(fill = "red",
            alpha = 0.5,
            size = 0.3) +
    coord_sf(datum = st_crs(common_crs),
             xlim = c(lims[1], lims[3]),
             ylim = c(lims[2], lims[4])) +
    labs(title = "(B) HOLC neighborhoods") +
    theme_map(base_size = 8,
              base_family = font_family)

## plot 3: show overlap
p3 <- ggplot() +
    geom_sf(data = df_bg_evv %>% filter(geoid %in% df_evv[["geoid"]]),
            fill = "blue",
            alpha = 0.5,
            linetype = "dashed",
            size = 0.2) +
    geom_sf(data = df_holc %>% filter(city == "Evansville"),
            fill = "red",
            alpha = 0.5,
            size = 0.2) +
    coord_sf(datum = st_crs(common_crs),
             xlim = c(lims[1], lims[3]),
             ylim = c(lims[2], lims[4])) +
    labs(title = "(C) Overlap of census block groups and HOLC areas") +
    theme_map(base_size = 8,
              base_family = font_family)

## plot 4: show block group parts inside HOLC areas
p4 <- ggplot(df_holc %>% filter(city == "Evansville")) +
    geom_sf(size = 0.3) +
    geom_sf(data = df_evv,
            alpha = 0.4,
            linetype = "dashed",
            size = 0.2) +
    coord_sf(datum = st_crs(common_crs),
             xlim = c(lims[1], lims[3]),
             ylim = c(lims[2], lims[4])) +
    labs(title = "(D) Census block group portions inside HOLC neighborhoods") +
    theme_map(base_size = 8,
              base_family = font_family)


## use patchwork library to arrange four plots into grid
p <- (p1 | p2) / (p3 | p4) +
    plot_annotation(
        ## title = "Census block groups and HOLC maps for Evansville, IN",
        ## caption = "Data: ACS; Mapping Inequality Project",
        theme = theme(plot.title = element_text(size = 12, family = font_family))
    )

## save plot
ggsave("holc_overlap.pdf",
       plot = p,
       path = fig_dir,
       width = 8.5,
       height = 8.5,
       units = "in",
       dpi = "retina")

## -----------------------------------------------------------------------------
## predictions
## -----------------------------------------------------------------------------

## -------------------------------------
## FCC
## -------------------------------------

preds <- readRDS(file.path(cln_dir, "predictions_fcc_beta.RDS")) %>%
    ## convert to percentage
    mutate(val = val * 100) %>%
    ## compute 95% CIs
    group_by(group, period, period_f, var) %>%
    summarise(q500 = quantile(val, probs = .5),
              q025 = quantile(val, probs = .025),
              q975 = quantile(val, probs = .975),
              .groups = "drop") %>%
    ## rename beta_* to HOLC grades
    mutate(var = case_when(
               var == "beta_1" ~ "A",
               var == "beta_2" ~ "B",
               var == "beta_3" ~ "C",
               var == "beta_4" ~ "D"
           ),
           group_paper = factor(group,
                                levels = paste0(c("adsl",
                                                  "adsl2",
                                                  "docsis30",
                                                  "docsis31",
                                                  "vdsl",
                                                  "fiber"),"_access_beta"),
                                labels = tech_labs(paste0(c("adsl",
                                                            "adsl2",
                                                            "docsis30",
                                                            "docsis31",
                                                            "vdsl",
                                                            "fiber"),"_access_beta"))
                                ),
           group_slides = factor(group,
                                 levels = paste0(c("adsl",
                                                   "adsl2",
                                                   "vdsl",
                                                   "docsis30",
                                                   "docsis31",
                                                   "fiber"),"_access_beta"),
                                 labels = tech_labs(paste0(c("adsl",
                                                             "adsl2",
                                                             "vdsl",
                                                             "docsis30",
                                                             "docsis31",
                                                             "fiber"),"_access_beta"))
                                 )
           )

## line plots
g <- ggplot(preds,
            aes(x = period_f, y = q500)) +
    facet_wrap(~ group_paper, ncol = 2, scales = "free_y") +
    geom_line(aes(colour = var)) +
    geom_ribbon(aes(ymin = q025, ymax = q975, fill = var), alpha = .2) +
    scale_colour_manual("HOLC", values = holc_vals) +
    scale_fill_manual("HOLC", values = holc_vals) +
    scale_x_date(breaks = seq(as.Date("2014-12-01"),
                              length.out = 10,
                              by = "6 months"),
                 date_labels = "%b %Y",
                 limits = as.Date(c("2014-12-01", "2019-06-01"))) +
    labs(x = "",
         y = "Posterior prediction of population with access") +
    theme_bw(base_family = font_family) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))

## save
ggsave("fcc_line.pdf",
       plot = g,
       path = fig_dir,
       width = 8.5,
       height = 10,
       units = "in",
       dpi = "retina")

## -------------------------------------
## ACS
## -------------------------------------

preds <- readRDS(file.path(cln_dir, "predictions_acs.RDS"))

## adjust preds to get CIs
preds_ci <- preds %>%
    group_by(group_f, var) %>%
    summarise(q025 = quantile(val, probs = .025),
              q250 = quantile(val, probs = .25),
              q500 = quantile(val, probs = .5),
              q750 = quantile(val, probs = .75),
              q975 = quantile(val, probs = .975),
              .groups = "drop") %>%
    ## rename beta_* to HOLC grades
    mutate(var = case_when(
               var == "beta_1" ~ "A",
               var == "beta_2" ~ "B",
               var == "beta_3" ~ "C",
               var == "beta_4" ~ "D"
           ),
           var = factor(var,
                        levels = rev(LETTERS[1:4])))

## ---------------------------
## overall
## ---------------------------

preds_ov <- preds %>%
    filter(str_detect(group_f, "Overall", negate = FALSE)) %>%
    ## rename beta_* to HOLC grades
    mutate(var = case_when(
               var == "beta_1" ~ "A",
               var == "beta_2" ~ "B",
               var == "beta_3" ~ "C",
               var == "beta_4" ~ "D"
           ),
           var = factor(var,
                        levels = rev(LETTERS[1:4])))

g <- ggplot(preds_ov,
            aes(x = val, fill = var)) +
    geom_density(aes(y = ..scaled..), alpha = 0.7) +
    ## geom_hline(yintercept = 0, colour = "grey") +
    scale_fill_manual("HOLC",
                      values = holc_vals) +
    scale_x_continuous(breaks = seq(.86,.94,.01),
                       minor_breaks = seq(.86,.94,.005),
                       labels = scales::percent_format(accuracy = 1),
                       limits = c(.86,.94),
                       expand = c(0,0)) +
    labs(x = "Percentage",
         y = "Overall household proportions (density)") +
    theme_bw(base_family = font_family) +
    theme(panel.grid.major.y = element_blank(),
          panel.grid.minor.y = element_blank(),
          axis.ticks.y = element_blank(),
          axis.text.y = element_blank(),
          axis.text.x = element_text(angle = 45, hjust = 1))

## save
ggsave("ov_cbb_dens.pdf",
       plot = g,
       path = fig_dir,
       width = 8.5,
       height = 5,
       units = "in",
       dpi = "retina")

## ---------------------------
## subgroups
## ---------------------------

preds_ci_re <- preds_ci %>%
    filter(str_detect(group_f, "\\$|Overall", negate = TRUE)) %>%
    mutate(group_f = factor(group_f,
                            levels = rev(c("AI/AK",
                                       "Asian",
                                       "Black",
                                       "Hispanic",
                                       "Multiple races",
                                       "NH/PI",
                                       "Other race",
                                       "White"))))
preds_ci_in <- preds_ci %>%
    filter(str_detect(group_f, "\\$", negate = FALSE)) %>%
    mutate(group_f = factor(group_f,
                            levels = c("<$10k",
                                       "[$10k, $20k)",
                                       "[$20k, $35k)",
                                       "[$35k, $50k)",
                                       "[$50k, $75k)",
                                       "$75k+")))

## stacked line plot
g <- ggplot(preds_ci_re,
            aes(x = group_f, colour = var)) +
    geom_linerange(aes(x = group_f, ymin = q025, ymax = q975),
                   position = position_dodge(.9),
                   size = 1) +
    geom_linerange(aes(x = group_f, ymin = q250, ymax = q750),
                   position = position_dodge(.9),
                   size = 1.5) +
    geom_point(aes(y = q500, colour = var),
               shape = 21,
               position = position_dodge(.9),
               fill = "white",
               size  = 2) +
    geom_vline(xintercept = seq(0.5, preds_ci_re %>%
                                     distinct(group_f) %>%
                                     pull %>%
                                     length,
                                by = 1),
               colour = "gray",
               size = .5,
               alpha = .5) +
    coord_flip(expand = FALSE) +
    scale_colour_manual("HOLC",
                        values = holc_vals,
                        guide = guide_legend(reverse = TRUE)) +
    scale_y_continuous(breaks = seq(.85,1,.05),
                       minor_breaks = seq(.85,1,.01),
                       labels = scales::percent_format(accuracy = 1),
                       limits = c(.85,1)) +
    labs(y = "Percentage",
         x = "Race/ethnicity") +
    theme_bw(base_family = font_family) +
    theme(panel.grid.major.y = element_blank(),
          axis.text.x = element_text(angle = 45, hjust = 1))

## save
ggsave("re_cbb_line.pdf",
       plot = g,
       path = fig_dir,
       width = 8.5,
       height = 5,
       units = "in",
       dpi = "retina")

## stacked line plot
g <- ggplot(preds_ci_in,
            aes(x = group_f, colour = var)) +
    geom_linerange(aes(x = group_f, ymin = q025, ymax = q975),
                   position = position_dodge(.9),
                   size = 1) +
    geom_linerange(aes(x = group_f, ymin = q250, ymax = q750),
                   position = position_dodge(.9),
                   size = 1.5) +
    geom_point(aes(y = q500, colour = var),
               shape = 21,
               position = position_dodge(.9),
               fill = "white",
               size  = 2) +
    geom_vline(xintercept = seq(0.5, preds_ci_in %>%
                                     distinct(group_f) %>%
                                     pull %>%
                                     length,
                                by = 1),
               colour = "gray",
               size = .5,
               alpha = .5) +
    coord_flip(expand = FALSE) +
    scale_colour_manual("HOLC",
                        values = holc_vals,
                        guide = guide_legend(reverse = TRUE)) +
    scale_y_continuous(breaks = seq(.5,1,.05),
                       minor_breaks = seq(.5,1,.01),
                       labels = scales::percent_format(accuracy = 1),
                       limits = c(.5,1)) +
     labs(y = "Percentage",
          x = "Income") +
    theme_bw(base_family = font_family) +
    theme(panel.grid.major.y = element_blank(),
          axis.text.x = element_text(angle = 45, hjust = 1))

## save
ggsave("inc_cbb_line.pdf",
       plot = g,
       path = fig_dir,
       width = 8.5,
       height = 5,
       units = "in",
       dpi = "retina")

## -----------------------------------------------------------------------------
## groups by zone
## -----------------------------------------------------------------------------

## analysis file
holc <- readRDS(file.path(cln_dir, "holc_acs_analysis.RDS"))

## v38 = "v40",       # income bands w/ broadband: <$10k
## v42 = "v44",       # ...[10k, 20k)
## v46 = "v48",       # ...[20k, 35k)
## v50 = "v52",       # ...[35k, 50k)
## v54 = "v56",       # ...[50k, 75k)
## v58 = "v60",       # ...75k+

## v131 = "v133",     # broadband | computer: white
## v137 = "v139",     # ...black
## v143 = "v145",     # ...amerind/alnat
## v149 = "v151",     # ...asian
## v155 = "v157",     # ...nhpi
## v161 = "v163",     # ...other race alone
## v167 = "v169",     # ...two or more races
## v173 = "v175",     # ...white, non-hispanic
## v179 = "v181")     # ...hispanic/latino

df <- holc %>%
    select(holc_grade,
           paste0("v",
                  c("38","40","42","44","46","48","50","52","54","56","58","60",
                    "137","139","143","145","149","151","155","157",
                    "161","163","167","169","173","175","179","181"),
                  "_wc")) %>%
    group_by(holc_grade) %>%
    summarise(across(, ~ sum(.x))) %>%
    filter(holc_grade != "E") %>%
    pivot_longer(cols = starts_with("v"),
                 names_to = "var",
                 values_to = "val") %>%
    mutate(var = case_when(
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
    select(holc_grade, group, total_zone = total) %>%
    arrange(holc_grade, group)

## get overall totals
df_tot <- df %>%
    group_by(group) %>%
    summarise(total = sum(total_zone))

## rejoin and compute percentages
df <- df %>%
    left_join(df_tot, by = "group") %>%
    mutate(pct = round(total_zone / total * 100),
           demo = ifelse(str_detect(group, "\\$"),
                         "Income",
                         "Race/ethnicity"),
           demo = factor(demo,
                         levels = c("Race/ethnicity",
                                    "Income")),
           group = factor(group,
                          levels = c("AI/AK",
                                     "Asian",
                                     "Black",
                                     "Hispanic",
                                     "Multiple races",
                                     "NH/PI",
                                     "Other race",
                                     "White",
                                     "<$10k",
                                     "[$10k, $20k)",
                                     "[$20k, $35k)",
                                     "[$35k, $50k)",
                                     "[$50k, $75k)",
                                     "$75k+")))

## get proportions within group
df_prop <- df %>%
    distinct(group, demo, total) %>%
    group_by(demo) %>%
    summarise(total_all = sum(total),
              pct_all = total / total_all * 100,
              group = group,
              .groups = "drop") %>%
    arrange(group)

## rejoin and make labels
df <- df %>%
    left_join(df_prop, by = c("demo", "group")) %>%
    mutate(group_ = paste0(group, "\n(", round(pct_all, 2), ")"),
           group_ = fct_relevel(group_, "<$10k\n(8.09)"))

## bar plot
g <- ggplot(df, aes(x = group_, y = pct, fill = holc_grade)) +
    facet_wrap(~ demo, ncol = 1, scales = "free_x") +
    geom_bar(stat = "identity", position = "dodge") +
    geom_text(aes(label = pct, family = font_family),
              position = position_dodge(width = 0.9),
              vjust = -0.5,
              size = 3) +
    scale_fill_manual("HOLC", values = holc_vals) +
    scale_y_continuous(limits = c(0,50)) +
    labs(y = "Percentage in HOLC zone",
         x = "") +
    theme_bw(base_family = font_family) +
    theme(panel.grid.major.x = element_blank())

## save
ggsave("sample_dist_bar.pdf",
       plot = g,
       path = fig_dir,
       width = 8.5,
       height = 5,
       units = "in",
       dpi = "retina")

## bar plot: count
g <- ggplot(df %>% mutate(total_zone = total_zone / 1000),
            aes(x = group_, y = total_zone, fill = holc_grade)) +
    facet_wrap(~ demo, ncol = 1, scales = "free_x") +
    geom_bar(stat = "identity", position = "dodge") +
    geom_text(aes(label = round(total_zone), family = font_family),
              position = position_dodge(width = 0.9),
              vjust = -0.5,
              size = 2.25) +
    scale_fill_manual("HOLC", values = holc_vals) +
    scale_y_continuous(breaks = seq(0,6000,1000),
                       limits = c(0,6200)) +
    labs(y = "Number (1000s) in HOLC zone",
         x = "") +
    theme_bw(base_family = font_family) +
    theme(panel.grid.major.x = element_blank())

ggsave("sample_dist_bar_count.pdf",
       plot = g,
       path = fig_dir,
       width = 8.5,
       height = 5,
       units = "in",
       dpi = "retina")

## -----------------------------------------------------------------------------
## END SCRIPT
################################################################################
