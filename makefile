# ==============================================================================
# 
# [ PROJ ] Digital redlining
# [ FILE ] makefile
# [ AUTH ] Benjamin Skinner, Hazel Levy, & Taylor Burtch
# [ INIT ] 2 April 2021
#
# ==============================================================================

# --- directories --------------------------------

DAT_DIR := data
DOC_DIR := docs
EST_DIR := estimates
FIG_DIR := figures
SCR_DIR := scripts
TAB_DIR := tables

# --- variables ----------------------------------

# data vars
acs_dat := $(DAT_DIR)/clean/acs_2015_2019_seq_135_sumlvl_140_us.RDS
bbd_dat := $(DAT_DIR)/clean/broadband_bg.RDS
cln_acs_dat := $(DAT_DIR)/clean/holc_acs_analysis.RDS
cln_fcc_dat := $(DAT_DIR)/clean/holc_fcc_analysis.RDS

# output vars (one example: assumes one change is all change)
acs_preds := $(DAT_DIR)/clean/predictions_acs.RDS
fcc_beta_preds := $(DAT_DIR)/clean/predictions_fcc_beta.RDS
fig_output := $(FIG_DIR)/inc_cbb_line.pdf
tab_output := $(TAB_DIR)/desc.tex
doc_output := $(DOC_DIR)/tabfig.pdf

# aux file vars; Stan options
aux_files := $(SCR_DIR)/r/macros.R $(SCR_DIR)/r/functions.R
stan_opts := $(SCR_DIR)/r/macros_stan.R

# --- build targets ------------------------------

all: setup get data analysis tables figures doc

data: $(acs_dat) $(bbd_dat) $(cln_acs_dat) $(cln_fcc_dat)
analysis: $(acs_preds) $(fcc_beta_preds) 
tables: $(tab_output)
figures: $(fig_output)
doc: $(doc_output)

.PHONY: all setup get data analysis tables figures doc

# --- packages -----------------------------------

setup: $(SCR_DIR)/r/check_packages.R
	@echo "Checking for and installing necessary R packages"
	Rscript $< .

# --- get data -----------------------------------

get: $(SCR_DIR)/r/get_data.R
	@echo "Checking for and downloading raw data"
	Rscript $< .

# --- clean data ---------------------------------

$(bbd_dat): $(SCR_DIR)/r/make_data_bb.R
	@echo "Making broadband data"
	@mkdir -p $(DAT_DIR)/clean
	Rscript $< .

$(acs_dat): $(SCR_DIR)/r/make_data_acs.R $(aux_files)
	@echo "Making ACS data"
	@mkdir -p $(DAT_DIR)/clean
	Rscript $< .

$(cln_acs_dat) $(cln_fcc_dat): $(SCR_DIR)/r/make_data_clean.R \
		$(aux_files) $(acs_dat) $(bbd_dat)
	@echo "Making clean data"
	@mkdir -p $(DAT_DIR)/clean
	Rscript $< .

# --- analysis -----------------------------------

$(acs_preds): $(SCR_DIR)/r/analysis_acs.R $(stan_opts) \
		$(aux_files) $(SCR_DIR)/stan/model_acs.stan \
		$(cln_acs_dat) 
	@echo "Running models: ACS"
	@mkdir -p $(EST_DIR)
	Rscript $< .

$(fcc_beta_preds): $(SCR_DIR)/r/analysis_fcc_beta.R $(stan_opts) \
		$(aux_files) $(SCR_DIR)/stan/model_fcc_beta.stan \
		$(cln_fcc_dat) 
	@echo "Running models: FCC (beta)"
	@mkdir -p $(EST_DIR)
	Rscript $< .

# --- tables & figures ---------------------------

$(tab_output): $(SCR_DIR)/r/make_tables.R $(acs_preds) $(fcc_beta_preds)
	@echo "Making tables"
	@mkdir -p $(TAB_DIR)	
	Rscript $< .

$(fig_output): $(SCR_DIR)/r/make_figures.R $(acs_preds) $(fcc_beta_preds)
	@echo "Making figures"
	@mkdir -p $(FIG_DIR)
	Rscript $< .

# --- docs ---------------------------------------

$(doc_output): $(DOC_DIR)/doc.md $(tab_output) $(fig_output)
	@echo "Changing into docs directory and compiling tables/figures"
	cd $(DOC_DIR) && \
	pandoc tabfig.md -f markdown -t pdf --output=tabfig.pdf && \
	cd ..

# --- clean up -----------------------------------

clean:
	@echo "Cleaning up directory"
	$(RM) -r $(DAT_DIR)/clean/*.RDS $(TAB_DIR)/*.tex 
	$(RM) -r $(FIG_DIR)/*.pdf $(EST_DIR)/*.csv $(DOC_DIR)/*.pdf
	$(RM) -r $(SCR_DIR)/stan/*_threads $(DAT_DIR)/crosswalks/acs_*

# ------------------------------------------------------------------------------
# end makefile
# ==============================================================================
