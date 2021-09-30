This repository contains the replication files for  

> Skinner, B.T., Levy, H., and Burtch, T. (2021). Digital
> redlining: the relevance of 20th century housing policy on 21st
> century broadband access for education

## To run

Clone the project repository, `cd` into project directory, and run the `makefile`:

```bash
git clone https://github.com/btskinner/civic_returns_pf_rep.git
cd ./digital_redlining_rep
make
```

### Run scripts separately

Or, after cloning the repository, run the R scripts one by one from
the `digital_redlining_rep/scripts/r` directory:

1. `get_data.R`
1. `check_packages.R`
1. `make_data_acs.R`
1. `make_data_bb.R`
1. `make_data_clean.R`
1. `analysis_fcc_beta.R`
1. `analysis_acs.R`
1. `make_tables.R`
1. `make_figures.R`
