This repository contains the replication files for  

> Skinner, B.T., Levy, H., and Burtch, T. (2021). Digital redlining:
> the relevance of 20th century housing policy to 21st century
> broadband access and education. _Annenberg Institute at Brown
> University_ (EdWorkingPaper: 21-471).
> https://doi.org/10.26300/q9av-9c93 

## To run

1. Clone the project repository and `cd` into project directory 

``` bash
git clone https://github.com/btskinner/civic_returns_pf_rep.git
```

2. Download data and place into the proper directories

See the README file in the `data` directory for links to the data
files you'll need. Please note that the raw Census, FCC, TIGER/Line
data files are quite large and can take a while to download.

3. Run scripts

Either `cd` into the main directory and run the `makefile`:

```bash
cd ./digital_redlining_rep
make
```
Alternately, after cloning the repository, run the R scripts one by one from
the `digital_redlining_rep/scripts/r` directory:

1. `check_packages.R`
1. `make_data_acs.R`
1. `make_data_bb.R`
1. `make_data_clean.R`
1. `analysis_fcc_beta.R`
1. `analysis_acs.R`
1. `make_tables.R`
1. `make_figures.R`

## Required software

Data cleaning and most analyses for this project are completed using the [R
language](https://cran.r-project.org). For Bayesian models, we use the
[Stan language](https://mc-stan.org) through the
[`cmdstanr`](https://mc-stan.org/cmdstanr/) interface. We use the
following R packages:

- [crosswalkr](https://CRAN.R-project.org/package=crosswalkr)
- [cmdstanr](https://mc-stan.org/cmdstanr/)
- [data.table](https://CRAN.R-project.org/package=data.table)
- [devtools](https://CRAN.R-project.org/package=devtools)
- [ggthemes](https://CRAN.R-project.org/package=ggthemes)
- [gtools](https://CRAN.R-project.org/package=gtools)
- [knitr](https://CRAN.R-project.org/package=knitr)
- [lubridate](https://CRAN.R-project.org/package=lubridate)
- [patchwork](https://CRAN.R-project.org/package=patchwork)
- [rstan](https://CRAN.R-project.org/package=rstan)
- [readxl](https://CRAN.R-project.org/package=readxl)
- [sf](https://CRAN.R-project.org/package=sf)
- [tidyverse](https://CRAN.R-project.org/package=tidyverse)
- [xtable](https://CRAN.R-project.org/package=xtable)


