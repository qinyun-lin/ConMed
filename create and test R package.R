install.packages("devtools")
library("devtools")
devtools::install_github("r-lib/roxygen2")
install.packages("roxygen2")
library("roxygen2")

library(ggplot2)

devtools::check()
devtools::install()

library(ConMed)

# after downloading the package to your working directory
install.packages("ConMed_0.0.0.9000.tar.gz", repos=NULL, type="source")
library(conmediator)

#######################
conmed_ind(est_eff_a = 0.181,
           std_err_a = 0.087,
           est_eff_b = 0.432,
           std_err_b = 0.074,
           n_obs = 123,
           n_covariates_a = 0,
           n_covariates_b = 1,
           alpha = 0.05,
           tails = 2)

conmed_ind_ME(est_eff_a = 0.171, std_err_a = 0.087,
              est_eff_b = 0.387, std_err_b = 0.077, n_obs = 123,
              rel_M = 0.9, rel_Y = 0.9,
              sd_X = 1, sd_M = 1.054093, sd_Y = 1.054093,
              R2_a = 0.02941117, R2_b = 0.1685979)



conmed_ind(est_eff_a = 0.432,
           std_err_a = 0.074,
           est_eff_b = 0.432,
           std_err_b = 0.074,
           n_obs = 123,
           n_covariates_a = 1,
           n_covariates_b = 2,
           alpha = .05,
           tails = 2,
           nu = 0)

conmed_ind(est_eff_a = 0.181,
           std_err_a = 0.2,
           est_eff_b = 0.432,
           std_err_b = 0.074,
           n_obs = 123,
           n_covariates_a = 1,
           n_covariates_b = 2,
           alpha = .05,
           tails = 2,
           nu = 0)

conmed_ind_ME(est_eff_a = 0.8,
              std_err_a = 0.087,
              est_eff_b = 0.181,
              std_err_b = 0.074,
              n_obs = 123,
              n_covariates_a = 1,
              n_covariates_b = 1,
              rel_M = 1,
              rel_Y = 0.5,
              rel_X = 1,
              sd_X = 1,
              sd_M = 1,
              sd_Y = 1,
              R2_a = 0.03268,
              R2_b = 0.03268,
              alpha = .05,
              tails = 2,
              nu = 0)


est_eff_a = 0.181
std_err_a = 0.087
est_eff_b = 0.432
std_err_b = 0.074
n_obs = 123
n_covariates_a = 1
n_covariates_b = 2
alpha = .05
tails = 2
nu = 0

est_eff_a = 0.181
std_err_a = 0.087
est_eff_b = 0.432
std_err_b = 0.074
n_obs = 123
n_covariates_a = 1
n_covariates_b = 2
rel_M = 0.9
rel_Y = 0.9
rel_X = 1
sd_X = 1
sd_M = 1
sd_Y = 1
R2_a = 0.03268
R2_b = 0.2059
alpha = .05
tails = 2
nu = 0
