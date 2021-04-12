#' Perform sensitivity analysis for mediation analysis
#' @description For published studies, this command calculates (1) how much bias there must be in an estimate to invalidate/sustain an inference; (2) the impact of an omitted variable necessary to invalidate/sustain an inference for a regression coefficient.
#' @param est_eff the estimated effect (such as an unstandardized beta coefficient or a group mean difference)
#' @param std_err the standard error of the estimate of the unstandardized regression coefficient
#' @param n_obs the number of observations in the sample
#' @param n_covariates the number of covariates in the regression model
#' @param alpha probability of rejecting the null hypothesis (defaults to 0.05)
#' @param tails integer whether hypothesis testing is one-tailed (1) or two-tailed (2; defaults to 2)
#' @param index whether output is RIR or IT (impact threshold); defaults to "IT"
#' @param nu what hypothesis to be tested; defaults to testing whether est_eff is significantly different from 0
#' @param model_type the type of model being estimated; defaults to "ols" for a linear regression model; the other option is "logistic"
#' @param n_treat the number of cases associated with the treatment condition; applicable only when model_type = "logistic"
#' @param switch_trm whether to switch the treatment and control cases; defaults to FALSE; applicable only when model_type = "logistic"
#' @param a cell is the number of cases in the control group showing unsuccessful results
#' @param b cell is the number of cases in the control group showing successful results
#' @param c cell is the number of cases in the treatment group showing unsuccessful results
#' @param d cell is the number of cases in the treatment group showing successful results
#' @param two_by_two_table table that is a matrix or can be coerced to one (data.frame, tibble, tribble) from which the a, b, c, and d arguments can be extracted
#' @param test whether using Fisher's Exact Test or A chi-square test; defaults to Fisher's Exact Test
#' @param replace whether using entire sample or the control group to calculate the base rate; default is the entire sample
#' @param to_return whether to return a data.frame (by specifying this argument to equal "raw_output" for use in other analyses) or a plot ("plot"); default is to print ("print") the output to the console; can specify a vector of output to return
#' @importFrom stats fisher.test
#' @return prints the bias and the number of cases that would have to be replaced with cases for which there is no effect to invalidate the inference
#' @examples
#' # using pkonfound for linear models
#' pkonfound(2, .4, 100, 3)
#' pkonfound(-2.2, .65, 200, 3)
#' pkonfound(.5, 3, 200, 3)
#' pkonfound(-0.2, 0.103, 20888, 3, n_treat = 17888, model_type = "logistic")
#'
#' pkonfound(2, .4, 100, 3, to_return = "thresh_plot")
#' pkonfound(2, .4, 100, 3, to_return = "corr_plot")
#'
#' pkonfound_output <- pkonfound(2, .4, 200, 3,
#'   to_return = c("raw_output", "thresh_plot", "corr_plot")
#' )
#' summary(pkonfound_output)
#' pkonfound_output$raw_output
#' pkonfound_output$thresh_plot
#' pkonfound_output$corr_plot
#'
#' # using pkonfound for a 2x2 table
#' pkonfound(a = 35, b = 17, c = 17, d = 38)
#' pkonfound(a = 35, b = 17, c = 17, d = 38, alpha = 0.01)
#' pkonfound(a = 35, b = 17, c = 17, d = 38, alpha = 0.01, switch_trm = FALSE)
#' pkonfound(a = 35, b = 17, c = 17, d = 38, test = "chisq")
#'
# my_table <- tibble::tribble(
# ~unsuccess, ~success,
# 35,         17,
# 17,         38,
# )
#
# pkonfound(two_by_two_table = my_table)
#'
#' @export

conmed_ind_ME <- function(est_eff_a,
                       std_err_a,
                       est_eff_b,
                       std_err_b,
                       n_obs,
                       n_covariates_a = 1,
                       n_covariates_b = 1,
                       rel_M = 1,
                       rel_Y = 1,
                       rel_X = 1,
                       sd_X,
                       sd_M,
                       sd_Y,
                       R2_a,
                       R2_b,
                       alpha = .05,
                       tails = 2,
                       nu = 0) {

  # calculating statistics used in every case
  if (est_eff_a < 0) {
    critical_t_a <- stats::qt(1 - (alpha / tails), n_obs - n_covariates_a - 2) * -1
  } else {
    critical_t_a <- stats::qt(1 - (alpha / tails), n_obs - n_covariates_a - 2)
  }

  if (est_eff_b < 0) {
    critical_t_b <- stats::qt(1 - (alpha / tails), n_obs - n_covariates_b - 2) * -1
  } else {
    critical_t_b <- stats::qt(1 - (alpha / tails), n_obs - n_covariates_b - 2)
  }

  beta_threshold_a <- critical_t_a * std_err_a
  beta_threshold_b <- critical_t_b * std_err_b

  # transforming t into r (biased by measurement error)
  obs_r_ME_a <- (est_eff_a / std_err_a) / sqrt(((n_obs - n_covariates_a - 2) + ((est_eff_a / std_err_a)^2)))
  obs_r_ME_b <- (est_eff_b / std_err_b) / sqrt(((n_obs - n_covariates_b - 2) + ((est_eff_b / std_err_b)^2)))

  # calculate important intermediate products
  R2_yz_ME_a <- max(0, (obs_r_ME_a^2 - R2_a) / (obs_r_ME_a^2 - 1))
  R2_xz_ME_a <- max(0, 1 - (sd_M^2 * (1 - R2_a)) / (sd_X^2 * (n_obs - n_covariates_a - 2) * std_err_a^2))

  R2_yz_ME_b <- max(0, (obs_r_ME_b^2 - R2_b) / (obs_r_ME_b^2 - 1))
  R2_xz_ME_b <- max(0, 1 - (sd_Y^2 * (1 - R2_b)) / (sd_M^2 * (n_obs - n_covariates_b - 2) * std_err_b^2))

  # correct for measurement error in obs_r_ME
  obs_r_a <- (obs_r_ME_a/sqrt(rel_M) - sqrt(R2_yz_ME_a) / sqrt(rel_M) * sqrt(R2_xz_ME_a) / sqrt(rel_X)) /
    sqrt(1 - R2_yz_ME_a / rel_M) * sqrt(1 - R2_xz_ME_a / rel_X)
  obs_r_b <- (obs_r_ME_b/sqrt(rel_Y) - sqrt(R2_yz_ME_b) / sqrt(rel_Y) * sqrt(R2_xz_ME_b) / sqrt(rel_M)) /
    sqrt(1 - R2_yz_ME_b / rel_Y) * sqrt(1 - R2_xz_ME_b / rel_M)

  # finding critical r
  critical_r_a <- critical_t_a / sqrt((critical_t_a^2) + (n_obs - n_covariates_a - 3))
  critical_r_b <- critical_t_b / sqrt((critical_t_b^2) + (n_obs - n_covariates_b - 3))

  # decide which estiamte to focus, either a or b
  if (abs(obs_r_a) > abs(critical_r_a) & abs(obs_r_b) > abs(critical_r_b)) {
    # both a and b are significant, then pick the one that is easier to invalidate
    if (abs(obs_r_a) > abs(obs_r_b)) {
      obs_r <- obs_r_b
      critical_r <- critical_r_b
      est_eff <- est_eff_b
      beta_threshold <- beta_threshold_b
      focus <- "inval_b"
    } else {
      obs_r <- obs_r_a
      critical_r <- critical_r_a
      est_eff <- est_eff_a
      beta_threshold <- beta_threshold_a
      focus <- "inval_a"
    }
  }

  if (abs(obs_r_a) > abs(critical_r_a) & abs(obs_r_b) <= abs(critical_r_b)) {
    # only a is significant, then sustain b
    obs_r <- obs_r_b
    critical_r <- critical_r_b
    est_eff <- est_eff_b
    beta_threshold <- beta_threshold_b
    focus <- "sustain_b"
  }

  if (abs(obs_r_a) <= abs(critical_r_a) & abs(obs_r_b) > abs(critical_r_b)) {
    # only b is significant, then sustain a
    obs_r <- obs_r_a
    critical_r <- critical_r_a
    est_eff <- est_eff_a
    beta_threshold <- beta_threshold_a
    focus <- "sustain_a"
  }

  if (abs(obs_r_a) <= abs(critical_r_a) & abs(obs_r_b) <= abs(critical_r_b)) {
    # none of a and b is significant, then we really need both to sustain
    focus <- "both a and b"
  }

  # calculating threshold
  if (focus != "both a and b") {

    if ((abs(obs_r) > abs(critical_r)) & ((obs_r * critical_r) > 0)) {
      mp <- -1
    } else {
      mp <- 1
    }
    # calculating impact of the confounding variable
    itcv <- (obs_r - critical_r) / (1 + mp * abs(critical_r))
    # finding correlation of confound to invalidate / sustain inference
    r_con <- round(sqrt(abs(itcv)), 3)
    output_print(ME = T, focus, est_eff, beta_threshold, nu, obs_r, critical_r, r_con, itcv, alpha)
    if (focus == "inval_a" | focus == "inval_b") {
      plot_curve(focus, obs_r, critical_r, r_con)
    }
  } else {

    if ((abs(obs_r_a) > abs(critical_r_a)) & ((obs_r_a * critical_r_a) > 0)) {
      mp_a <- -1
    } else {
      mp_a <- 1
    }
    # calculating impact of the confounding variable
    itcv_a <- (obs_r_a - critical_r_a) / (1 + mp_a * abs(critical_r_a))
    # finding correlation of confound to invalidate / sustain inference
    r_con_a <- round(sqrt(abs(itcv_a)), 3)

    if ((abs(obs_r_b) > abs(critical_r_b)) & ((obs_r_b * critical_r_b) > 0)) {
      mp_b <- -1
    } else {
      mp_b <- 1
    }
    # calculating impact of the confounding variable
    itcv_b <- (obs_r_b - critical_r_b) / (1 + mp_b * abs(critical_r_b))
    # finding correlation of confound to invalidate / sustain inference
    r_con_b <- round(sqrt(abs(itcv_b)), 3)

    #output_print_both(est_eff_a, est_eff_b, beta_threshold_a, beta_threshold_b, nu,
    #                  obs_r_a, obs_r_b, critical_r_a, critical_r_b,
    #                  r_con_a, r_con_b, itcv_a, itcv_b, alpha)
  }

}

