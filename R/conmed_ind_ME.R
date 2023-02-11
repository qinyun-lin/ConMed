#' Perform sensitivity analysis for mediation analysis
#' @description This command calculates the impact of an omitted variable necessary to invalidate/sustain an inference for a mediation effect, allowing for potential measurment error.
#' @param est_eff_a the estimated effect for path a (from treatment to mediator)
#' @param std_err_a the standard error of the estimate of path a (from treatment to mediator)
#' @param est_eff_b the estimated effect for path b (from mediator to outcome)
#' @param std_err_b the standard error of the estimate of path b (from mediator to outcome)
#' @param n_obs the number of observations in the sample
#' @param n_covariates_a the number of covariates in the regression model for estimating path a (from treatment to mediator), including the
#' @param n_covariates_b the number of covariates in the regression model for estimating path b (from mediator to outcome)
#' @param rel_M the reliability level of the mediator
#' @param rel_Y the reliability level of the outcome
#' @param rel_X the reliability level of the treatment
#' @param sd_X the standard deviation of the treatment
#' @param sd_M the standard deviation of the mediator
#' @param sd_Y the standard deviation of the outcome
#' @param R2_a the R square of the regression model that estiamtes the a pathway (from treatment to mediator)
#' @param R2_b the R square of the regression model that estiamtes the b pathway (from mediator to outcome)
#' @param alpha probability of rejecting the null hypothesis (defaults to 0.05)
#' @param tails integer whether hypothesis testing is one-tailed (1) or two-tailed (2; defaults to 2)
#' @return prints the ITCV to invalidate/sustain the inference & show the impact curve
#' @examples
#' conmed_ind_ME(est_eff_a = 0.171, std_err_a = 0.087, est_eff_b = 0.387, std_err_b = 0.077, n_obs = 123, rel_M = 0.9, rel_Y = 0.9, sd_X = 1, sd_M = 1.054093, sd_Y = 1.054093, R2_a = 0.02941117, R2_b = 0.1685979)
#' @export

conmed_ind_ME <- function(est_eff_a,
                       std_err_a,
                       est_eff_b,
                       std_err_b,
                       n_obs,
                       n_covariates_a = 0,
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
  ind <- est_eff_a * est_eff_b

  if (est_eff_a < 0) {
    critical_t_a <- stats::qt(1 - (alpha / tails), n_obs - n_covariates_a - 1) * -1
  } else {
    critical_t_a <- stats::qt(1 - (alpha / tails), n_obs - n_covariates_a - 1)
  }

  if (est_eff_b < 0) {
    critical_t_b <- stats::qt(1 - (alpha / tails), n_obs - n_covariates_b - 1) * -1
  } else {
    critical_t_b <- stats::qt(1 - (alpha / tails), n_obs - n_covariates_b - 1)
  }

  beta_threshold_a <- critical_t_a * std_err_a
  beta_threshold_b <- critical_t_b * std_err_b

  # transforming t into r (biased by measurement error)
  obs_r_ME_a <- (est_eff_a / std_err_a) / sqrt(((n_obs - n_covariates_a - 3) + ((est_eff_a / std_err_a)^2)))
  obs_r_ME_b <- (est_eff_b / std_err_b) / sqrt(((n_obs - n_covariates_b - 3) + ((est_eff_b / std_err_b)^2)))

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

  # decide which estimate to focus, either a or b
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
    output_print_ME(focus, ind, est_eff, beta_threshold, nu, obs_r, critical_r, r_con, itcv, alpha)
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

    output_print_both_ME(ind, est_eff_a, est_eff_b, beta_threshold_a, beta_threshold_b, nu,
                      obs_r_a, obs_r_b, critical_r_a, critical_r_b,
                      r_con_a, r_con_b, itcv_a, itcv_b, alpha)
  }

}

