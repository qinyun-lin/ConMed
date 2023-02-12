#' Perform sensitivity analysis for mediation analysis
#' @description This command calculates the impact of an omitted variable necessary to invalidate/sustain an inference for a mediation effect.
#' @param est_eff_a the estimated effect for path a (from treatment to mediator)
#' @param std_err_a the standard error of the estimate of path a (from treatment to mediator)
#' @param est_eff_b the estimated effect for path b (from mediator to outcome)
#' @param std_err_b the standard error of the estimate of path b (from mediator to outcome)
#' @param n_obs the number of observations in the sample
#' @param n_covariates_a the number of covariates in the regression model for estimating path a (from treatment to mediator)
#' @param n_covariates_b the number of covariates in the regression model for estimating path b (from mediator to outcome)
#' @param alpha probability of rejecting the null hypothesis (defaults to 0.05)
#' @param tails integer whether hypothesis testing is one-tailed (1) or two-tailed (2; defaults to 2)
#' @return prints the ITCV to invalidate/sustain the inference & show the impact curve
#' @examples
#' conmed_ind(est_eff_a = 0.181, std_err_a = 0.087, est_eff_b = 0.432, std_err_b = 0.074, nobs = 123)
#' @export
conmed_ind <- function(est_eff_a,
                      std_err_a,
                      est_eff_b,
                      std_err_b,
                      n_obs,
                      n_covariates_a = 0,
                      n_covariates_b = 1,
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
  ind <- est_eff_a * est_eff_b

  # transforming t into r
  obs_r_a <- (est_eff_a / std_err_a) / sqrt(((n_obs - n_covariates_a - 3) + ((est_eff_a / std_err_a)^2)))
  obs_r_b <- (est_eff_b / std_err_b) / sqrt(((n_obs - n_covariates_b - 3) + ((est_eff_b / std_err_b)^2)))

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
    # none of a and b is significant, then we really need to sustain both
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
    output_print(focus, ind, est_eff, beta_threshold, nu, obs_r, critical_r, r_con, itcv, alpha)
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

    output_print_both(ind, est_eff_a, est_eff_b, beta_threshold_a, beta_threshold_b, nu,
                      obs_r_a, obs_r_b, critical_r_a, critical_r_b,
                      r_con_a, r_con_b, itcv_a, itcv_b, alpha)
  }

}

