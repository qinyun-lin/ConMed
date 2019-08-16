#' how the estimate and confidence interval change with regards to correlation between X and Mu
#'
#' give the result for how the estimate and confidence interval change with regards to correlation between X and Mu
#'
#' @param rxmo correlation between X and Mo
#' @param rxy correlation between X and Y
#' @param rymo correlation between Y and Mo
#' @param rmomu correlation between Mo and Mu
#' @param rymu correlation between Y and Mu
#' @param nobs number of observations in the sample or sample size
#' @param conflevel confidence interval level, default is 0.95
#'
#' @return result for how rxmu affects the estimates
#' @import stats
#' @export
rxmu_data <- function(rxmo=-2,rxy=-2,rymo=-2,rmomu=-2,rymu=-2, nobs=0, conflevel=0.95){
  #check input
  if (!(check_input(rxmo) && check_input(rxy) && check_input(rymo) && check_input(rymu) && check_input(rmomu))) {
    stop("Error: input is problematic! Correlations should be among -1 and 1.")
  }
  if (nobs <= 0 ) {
    stop("Error: input is problematic! Please give positive numbers for sample size!")
  }
  result_a1 <- result_b1 <- result_c <- result_ind1 <- matrix(ncol = 7)
  colnames(result_a1) <- colnames(result_b1) <- colnames(result_c) <- colnames(result_ind1) <- c("rxmu","label", "est","se","ci.lower","ci.upper","est.omit")
  for (i in -900 : 900) {
    rxmu <- i/1000
    #calculate parameters from correlations
    a1 <- cal_a1(rxmo,rmomu,rxmu)
    b1 <- cal_b1(rxmo,rxmu,rmomu,rxy,rymo,rymu)
    c <- cal_c(rxmo,rxmu,rmomu,rxy,rymo,rymu)
    a2 <- cal_a2(rxmu)
    b2 <- cal_b2(rxmo,rxmu,rmomu,rxy,rymo,rymu)
    k <- cal_k(rxmo,rxmu,rmomu)
    z <- stats::qnorm((1-conflevel)/2)
    #calculate and record the new estimate, se and confidence interval if the standardization constraint is fulfilled
    if (constrain_standardized(a1,a2,b1,b2,k,c)) {
      ind1 <- a1*b1
      a1_omit <- a1 + cal_bias_a1(rxmo,rxmu,rmomu)
      b1_omit <- b1 + cal_bias_b1(rxmo,rxmu,rmomu,rxy,rymo,rymu)
      c_omit <- c + cal_bias_c(rxmo,rxmu,rmomu,rxy,rymo,rymu)
      ind1_omit <- a1_omit * b1_omit
      var.e.mo <- 1 - k^2 - a1^2 - 2 * k * a1 * rxmu
      var.e.mu <- 1 - a2^2
      var.e.y <- 1 - b1^2 - b2^2 - c^2 - 2 * b1 * b2 * rmomu - 2 * b1 * c * rxmo - 2 * b2 * c * rxmu
      xx_mo <- matrix(c(1,rxmu,rxmu,1),2,2)
      xx_y <- matrix(c(1,rmomu,rxmo,rmomu,1,rxmu,rxmo,rxmu,1),3,3)
      se_a1 <- sqrt(solve(nobs*xx_mo)[2,2]*var.e.mo)
      se_b1 <- sqrt(solve(nobs*xx_y)[1,1]*var.e.y)
      se_ind1 <- sqrt(b1^2 * (se_a1^2) + a1^2 * (se_b1^2))
      se_c <- sqrt(solve(nobs*xx_y)[3,3]*var.e.y)
      result_a1 <- rbind(result_a1,c(rxmu,"a1", a1, se_a1, a1+z*se_a1, a1-z*se_a1, a1_omit))
      result_b1 <- rbind(result_b1,c(rxmu,"b1", b1, se_b1, b1+z*se_b1, b1-z*se_b1, b1_omit))
      result_c <- rbind(result_c,c(rxmu,"c", c, se_c, c+z*se_c, c-z*se_c, c_omit))
      result_ind1 <- rbind(result_ind1, c(rxmu,"indirect1", ind1, se_ind1, ind1+z*se_ind1, ind1-z*se_ind1, ind1_omit))
    }
  }
  result_a1 <- as.data.frame(result_a1[2:nrow(result_a1),])
  result_b1 <- as.data.frame(result_b1[2:nrow(result_b1),])
  result_c <- as.data.frame(result_c[2:nrow(result_c),])
  result_ind1 <- as.data.frame(result_ind1[2:nrow(result_ind1),])
  result <- rbind(result_a1,result_b1,result_c,result_ind1)
  result$label <- as.factor(result$label)
  result$rxmu <- as.numeric(as.character(result$rxmu))
  result$est <- as.numeric(as.character(result$est))
  result$se <- as.numeric(as.character(result$se))
  result$ci.lower <- as.numeric(as.character(result$ci.lower))
  result$ci.upper <- as.numeric(as.character(result$ci.upper))
  result$est.omit <- as.numeric(as.character(result$est.omit))
  return(result)
}


#' how the estimate and confidence interval change with regards to correlation between Mo and Mu
#'
#' give the result for how the estimate and confidence interval change with regards to correlation between Mo and Mu
#'
#' @param rxmo correlation between X and Mo
#' @param rxy correlation between X and Y
#' @param rymo correlation between Y and Mo
#' @param rxmu correlation between X and Mu
#' @param rymu correlation between Y and Mu
#' @param nobs number of observations in the sample or sample size
#' @param conflevel confidence interval level, default is 0.95
#'
#' @return result for how rmomu affects the estimates
#' @import stats
#' @export
rmomu_data <- function(rxmo=-2,rxy=-2,rymo=-2,rxmu=-2,rymu=-2, nobs=0, conflevel=0.95){
  #check input
  if (!(check_input(rxmo) && check_input(rxy) && check_input(rymo) && check_input(rxmu) && check_input(rymu))) {
    stop("Error: input is problematic! Correlations should be among -1 and 1.")
  }
  if (nobs <= 0 ) {
    stop("Error: input is problematic! Please give positive numbers for sample size!")
  }
  result_a1 <- result_b1 <- result_c <- result_ind1 <- matrix(ncol = 7)
  colnames(result_a1) <- colnames(result_b1) <- colnames(result_c) <- colnames(result_ind1) <- c("rmomu","label", "est","se","ci.lower","ci.upper", "est.omit")
  for (i in -900 : 900) {
    rmomu <- i/1000
    #calculate parameters from correlations
    a1 <- cal_a1(rxmo,rmomu,rxmu)
    b1 <- cal_b1(rxmo,rxmu,rmomu,rxy,rymo,rymu)
    c <- cal_c(rxmo,rxmu,rmomu,rxy,rymo,rymu)
    a2 <- cal_a2(rxmu)
    b2 <- cal_b2(rxmo,rxmu,rmomu,rxy,rymo,rymu)
    k <- cal_k(rxmo,rxmu,rmomu)
    z <- stats::qnorm((1-conflevel)/2)
    #calculate and record the new estimate, se and confidence interval if the standardization constraint is fulfilled
    if (constrain_standardized(a1,a2,b1,b2,k,c)) {
      ind1 <- a1*b1
      a1_omit <- a1 + cal_bias_a1(rxmo,rxmu,rmomu)
      b1_omit <- b1 + cal_bias_b1(rxmo,rxmu,rmomu,rxy,rymo,rymu)
      c_omit <- c + cal_bias_c(rxmo,rxmu,rmomu,rxy,rymo,rymu)
      ind1_omit <- a1_omit * b1_omit
      var.e.mo <- 1 - k^2 - a1^2 - 2 * k * a1 * rxmu
      var.e.mu <- 1 - a2^2
      var.e.y <- 1 - b1^2 - b2^2 - c^2 - 2 * b1 * b2 * rmomu - 2 * b1 * c * rxmo - 2 * b2 * c * rxmu
      xx_mo <- matrix(c(1,rxmu,rxmu,1),2,2)
      xx_y <- matrix(c(1,rmomu,rxmo,rmomu,1,rxmu,rxmo,rxmu,1),3,3)
      se_a1 <- sqrt(solve(nobs*xx_mo)[2,2]*var.e.mo)
      se_b1 <- sqrt(solve(nobs*xx_y)[1,1]*var.e.y)
      se_ind1 <- sqrt(b1^2 * (se_a1^2) + a1^2 * (se_b1^2))
      se_c <- sqrt(solve(nobs*xx_y)[3,3]*var.e.y)
      result_a1 <- rbind(result_a1,c(rmomu,"a1", a1, se_a1, a1+z*se_a1, a1-z*se_a1, a1_omit))
      result_b1 <- rbind(result_b1,c(rmomu,"b1", b1, se_b1, b1+z*se_b1, b1-z*se_b1, b1_omit))
      result_c <- rbind(result_c,c(rmomu,"c", c, se_c, c+z*se_c, c-z*se_c, c_omit))
      result_ind1 <- rbind(result_ind1, c(rmomu,"indirect1", ind1, se_ind1, ind1+z*se_ind1, ind1-z*se_ind1, ind1_omit))
    }
  }
  result_a1 <- as.data.frame(result_a1[2:nrow(result_a1),])
  result_b1 <- as.data.frame(result_b1[2:nrow(result_b1),])
  result_c <- as.data.frame(result_c[2:nrow(result_c),])
  result_ind1 <- as.data.frame(result_ind1[2:nrow(result_ind1),])
  result <- rbind(result_a1,result_b1,result_c,result_ind1)
  result$label <- as.factor(result$label)
  result$rmomu <- as.numeric(as.character(result$rmomu))
  result$est <- as.numeric(as.character(result$est))
  result$se <- as.numeric(as.character(result$se))
  result$ci.lower <- as.numeric(as.character(result$ci.lower))
  result$ci.upper <- as.numeric(as.character(result$ci.upper))
  result$est.omit <- as.numeric(as.character(result$est.omit))
  return(result)
}



#' how the estimate and confidence interval change with regards to correlation between Y and Mu
#'
#' give the result for how the estimate and confidence interval change with regards to correlation between Y and Mu
#'
#' @param rxmo correlation between X and Mo
#' @param rxy correlation between X and Y
#' @param rymo correlation between Y and Mo
#' @param rxmu correlation between X and Mu
#' @param rmomu correlation between Mo and Mu
#' @param nobs number of observations in the sample or sample size
#' @param conflevel confidence interval level, default is 0.95
#'
#' @return result for how rymu affects the estimates
#' @import stats
#' @export
rymu_data <- function(rxmo=-2,rxy=-2,rymo=-2,rxmu=-2,rmomu=-2, nobs=0, conflevel=0.95){
  #check input
  if (!(check_input(rxmo) && check_input(rxy) && check_input(rymo) && check_input(rxmu) && check_input(rmomu)))
    {stop("Error: input is problematic! Correlations should be among -1 and 1.")}
  if (nobs <= 0 )
    {stop("Error: input is problematic! Please give positive numbers for sample size!")}
  result_a1 <- result_b1 <- result_c <- result_ind1 <- matrix(ncol = 7)
  colnames(result_a1) <- colnames(result_b1) <- colnames(result_c) <- colnames(result_ind1) <- c("rymu","label", "est","se","ci.lower","ci.upper", "est.omit")
  for (i in -900 : 900) {
    rymu <- i/1000
    #calculate parameters from correlations
    a1 <- cal_a1(rxmo,rmomu,rxmu)
    b1 <- cal_b1(rxmo,rxmu,rmomu,rxy,rymo,rymu)
    c <- cal_c(rxmo,rxmu,rmomu,rxy,rymo,rymu)
    a2 <- cal_a2(rxmu)
    b2 <- cal_b2(rxmo,rxmu,rmomu,rxy,rymo,rymu)
    k <- cal_k(rxmo,rxmu,rmomu)
    z <- stats::qnorm((1-conflevel)/2)
    #calculate and record the new estimate, se and confidence interval if the standardization constraint is fulfilled
    if (constrain_standardized(a1,a2,b1,b2,k,c)) {
      ind1 <- a1*b1
      a1_omit <- a1 + cal_bias_a1(rxmo,rxmu,rmomu)
      b1_omit <- b1 + cal_bias_b1(rxmo,rxmu,rmomu,rxy,rymo,rymu)
      c_omit <- c + cal_bias_c(rxmo,rxmu,rmomu,rxy,rymo,rymu)
      ind1_omit <- a1_omit * b1_omit
      var.e.mo <- 1 - k^2 - a1^2 - 2 * k * a1 * rxmu
      var.e.mu <- 1 - a2^2
      var.e.y <- 1 - b1^2 - b2^2 - c^2 - 2 * b1 * b2 * rmomu - 2 * b1 * c * rxmo - 2 * b2 * c * rxmu
      xx_mo <- matrix(c(1,rxmu,rxmu,1),2,2)
      xx_y <- matrix(c(1,rmomu,rxmo,rmomu,1,rxmu,rxmo,rxmu,1),3,3)
      se_a1 <- sqrt(solve(nobs*xx_mo)[2,2]*var.e.mo)
      se_b1 <- sqrt(solve(nobs*xx_y)[1,1]*var.e.y)
      se_ind1 <- sqrt(b1^2 * (se_a1^2) + a1^2 * (se_b1^2))
      se_c <- sqrt(solve(nobs*xx_y)[3,3]*var.e.y)
      result_a1 <- rbind(result_a1,c(rymu,"a1", a1, se_a1, a1+z*se_a1, a1-z*se_a1, a1_omit))
      result_b1 <- rbind(result_b1,c(rymu,"b1", b1, se_b1, b1+z*se_b1, b1-z*se_b1, b1_omit))
      result_c <- rbind(result_c,c(rymu,"c", c, se_c, c+z*se_c, c-z*se_c, c_omit))
      result_ind1 <- rbind(result_ind1, c(rymu,"indirect1", ind1, se_ind1, ind1+z*se_ind1, ind1-z*se_ind1, ind1_omit))
    }
  }
  result_a1 <- as.data.frame(result_a1[2:nrow(result_a1),])
  result_b1 <- as.data.frame(result_b1[2:nrow(result_b1),])
  result_c <- as.data.frame(result_c[2:nrow(result_c),])
  result_ind1 <- as.data.frame(result_ind1[2:nrow(result_ind1),])
  result <- rbind(result_a1,result_b1,result_c,result_ind1)
  result$label <- as.factor(result$label)
  result$rymu <- as.numeric(as.character(result$rymu))
  result$est <- as.numeric(as.character(result$est))
  result$se <- as.numeric(as.character(result$se))
  result$ci.lower <- as.numeric(as.character(result$ci.lower))
  result$ci.upper <- as.numeric(as.character(result$ci.upper))
  result$est.omit <- as.numeric(as.character(result$est.omit))
  return(result)
}

