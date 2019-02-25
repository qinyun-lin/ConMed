#' sensitivity plot for cross-sectional percentage of bias with regards to correlation between X and Mu
#'
#' give the sensitivity plot for how the correlation between X and Mu affects the percentage of bias in cross-sectional model
#'
#' @param rxmo correlation between X and Mo
#' @param rxy correlation between X and Y
#' @param rymo correlation between Y and Mo
#' @param rmomu correlation between Mo and Mu
#' @param rymu correlation between Y and Mu
#'
#' @return plot for how rxmu affects the percentage of bias in cross-sectional model
#' @import ggplot2
#' @import scales
#' @import tidyr
#' @export
rxmu <- function(rxmo=-2,rxy=-2,rymo=-2,rmomu=-2,rymu=-2){
  #check input
  if (!(check_input(rxmo) && check_input(rxy) && check_input(rymo) && check_input(rmomu) && check_input(rymu))) {
    return("Error: input is problematic!")
  }
  result <- matrix(ncol=4)
  colnames(result) <- c("Bias_a1","Bias_b1","Bias_c","rxmu")
  #return("Normal")
  for (i in -22.5 : 22.5) {
    rxmu <- i/25
    #calculate parameters from correlations
    a1 <- cal_a1(rxmo,rmomu,rxmu)
    b1 <- cal_b1(rxmo,rxmu,rmomu,rxy,rymo,rymu)
    c <- cal_c(rxmo,rxmu,rmomu,rxy,rymo,rymu)
    a2 <- cal_a2(rxmu)
    b2 <- cal_b2(rxmo,rxmu,rmomu,rxy,rymo,rymu)
    k <- cal_k(rxmo,rxmu,rmomu)
    #calculate the percentage of bias if the standardization constraint is fulfilled
    if (constrain_standardized(a1,a2,b1,b2,k,c)) {
      bias_a1 <- cal_bias_a1_perc(rxmo,rxmu,rmomu)
      bias_b1 <- cal_bias_b1_perc(rxmo,rxmu,rmomu,rxy,rymo,rymu)
      bias_c <- cal_bias_c_perc(rxmo,rxmu,rmomu,rxy,rymo,rymu)
      result <- rbind(result,c(bias_a1,bias_b1,bias_c,rxmu))
    }
  }
  result.wide <- as.data.frame(result[2:nrow(result),])
  result.long <- gather(result.wide,Parameter,perc_bias,Bias_a1:Bias_c,factor_key = TRUE)
  figure <- ggplot(result.long, (aes(rxmu, perc_bias, linetype = Parameter,colour = Parameter,shape = Parameter))) +
    geom_line() +
    geom_point() +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1))+
    scale_colour_discrete("") +
    scale_linetype_manual("", values=c(1,2,3)) +
    scale_shape_manual("", values=c(16,17,18)) +
    labs(y="Percent of bias")
  return(figure)
}

#' sensitivity plot for cross-sectional percentage of bias with regards to correlation between Mo and Mu
#'
#' give the sensitivity plot for how the correlation between Mo and Mu affects the percentage of bias in cross-sectional model
#'
#' @param rxmo correlation between X and Mo
#' @param rxy correlation between X and Y
#' @param rymo correlation between Y and Mo
#' @param rxmu correlation between X and Mu
#' @param rymu correlation between Y and Mu
#'
#' @return plot for how rmomu affects the percentage of bias in cross-sectional model
#' @import ggplot2
#' @import scales
#' @import tidyr
#' @export
rmomu <- function(rxmo=-2,rxy=-2,rymo=-2,rxmu=-2,rymu=-2){
  #check input
  if (!(check_input(rxmo) && check_input(rxy) && check_input(rymo) && check_input(rxmu) && check_input(rymu))) {
    return("Error: input is problematic!")
  }
  result <- matrix(ncol=4)
  colnames(result) <- c("Bias_a1","Bias_b1","Bias_c","rmomu")
  #return("Normal")
  for (i in -22.5 : 22.5) {
    rmomu <- i/25
    #calculate parameters from correlations
    a1 <- cal_a1(rxmo,rmomu,rxmu)
    b1 <- cal_b1(rxmo,rxmu,rmomu,rxy,rymo,rymu)
    c <- cal_c(rxmo,rxmu,rmomu,rxy,rymo,rymu)
    a2 <- cal_a2(rxmu)
    b2 <- cal_b2(rxmo,rxmu,rmomu,rxy,rymo,rymu)
    k <- cal_k(rxmo,rxmu,rmomu)
    #calculate the percentage of bias if the standardization constraint is fulfilled
    if (constrain_standardized(a1,a2,b1,b2,k,c)) {
      bias_a1 <- cal_bias_a1_perc(rxmo,rxmu,rmomu)
      bias_b1 <- cal_bias_b1_perc(rxmo,rxmu,rmomu,rxy,rymo,rymu)
      bias_c <- cal_bias_c_perc(rxmo,rxmu,rmomu,rxy,rymo,rymu)
      result <- rbind(result,c(bias_a1,bias_b1,bias_c,rmomu))
    }
  }
  result.wide <- as.data.frame(result[2:nrow(result),])
  result.long <- gather(result.wide,Parameter,perc_bias,Bias_a1:Bias_c,factor_key = TRUE)
  figure <- ggplot(result.long, (aes(rmomu,perc_bias, linetype = Parameter,colour = Parameter,shape = Parameter))) +
    geom_line() +
    geom_point() +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1))+
    scale_colour_discrete("") +
    scale_linetype_manual("", values=c(1,2,3)) +
    scale_shape_manual("", values=c(16,17,18)) +
    labs(y="Percent of bias")
  return(figure)
}

#' sensitivity plot for cross-sectional percentage of bias with regards to correlation between Y and Mu
#'
#' give the sensitivity plot for how the correlation between Y and Mu affects the percentage of bias in cross-sectional model
#'
#' @param rxmo correlation between X and Mo
#' @param rxy correlation between X and Y
#' @param rymo correlation between Y and Mo
#' @param rxmu correlation between X and Mu
#' @param rmomu correlation between Mo and Mu
#'
#' @return plot for how rmomu affects the percentage of bias in cross-sectional model
#' @import ggplot2
#' @import scales
#' @import tidyr
#' @export
rymu <- function(rxmo=-2,rxy=-2,rymo=-2,rxmu=-2,rmomu=-2){
  #check input
  if (!(check_input(rxmo) && check_input(rxy) && check_input(rymo) && check_input(rxmu) && check_input(rmomu))) {
    return("Error: input is problematic!")
  }
  result <- matrix(ncol=4)
  colnames(result) <- c("Bias_a1","Bias_b1","Bias_c","rymu")
  #return("Normal")
  for (i in -22.5 : 22.5) {
    rymu <- i/25
    #calculate parameters from correlations
    a1 <- cal_a1(rxmo,rmomu,rxmu)
    b1 <- cal_b1(rxmo,rxmu,rmomu,rxy,rymo,rymu)
    c <- cal_c(rxmo,rxmu,rmomu,rxy,rymo,rymu)
    a2 <- cal_a2(rxmu)
    b2 <- cal_b2(rxmo,rxmu,rmomu,rxy,rymo,rymu)
    k <- cal_k(rxmo,rxmu,rmomu)
    #calculate the percentage of bias if the standardization constraint is fulfilled
    if (constrain_standardized(a1,a2,b1,b2,k,c)) {
      bias_a1 <- cal_bias_a1_perc(rxmo,rxmu,rmomu)
      bias_b1 <- cal_bias_b1_perc(rxmo,rxmu,rmomu,rxy,rymo,rymu)
      bias_c <- cal_bias_c_perc(rxmo,rxmu,rmomu,rxy,rymo,rymu)
      result <- rbind(result,c(bias_a1,bias_b1,bias_c,rymu))
    }
  }
  result.wide <- as.data.frame(result[2:nrow(result),])
  result.long <- gather(result.wide,Parameter,perc_bias,Bias_a1:Bias_c,factor_key = TRUE)
  figure <- ggplot(result.long, (aes(rymu, perc_bias, linetype = Parameter,colour = Parameter,shape = Parameter))) +
    geom_line() +
    geom_point() +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1))+
    scale_colour_discrete("") +
    scale_linetype_manual("", values=c(1,2,3)) +
    scale_shape_manual("", values=c(16,17,18)) +
    labs(y="Percent of bias")
  return(figure)
}

