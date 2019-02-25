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
#' @export
rxmu <- function(rxmo=-2,rxy=-2,rymo=-2,rmomu=-2,rymu=-2){
  #check input
  if (!(check_input(rxmo) && check_input(rxy) && check_input(rymo) && check_input(rmomu) && check_input(rymu))) {
    return("Error: input is problematic!")
  }
  result_a1 <- result_b1 <- result_c <- matrix(ncol = 3)
  colnames(result_a1) <- colnames(result_b1) <- colnames(result_c) <- c("rxmu","Bias","Parameter")
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
      result_a1 <- rbind(result_a1,c(rxmu,bias_a1, "bias_a1"))
      result_b1 <- rbind(result_b1,c(rxmu,bias_b1,"bias_b1"))
      result_c <- rbind(result_c,c(rxmu,bias_c,"bias_c"))
    }
  }
  result_a1 <- as.data.frame(result_a1[2:nrow(result_a1),])
  result_b1 <- as.data.frame(result_b1[2:nrow(result_b1),])
  result_c <- as.data.frame(result_c[2:nrow(result_c),])
  result <- rbind(result_a1,result_b1,result_c)
  result$Parameter <- as.factor(result$Parameter)
  result$rxmu <- as.numeric(as.character(result$rxmu))
  result$Bias <- as.numeric(as.character(result$Bias))
  figure <- ggplot(result, (aes(rxmu, result$Bias, linetype = result$Parameter,colour = result$Parameter,shape = result$Parameter))) +
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
#' @export
rmomu <- function(rxmo=-2,rxy=-2,rymo=-2,rxmu=-2,rymu=-2){
  #check input
  if (!(check_input(rxmo) && check_input(rxy) && check_input(rymo) && check_input(rxmu) && check_input(rymu))) {
    return("Error: input is problematic!")
  }
  result_a1 <- result_b1 <- result_c <- matrix(ncol = 3)
  colnames(result_a1) <- colnames(result_b1) <- colnames(result_c) <- c("rmomu","Bias","Parameter")
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
      result_a1 <- rbind(result_a1,c(rmomu,bias_a1, "bias_a1"))
      result_b1 <- rbind(result_b1,c(rmomu,bias_b1,"bias_b1"))
      result_c <- rbind(result_c,c(rmomu,bias_c,"bias_c"))
    }
  }
  result_a1 <- as.data.frame(result_a1[2:nrow(result_a1),])
  result_b1 <- as.data.frame(result_b1[2:nrow(result_b1),])
  result_c <- as.data.frame(result_c[2:nrow(result_c),])
  result <- rbind(result_a1,result_b1,result_c)
  result$Parameter <- as.factor(result$Parameter)
  result$rmomu <- as.numeric(as.character(result$rmomu))
  result$Bias <- as.numeric(as.character(result$Bias))
  figure <- ggplot(result, (aes(rmomu,result$Bias, linetype = result$Parameter,colour = result$Parameter,shape = result$Parameter))) +
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
#' @export
rymu <- function(rxmo=-2,rxy=-2,rymo=-2,rxmu=-2,rmomu=-2){
  #check input
  if (!(check_input(rxmo) && check_input(rxy) && check_input(rymo) && check_input(rxmu) && check_input(rmomu))) {
    return("Error: input is problematic!")
  }
  result_a1 <- result_b1 <- result_c <- matrix(ncol = 3)
  colnames(result_a1) <- colnames(result_b1) <- colnames(result_c) <- c("rymu","Bias","Parameter")
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
      result_a1 <- rbind(result_a1,c(rymu,bias_a1, "bias_a1"))
      result_b1 <- rbind(result_b1,c(rymu,bias_b1,"bias_b1"))
      result_c <- rbind(result_c,c(rymu,bias_c,"bias_c"))
    }
  }
  result_a1 <- as.data.frame(result_a1[2:nrow(result_a1),])
  result_b1 <- as.data.frame(result_b1[2:nrow(result_b1),])
  result_c <- as.data.frame(result_c[2:nrow(result_c),])
  result <- rbind(result_a1,result_b1,result_c)
  result$Parameter <- as.factor(result$Parameter)
  result$rymu <- as.numeric(as.character(result$rymu))
  result$Bias <- as.numeric(as.character(result$Bias))
  figure <- ggplot(result, (aes(rymu, result$Bias, linetype = result$Parameter,colour = result$Parameter,shape = result$Parameter))) +
    geom_line() +
    geom_point() +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1))+
    scale_colour_discrete("") +
    scale_linetype_manual("", values=c(1,2,3)) +
    scale_shape_manual("", values=c(16,17,18)) +
    labs(y="Percent of bias")
  return(figure)
}

