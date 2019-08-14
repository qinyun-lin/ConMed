#' sensitivity plot for estimates with regards to correlation between X and Mu
#'
#' give the sensitivity plot for how the correlation between X and Mu affects the estimates and confidence interval
#'
#' @param rxmo correlation between X and Mo
#' @param rxy correlation between X and Y
#' @param rymo correlation between Y and Mo
#' @param rmomu correlation between Mo and Mu
#' @param rymu correlation between Y and Mu
#' @param nobs number of observations in the sample or sample size
#' @param labelest label for which estimate the figure is for: indirect1, a1, b1 or c, default is indirect1
#' @param conflevel confidence interval level, default is 0.95
#'
#' @return plot for how rxmu affects the estimates and confidence interval
#' @import ggplot2
#' @import scales
#' @export
rxmu_plot <- function(rxmo=-2,rxy=-2,rymo=-2,rmomu=-2,rymu=-2, nobs=0, labelest = "indirect1", conflevel=0.95){
  result <- rxmu_data(rxmo, rxy, rymo, rmomu, rymu, nobs, conflevel)
  if (labelest == "indirect1") {
    result_ind1 <- result[result$label=="indirect1",]
    figure <- ggplot(result_ind1, aes_string(x='rxmu', y='est'))+
      geom_point()+
      geom_line()+
      geom_ribbon(data=result_ind1,aes_string(ymin='ci.lower',ymax='ci.upper'),alpha=0.3) +
      geom_hline(yintercept=result_ind1$est.omit,linetype="dashed") +
      labs(y="Estimate and confidence interval for indirect effect")
  }
  if (labelest == "a1") {
    result_a1 <- result[result$label=="a1",]
    figure <- ggplot(result_a1, aes_string(x='rxmu', y='est'))+
      geom_point()+
      geom_line()+
      geom_ribbon(data=result_a1,aes_string(ymin='ci.lower',ymax='ci.upper'),alpha=0.3) +
      geom_hline(yintercept=result_a1$est.omit,linetype="dashed") +
      labs(y="Estimate and confidence interval for a")
  }
  if (labelest == "b1") {
    result_b1 <- result[result$label=="b1",]
    figure <- ggplot(result_b1, aes_string(x='rxmu', y='est'))+
      geom_point()+
      geom_line()+
      geom_ribbon(data=result_b1,aes_string(ymin='ci.lower',ymax='ci.upper'),alpha=0.3) +
      geom_hline(yintercept=result_b1$est.omit,linetype="dashed") +
      labs(y="Estimate and confidence interval for b")
  }
  if (labelest == "c") {
    result_c <- result[result$label=="c",]
    figure <- ggplot(result_c, aes_string(x='rxmu', y='est'))+
      geom_point()+
      geom_line()+
      geom_ribbon(data=result_c,aes_string(ymin='ci.lower',ymax='ci.upper'),alpha=0.3) +
      geom_hline(yintercept=result_c$est.omit,linetype="dashed") +
      labs(y="Estimate and confidence interval for direct effect")
  }
  return(figure)
}

#' sensitivity plot for estimates and confidence interval with regards to correlation between Mo and Mu
#'
#' give the sensitivity plot for how the correlation between Mo and Mu affects the estimates and confidence interval
#'
#' @param rxmo correlation between X and Mo
#' @param rxy correlation between X and Y
#' @param rymo correlation between Y and Mo
#' @param rxmu correlation between X and Mu
#' @param rymu correlation between Y and Mu
#' @param nobs number of observations in the sample or sample size
#' @param labelest label for which estimate the figure is for: indirect1, a1, b1 or c, default is indirect1
#' @param conflevel confidence interval level, default is 0.95
#'
#' @return plot for how rmomu affects the estimates and confidence interval
#' @import ggplot2
#' @import scales
#' @export
rmomu_plot <- function(rxmo=-2,rxy=-2,rymo=-2,rxmu=-2,rymu=-2, nobs=0, labelest = "indirect1", conflevel=0.95){
  result <- rmomu_data(rxmo, rxy, rymo, rxmu, rymu, nobs, conflevel)
  if (labelest == "indirect1") {
    result_ind1 <- result[result$label=="indirect1",]
    figure <- ggplot(result_ind1, aes_string(x='rmomu', y='est'))+
      geom_point()+
      geom_line()+
      geom_ribbon(data=result_ind1,aes_string(ymin='ci.lower',ymax='ci.upper'),alpha=0.3) +
      geom_hline(yintercept=result_ind1$est.omit,linetype="dashed") +
      labs(y="Estimate and confidence interval for indirect effect")
  }
  if (labelest == "a1") {
    result_a1 <- result[result$label=="a1",]
    figure <- ggplot(result_a1, aes_string(x='rmomu', y='est'))+
      geom_point()+
      geom_line()+
      geom_ribbon(data=result_a1,aes_string(ymin='ci.lower',ymax='ci.upper'),alpha=0.3) +
      geom_hline(yintercept=result_a1$est.omit,linetype="dashed") +
      labs(y="Estimate and confidence interval for a")
  }
  if (labelest == "b1") {
    result_b1 <- result[result$label=="b1",]
    figure <- ggplot(result_b1, aes_string(x='rmomu', y='est'))+
      geom_point()+
      geom_line()+
      geom_ribbon(data=result_b1,aes_string(ymin='ci.lower',ymax='ci.upper'),alpha=0.3) +
      geom_hline(yintercept=result_b1$est.omit,linetype="dashed") +
      labs(y="Estimate and confidence interval for b")
  }
  if (labelest == "c") {
    result_c <- result[result$label=="c",]
    figure <- ggplot(result_c, aes_string(x='rmomu', y='est'))+
      geom_point()+
      geom_line()+
      geom_ribbon(data=result_c,aes_string(ymin='ci.lower',ymax='ci.upper'),alpha=0.3) +
      geom_hline(yintercept=result_c$est.omit,linetype="dashed") +
      labs(y="Estimate and confidence interval for direct effect")
  }
  return(figure)
}

#' sensitivity plot for estimates and confidence interval with regards to correlation between Y and Mu
#'
#' give the sensitivity plot for how the correlation between Y and Mu affects the estimates and confidence interval
#'
#' @param rxmo correlation between X and Mo
#' @param rxy correlation between X and Y
#' @param rymo correlation between Y and Mo
#' @param rxmu correlation between X and Mu
#' @param rmomu correlation between Mo and Mu
#' @param nobs number of observations in the sample or sample size
#' @param labelest label for which estimate the figure is for: indirect1, a1, b1 or c, default is indirect1
#' @param conflevel confidence interval level, default is 0.95
#'
#' @return plot for how rymu affects the percentage of bias
#' @import ggplot2
#' @import scales
#' @export
rymu_plot <- function(rxmo=-2, rxy=-2, rymo=-2, rxmu=-2, rmomu=-2, nobs=0, labelest = "indirect1", conflevel=0.95){
  result <- rymu_data(rxmo, rxy, rymo, rxmu, rmomu, nobs, conflevel)
  if (labelest == "indirect1") {
    result_ind1 <- result[result$label=="indirect1",]
    figure <- ggplot(result_ind1, aes_string(x='rymu', y='est'))+
      geom_point()+
      geom_line()+
      geom_ribbon(data=result_ind1,aes_string(ymin='ci.lower',ymax='ci.upper'),alpha=0.3) +
      geom_hline(yintercept=result_ind1$est.omit,linetype="dashed") +
      labs(y="Estimate and confidence interval for indirect effect")
  }
  if (labelest == "a1") {
    result_a1 <- result[result$label=="a1",]
    figure <- ggplot(result_a1, aes_string(x='rymu', y='est'))+
      geom_point()+
      geom_line()+
      geom_ribbon(data=result_a1,aes_string(ymin='ci.lower',ymax='ci.upper'),alpha=0.3) +
      geom_hline(yintercept=result_a1$est.omit,linetype="dashed") +
      labs(y="Estimate and confidence interval for a")
  }
  if (labelest == "b1") {
    result_b1 <- result[result$label=="b1",]
    figure <- ggplot(result_b1, aes_string(x='rymu', y='est'))+
      geom_point()+
      geom_line()+
      geom_ribbon(data=result_b1,aes_string(ymin='ci.lower',ymax='ci.upper'),alpha=0.3) +
      geom_hline(yintercept=result_b1$est.omit,linetype="dashed") +
      labs(y="Estimate and confidence interval for b")
  }
  if (labelest == "c") {
    result_c <- result[result$label=="c",]
    figure <- ggplot(result_c, aes_string(x='rymu', y='est'))+
      geom_point()+
      geom_line()+
      geom_ribbon(data=result_c,aes_string(ymin='ci.lower',ymax='ci.upper'),alpha=0.3) +
      geom_hline(yintercept=result_c$est.omit,linetype="dashed") +
      labs(y="Estimate and confidence interval for direct effect")
  }
  return(figure)
}

