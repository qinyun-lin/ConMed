#' auxillary function for sensitivity plot for estimates with regards to correlation between X and Mu
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
#' @return plot for how rxmu affects the estimates and confidence interval
rxmu_plot_auxi <- function(rxmo=-2,rxy=-2,rymo=-2,rmomu=-2,rymu=-2, nobs=0, labelest = "indirect1", conflevel=0.95){
  result <- rxmu_data(rxmo, rxy, rymo, rmomu, rymu, nobs, conflevel)
  if (labelest == "indirect1") {
    result_ind1 <- result[result$label=="indirect1",]
    figure <- ggplot2::ggplot(result_ind1, ggplot2::aes_string(x='rxmu', y='est'))+
      ggplot2::geom_point()+
      ggplot2::geom_line()+
      ggplot2::geom_ribbon(data=result_ind1,ggplot2::aes_string(ymin='ci.lower',ymax='ci.upper'),alpha=0.3) +
      ggplot2::geom_hline(yintercept=result_ind1$est.omit,linetype="dashed") +
      ggplot2::labs(y="indirect effect")
  }
  if (labelest == "a1") {
    result_a1 <- result[result$label=="a1",]
    figure <- ggplot2::ggplot(result_a1, ggplot2::aes_string(x='rxmu', y='est'))+
      ggplot2::geom_point()+
      ggplot2::geom_line()+
      ggplot2::geom_ribbon(data=result_a1,ggplot2::aes_string(ymin='ci.lower',ymax='ci.upper'),alpha=0.3) +
      ggplot2::geom_hline(yintercept=result_a1$est.omit,linetype="dashed") +
      ggplot2::labs(y="a")
  }
  if (labelest == "b1") {
    result_b1 <- result[result$label=="b1",]
    figure <- ggplot2::ggplot(result_b1, ggplot2::aes_string(x='rxmu', y='est'))+
      ggplot2::geom_point()+
      ggplot2::geom_line()+
      ggplot2::geom_ribbon(data=result_b1,ggplot2::aes_string(ymin='ci.lower',ymax='ci.upper'),alpha=0.3) +
      ggplot2::geom_hline(yintercept=result_b1$est.omit,linetype="dashed") +
      ggplot2::labs(y="b")
  }
  if (labelest == "c") {
    result_c <- result[result$label=="c",]
    figure <- ggplot2::ggplot(result_c, ggplot2::aes_string(x='rxmu', y='est'))+
      ggplot2::geom_point()+
      ggplot2::geom_line()+
      ggplot2::geom_ribbon(data=result_c,ggplot2::aes_string(ymin='ci.lower',ymax='ci.upper'),alpha=0.3) +
      ggplot2::geom_hline(yintercept=result_c$est.omit,linetype="dashed") +
      ggplot2::labs(y="direct effect c")
  }
  if (labelest == "a1b1indirect1") {
    result_a1b1ind <- result[result$label!="c",]
    figure <- ggplot2::ggplot(result_a1b1ind[c(T,rep(F,40)),],
                              ggplot2::aes_string(x='rxmu', y='est', lty='label', shape='label', color='label')) +
      ggplot2::geom_point(size=1.7)+
      ggplot2::geom_line(size=0.6)+
      ggplot2::geom_ribbon(data=result[result$label=="indirect1",],ggplot2::aes_string(ymin='ci.lower',ymax='ci.upper'),alpha=0.3) +
      ggplot2::geom_hline(yintercept=result[result$label=="indirect1",]$est.omit,linetype="dashed") +
      ggplot2::labs(y="indirect effect")
  }
  return(figure)
}

#' sensitivity plot for estimates with regards to correlation between X and Mu
#'
#' give the sensitivity plot for how the correlation between X and Mu affects the estimates and confidence interval
#'
#' @param rxmo correlation between X and Mo
#' @param rxy correlation between X and Y
#' @param rymo correlation between Y and Mo
#' @param nobs number of observations in the sample or sample size
#' @param labelest label for which estimate the figure is for: indirect1, a1, b1 or c, default is indirect1
#' @param conflevel confidence interval level, default is 0.95
#' @param specifyunob whether the user wants to specify two unobserved correlations default is 0, meaning no
#' @param rmomu correlation between Mo and Mu
#' @param rymu correlation between Y and Mu
#' @return plot for how rxmu affects the estimates and confidence interval
#' @export
rxmu_plot <- function(rxmo=-2,rxy=-2,rymo=-2,nobs=nobs,labelest = "indirect1", conflevel=0.95,specifyunob = 0, rmomu=-2,rymu=-2){
  if (specifyunob == 0) {
    plow <- rxmu_plot_auxi(rxmo=rxmo,rxy=rxy,rymo=rymo,rmomu=0.1,rymu=0.1, nobs=nobs, labelest = labelest, conflevel=conflevel)
    pmedium <- rxmu_plot_auxi(rxmo=rxmo,rxy=rxy,rymo=rymo,rmomu=0.3,rymu=0.3, nobs=nobs, labelest = labelest, conflevel=conflevel)
    phigh <- rxmu_plot_auxi(rxmo=rxmo,rxy=rxy,rymo=rymo,rmomu=0.5,rymu=0.5, nobs=nobs, labelest = labelest, conflevel=conflevel)
    plowhigh <- rxmu_plot_auxi(rxmo=rxmo,rxy=rxy,rymo=rymo,rmomu=0.1,rymu=0.5, nobs=nobs, labelest = labelest, conflevel=conflevel)
    phighlow <- rxmu_plot_auxi(rxmo=rxmo,rxy=rxy,rymo=rymo,rmomu=0.5,rymu=0.1, nobs=nobs, labelest = labelest, conflevel=conflevel)
    figure <- ggpubr::ggarrange(plow, pmedium, phigh, plowhigh, phighlow,
                                labels = c("Low", "Medium", "High", "Low rmomu & High rymu", "High rmomu & Low rymu"),
                                ncol = 1, nrow = 5)
  }
   if (specifyunob == 1) {
     figure <- rxmu_plot_auxi(rxmo=rxmo,rxy=rxy,rymo=rymo,rmomu=rmomu,rymu=rymu, nobs=nobs, labelest = labelest, conflevel=conflevel)
   }
  return(figure)
}

#' auxillary function for sensitivity plot for estimates and confidence interval with regards to correlation between Mo and Mu
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
rmomu_plot_auxi <- function(rxmo=-2,rxy=-2,rymo=-2,rxmu=-2,rymu=-2, nobs=0, labelest = "indirect1", conflevel=0.95){
  result <- rmomu_data(rxmo, rxy, rymo, rxmu, rymu, nobs, conflevel)
  if (labelest == "indirect1") {
    result_ind1 <- result[result$label=="indirect1",]
    figure <- ggplot2::ggplot(result_ind1, ggplot2::aes_string(x='rmomu', y='est'))+
      ggplot2::geom_point()+
      ggplot2::geom_line()+
      ggplot2::geom_ribbon(data=result_ind1,ggplot2::aes_string(ymin='ci.lower',ymax='ci.upper'),alpha=0.3) +
      ggplot2::geom_hline(yintercept=result_ind1$est.omit,linetype="dashed") +
      ggplot2::labs(y="indirect effect")
  }
  if (labelest == "a1") {
    result_a1 <- result[result$label=="a1",]
    figure <- ggplot2::ggplot(result_a1, ggplot2::aes_string(x='rmomu', y='est'))+
      ggplot2::geom_point()+
      ggplot2::geom_line()+
      ggplot2::geom_ribbon(data=result_a1,ggplot2::aes_string(ymin='ci.lower',ymax='ci.upper'),alpha=0.3) +
      ggplot2::geom_hline(yintercept=result_a1$est.omit,linetype="dashed") +
      ggplot2::labs(y="a")
  }
  if (labelest == "b1") {
    result_b1 <- result[result$label=="b1",]
    figure <- ggplot2::ggplot(result_b1, ggplot2::aes_string(x='rmomu', y='est'))+
      ggplot2::geom_point()+
      ggplot2::geom_line()+
      ggplot2::geom_ribbon(data=result_b1,ggplot2::aes_string(ymin='ci.lower',ymax='ci.upper'),alpha=0.3) +
      ggplot2::geom_hline(yintercept=result_b1$est.omit,linetype="dashed") +
      ggplot2::labs(y="b")
  }
  if (labelest == "c") {
    result_c <- result[result$label=="c",]
    figure <- ggplot2::ggplot(result_c, ggplot2::aes_string(x='rmomu', y='est'))+
      ggplot2::geom_point()+
      ggplot2::geom_line()+
      ggplot2::geom_ribbon(data=result_c,ggplot2::aes_string(ymin='ci.lower',ymax='ci.upper'),alpha=0.3) +
      ggplot2::geom_hline(yintercept=result_c$est.omit,linetype="dashed") +
      ggplot2::labs(y="direct effect c")
  }
  if (labelest == "a1b1indirect1") {
    result_a1b1ind <- result[result$label!="c",]
    figure <- ggplot2::ggplot(result_a1b1ind[c(T,rep(F,40)),],
                              ggplot2::aes_string(x='rmomu', y='est', lty='label', shape='label', color='label')) +
      ggplot2::geom_point(size=1.7)+
      ggplot2::geom_line(size=0.6)+
      ggplot2::geom_ribbon(data=result[result$label=="indirect1",],ggplot2::aes_string(ymin='ci.lower',ymax='ci.upper'),alpha=0.3) +
      ggplot2::geom_hline(yintercept=result[result$label=="indirect1",]$est.omit,linetype="dashed") +
      ggplot2::labs(y="indirect effect")
  }
  return(figure)
}

#' sensitivity plot for estimates with regards to correlation between Mo and Mu
#'
#' give the sensitivity plot for how the correlation between Mo and Mu affects the estimates and confidence interval
#'
#' @param rxmo correlation between X and Mo
#' @param rxy correlation between X and Y
#' @param rymo correlation between Y and Mo
#' @param nobs number of observations in the sample or sample size
#' @param labelest label for which estimate the figure is for: indirect1, a1, b1 or c, default is indirect1
#' @param conflevel confidence interval level, default is 0.95
#' @param specifyunob whether the user wants to specify two unobserved correlations default is 0, meaning no
#' @param rxmu correlation between X and Mu
#' @param rymu correlation between Y and Mu
#' @return plot for how rmomu affects the estimates and confidence interval
#' @export
rmomu_plot <- function(rxmo=-2,rxy=-2,rymo=-2,nobs=nobs,labelest = "indirect1", conflevel=0.95,specifyunob = 0, rxmu=-2,rymu=-2){
  if (specifyunob == 0) {
    plow <- rmomu_plot_auxi(rxmo=rxmo,rxy=rxy,rymo=rymo,rxmu=0.0995,rymu=0.1, nobs=nobs, labelest = labelest, conflevel=conflevel)
    pmedium <- rmomu_plot_auxi(rxmo=rxmo,rxy=rxy,rymo=rymo,rxmu=0.2425,rymu=0.3, nobs=nobs, labelest = labelest, conflevel=conflevel)
    phigh <- rmomu_plot_auxi(rxmo=rxmo,rxy=rxy,rymo=rymo,rxmu=0.3713,rymu=0.5, nobs=nobs, labelest = labelest, conflevel=conflevel)
    plowhigh <- rmomu_plot_auxi(rxmo=rxmo,rxy=rxy,rymo=rymo,rxmu=0.0995,rymu=0.5, nobs=nobs, labelest = labelest, conflevel=conflevel)
    phighlow <- rmomu_plot_auxi(rxmo=rxmo,rxy=rxy,rymo=rymo,rxmu=0.3713,rymu=0.1, nobs=nobs, labelest = labelest, conflevel=conflevel)
    figure <- ggpubr::ggarrange(plow, pmedium, phigh, plowhigh, phighlow,
                                labels = c("Low", "Medium", "High", "Low rxmu & High rymu", "High rxmu & Low rymu"),
                                ncol = 1, nrow = 5)
  }
  if (specifyunob == 1) {
    figure <- rmomu_plot_auxi(rxmo=rxmo,rxy=rxy,rymo=rymo,rxmu=rxmu,rymu=rymu, nobs=nobs, labelest = labelest, conflevel=conflevel)
  }
  return(figure)
}

#' auxillary function for sensitivity plot for estimates and confidence interval with regards to correlation between Y and Mu
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
#' @return plot for how rymu affects the percentage of bias
rymu_plot_auxi <- function(rxmo=-2, rxy=-2, rymo=-2, rxmu=-2, rmomu=-2, nobs=0, labelest = "indirect1", conflevel=0.95){
  result <- rymu_data(rxmo, rxy, rymo, rxmu, rmomu, nobs, conflevel)
  if (labelest == "indirect1") {
    result_ind1 <- result[result$label=="indirect1",]
    figure <- ggplot2::ggplot(result_ind1, ggplot2::aes_string(x='rymu', y='est'))+
      ggplot2::geom_point()+
      ggplot2::geom_line()+
      ggplot2::geom_ribbon(data=result_ind1,ggplot2::aes_string(ymin='ci.lower',ymax='ci.upper'),alpha=0.3) +
      ggplot2::geom_hline(yintercept=result_ind1$est.omit,linetype="dashed") +
      ggplot2::labs(y="indirect effect")
  }
  if (labelest == "a1") {
    result_a1 <- result[result$label=="a1",]
    figure <- ggplot2::ggplot(result_a1, ggplot2::aes_string(x='rymu', y='est'))+
      ggplot2::geom_point()+
      ggplot2::geom_line()+
      ggplot2::geom_ribbon(data=result_a1,ggplot2::aes_string(ymin='ci.lower',ymax='ci.upper'),alpha=0.3) +
      ggplot2::geom_hline(yintercept=result_a1$est.omit,linetype="dashed") +
      ggplot2::labs(y="a")
  }
  if (labelest == "b1") {
    result_b1 <- result[result$label=="b1",]
    figure <- ggplot2::ggplot(result_b1, ggplot2::aes_string(x='rymu', y='est'))+
      ggplot2::geom_point()+
      ggplot2::geom_line()+
      ggplot2::geom_ribbon(data=result_b1,ggplot2::aes_string(ymin='ci.lower',ymax='ci.upper'),alpha=0.3) +
      ggplot2::geom_hline(yintercept=result_b1$est.omit,linetype="dashed") +
      ggplot2::labs(y="b")
  }
  if (labelest == "c") {
    result_c <- result[result$label=="c",]
    figure <- ggplot2::ggplot(result_c, ggplot2::aes_string(x='rymu', y='est'))+
      ggplot2::geom_point()+
      ggplot2::geom_line()+
      ggplot2::geom_ribbon(data=result_c,ggplot2::aes_string(ymin='ci.lower',ymax='ci.upper'),alpha=0.3) +
      ggplot2::geom_hline(yintercept=result_c$est.omit,linetype="dashed") +
      ggplot2::labs(y="direct effect c")
  }
  if (labelest == "a1b1indirect1") {
    result_a1b1ind <- result[result$label!="c",]
    figure <- ggplot2::ggplot(result_a1b1ind[c(T,rep(F,40)),],
                              ggplot2::aes_string(x='rymu', y='est', lty='label', shape='label', color='label')) +
      ggplot2::geom_point(size=1.7)+
      ggplot2::geom_line(size=0.6)+
      ggplot2::geom_ribbon(data=result[result$label=="indirect1",],ggplot2::aes_string(ymin='ci.lower',ymax='ci.upper'),alpha=0.3) +
      ggplot2::geom_hline(yintercept=result[result$label=="indirect1",]$est.omit,linetype="dashed") +
      ggplot2::labs(y="indirect effect")
  }
  return(figure)
}

#' sensitivity plot for estimates with regards to correlation between Y and Mu
#'
#' give the sensitivity plot for how the correlation between Y and Mu affects the estimates and confidence interval
#'
#' @param rxmo correlation between X and Mo
#' @param rxy correlation between X and Y
#' @param rymo correlation between Y and Mo
#' @param nobs number of observations in the sample or sample size
#' @param labelest label for which estimate the figure is for: indirect1, a1, b1 or c, default is indirect1
#' @param conflevel confidence interval level, default is 0.95
#' @param specifyunob whether the user wants to specify two unobserved correlations default is 0, meaning no
#' @param rxmu correlation between X and Mu
#' @param rmomu correlation between Mo and Mu
#' @return plot for how rmomu affects the estimates and confidence interval
#' @export
rymu_plot <- function(rxmo=-2,rxy=-2,rymo=-2,nobs=nobs,labelest = "indirect1", conflevel=0.95,specifyunob = 0, rxmu=-2,rmomu=-2){
  if (specifyunob == 0) {
    plow <- rymu_plot_auxi(rxmo=rxmo,rxy=rxy,rymo=rymo,rxmu=0.0995,rmomu=0.1, nobs=nobs, labelest = labelest, conflevel=conflevel)
    pmedium <- rymu_plot_auxi(rxmo=rxmo,rxy=rxy,rymo=rymo,rxmu=0.2425,rmomu=0.3, nobs=nobs, labelest = labelest, conflevel=conflevel)
    phigh <- rymu_plot_auxi(rxmo=rxmo,rxy=rxy,rymo=rymo,rxmu=0.3713,rmomu=0.5, nobs=nobs, labelest = labelest, conflevel=conflevel)
    plowhigh <- rymu_plot_auxi(rxmo=rxmo,rxy=rxy,rymo=rymo,rxmu=0.0995,rmomu=0.5, nobs=nobs, labelest = labelest, conflevel=conflevel)
    phighlow <- rymu_plot_auxi(rxmo=rxmo,rxy=rxy,rymo=rymo,rxmu=0.3713,rmomu=0.1, nobs=nobs, labelest = labelest, conflevel=conflevel)
    figure <- ggpubr::ggarrange(plow, pmedium, phigh, plowhigh, phighlow,
                                labels = c("Low", "Medium", "High", "Low rxmu & High rmomu", "High rxmu & Low rmomu"),
                                ncol = 1, nrow = 5)
  }
  if (specifyunob == 1) {
    figure <- rymu_plot_auxi(rxmo=rxmo,rxy=rxy,rymo=rymo,rxmu=rxmu,rmomu=rmomu, nobs=nobs, labelest = labelest, conflevel=conflevel)
  }
  return(figure)
}
