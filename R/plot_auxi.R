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
      ggplot2::labs(y=expression(a[1]*b[1]))+
      ggplot2::labs(x=expression(rho[X*M[U]]))
                      }
  if (labelest == "a1") {
    result_a1 <- result[result$label=="a1",]
    figure <- ggplot2::ggplot(result_a1, ggplot2::aes_string(x='rxmu', y='est'))+
      ggplot2::geom_point()+
      ggplot2::geom_line()+
      ggplot2::geom_ribbon(data=result_a1,ggplot2::aes_string(ymin='ci.lower',ymax='ci.upper'),alpha=0.3) +
      ggplot2::geom_hline(yintercept=result_a1$est.omit,linetype="dashed") +
      ggplot2::labs(y=expression(a[1]))+
      ggplot2::labs(x=expression(rho[X*M[U]]))
  }
  if (labelest == "b1") {
    result_b1 <- result[result$label=="b1",]
    figure <- ggplot2::ggplot(result_b1, ggplot2::aes_string(x='rxmu', y='est'))+
      ggplot2::geom_point()+
      ggplot2::geom_line()+
      ggplot2::geom_ribbon(data=result_b1,ggplot2::aes_string(ymin='ci.lower',ymax='ci.upper'),alpha=0.3) +
      ggplot2::geom_hline(yintercept=result_b1$est.omit,linetype="dashed") +
      ggplot2::labs(y=expression(b[1]))+
      ggplot2::labs(x=expression(rho[X*M[U]]))
  }
  if (labelest == "c") {
    result_c <- result[result$label=="c",]
    figure <- ggplot2::ggplot(result_c, ggplot2::aes_string(x='rxmu', y='est'))+
      ggplot2::geom_point()+
      ggplot2::geom_line()+
      ggplot2::geom_ribbon(data=result_c,ggplot2::aes_string(ymin='ci.lower',ymax='ci.upper'),alpha=0.3) +
      ggplot2::geom_hline(yintercept=result_c$est.omit,linetype="dashed") +
      ggplot2::labs(y=expression(c))+
      ggplot2::labs(x=expression(rho[X*M[U]]))
  }
  if (labelest == "a1b1indirect1") {
    result_a1b1ind <- result[result$label!="c",]
    levels(result_a1b1ind$label) <- c("a1", "b1", "c", "a1b1")
    figure <- ggplot2::ggplot(result_a1b1ind[c(T,rep(F,40)),],
                              ggplot2::aes_string(x='rxmu', y='est', lty='label', shape='label', color='label')) +
      ggplot2::geom_point(size=1.7)+
      ggplot2::geom_line(size=0.6)+
      ggplot2::geom_ribbon(
        data=result_a1b1ind[result_a1b1ind$label=="a1b1",],
        ggplot2::aes_string(ymin='ci.lower',ymax='ci.upper'),
        alpha=0.3) +
      ggplot2::geom_hline(
        yintercept=result_a1b1ind[result_a1b1ind$label=="a1b1",]$est.omit,
        linetype="dashed") +
      ggplot2::labs(x = expression(rho[X*M[U]])) +
      ggplot2::labs(y = NULL)+
      ggplot2::theme_bw() +
      ggplot2::theme(plot.title = element_text(size = 10, hjust = 0.5, face = "bold"),
                     axis.line = element_line(colour = "black"),
                     axis.ticks.x=element_blank(),
                     legend.text = element_text(colour="black", size = 10),
                     legend.title = element_blank())
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
      ggplot2::labs(y=expression(a[1]*b[1]))+
      ggplot2::labs(x=expression(rho[M[O]*M[U]]))
  }
  if (labelest == "a1") {
    result_a1 <- result[result$label=="a1",]
    figure <- ggplot2::ggplot(result_a1, ggplot2::aes_string(x='rmomu', y='est'))+
      ggplot2::geom_point()+
      ggplot2::geom_line()+
      ggplot2::geom_ribbon(data=result_a1,ggplot2::aes_string(ymin='ci.lower',ymax='ci.upper'),alpha=0.3) +
      ggplot2::geom_hline(yintercept=result_a1$est.omit,linetype="dashed") +
      ggplot2::labs(y=expression(a[1]))+
      ggplot2::labs(x=expression(rho[M[O]*M[U]]))
  }
  if (labelest == "b1") {
    result_b1 <- result[result$label=="b1",]
    figure <- ggplot2::ggplot(result_b1, ggplot2::aes_string(x='rmomu', y='est'))+
      ggplot2::geom_point()+
      ggplot2::geom_line()+
      ggplot2::geom_ribbon(data=result_b1,ggplot2::aes_string(ymin='ci.lower',ymax='ci.upper'),alpha=0.3) +
      ggplot2::geom_hline(yintercept=result_b1$est.omit,linetype="dashed") +
      ggplot2::labs(y=expression(b[1]))+
      ggplot2::labs(x=expression(rho[M[O]*M[U]]))
  }
  if (labelest == "c") {
    result_c <- result[result$label=="c",]
    figure <- ggplot2::ggplot(result_c, ggplot2::aes_string(x='rmomu', y='est'))+
      ggplot2::geom_point()+
      ggplot2::geom_line()+
      ggplot2::geom_ribbon(data=result_c,ggplot2::aes_string(ymin='ci.lower',ymax='ci.upper'),alpha=0.3) +
      ggplot2::geom_hline(yintercept=result_c$est.omit,linetype="dashed") +
      ggplot2::labs(y=expression(c))+
      ggplot2::labs(x=expression(rho[M[O]*M[U]]))
      }
  if (labelest == "a1b1indirect1") {
    result_a1b1ind <- result[result$label!="c",]
    levels(result_a1b1ind$label) <- c("a1", "b1", "c", "a1b1")
    figure <- ggplot2::ggplot(result_a1b1ind[c(T,rep(F,40)),],
                              ggplot2::aes_string(x='rmomu', y='est', lty='label', shape='label', color='label')) +
      ggplot2::geom_point(size=1.7)+
      ggplot2::geom_line(size=0.6)+
      ggplot2::geom_ribbon(
        data=result_a1b1ind[result_a1b1ind$label=="a1b1",],
        ggplot2::aes_string(ymin='ci.lower',ymax='ci.upper'),
        alpha=0.3) +
      ggplot2::geom_hline(
        yintercept=result_a1b1ind[result_a1b1ind$label=="a1b1",]$est.omit,
        linetype="dashed") +
      ggplot2::ylab(NULL) +
      ggplot2::labs(x=expression(rho[M[O]*M[U]])) +
      ggplot2::theme_bw() +
      ggplot2::theme(plot.title = element_text(size = 10, hjust = 0.5, face = "bold"),
                     axis.line = element_line(colour = "black"),
                     axis.ticks.x=element_blank(),
                     legend.text = element_text(colour="black", size = 10),
                     legend.title = element_blank())
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
      ggplot2::labs(y=expression(a[1]*b[1]))+
      ggplot2::labs(x=expression(rho[Y*M[U]]))
  }
  if (labelest == "a1") {
    result_a1 <- result[result$label=="a1",]
    figure <- ggplot2::ggplot(result_a1, ggplot2::aes_string(x='rymu', y='est'))+
      ggplot2::geom_point()+
      ggplot2::geom_line()+
      ggplot2::geom_ribbon(data=result_a1,ggplot2::aes_string(ymin='ci.lower',ymax='ci.upper'),alpha=0.3) +
      ggplot2::geom_hline(yintercept=result_a1$est.omit,linetype="dashed") +
      ggplot2::labs(y=expression(a[1]))+
      ggplot2::labs(x=expression(rho[Y*M[U]]))
  }
  if (labelest == "b1") {
    result_b1 <- result[result$label=="b1",]
    figure <- ggplot2::ggplot(result_b1, ggplot2::aes_string(x='rymu', y='est'))+
      ggplot2::geom_point()+
      ggplot2::geom_line()+
      ggplot2::geom_ribbon(data=result_b1,ggplot2::aes_string(ymin='ci.lower',ymax='ci.upper'),alpha=0.3) +
      ggplot2::geom_hline(yintercept=result_b1$est.omit,linetype="dashed") +
      ggplot2::labs(y=expression(b[1]))+
      ggplot2::labs(x=expression(rho[Y*M[U]]))
  }
  if (labelest == "c") {
    result_c <- result[result$label=="c",]
    figure <- ggplot2::ggplot(result_c, ggplot2::aes_string(x='rymu', y='est'))+
      ggplot2::geom_point()+
      ggplot2::geom_line()+
      ggplot2::geom_ribbon(data=result_c,ggplot2::aes_string(ymin='ci.lower',ymax='ci.upper'),alpha=0.3) +
      ggplot2::geom_hline(yintercept=result_c$est.omit,linetype="dashed") +
      ggplot2::labs(y=expression(c))+
      ggplot2::labs(x=expression(rho[Y*M[U]]))
  }
  if (labelest == "a1b1indirect1") {
    result_a1b1ind <- result[result$label!="c",]
    levels(result_a1b1ind$label) <- c("a1", "b1", "c", "a1b1")
    figure <- ggplot2::ggplot(result_a1b1ind[c(T,rep(F,40)),],
                              ggplot2::aes_string(x='rymu', y='est', lty='label', shape='label', color='label')) +
      ggplot2::geom_point(size=1.7)+
      ggplot2::geom_line(size=0.6)+
      ggplot2::geom_ribbon(
        data=result_a1b1ind[result_a1b1ind$label=="a1b1",],
        ggplot2::aes_string(ymin='ci.lower',ymax='ci.upper'),
        alpha=0.3) +
      ggplot2::geom_hline(
        yintercept=result_a1b1ind[result_a1b1ind$label=="a1b1",]$est.omit,
        linetype="dashed") +
      ggplot2::ylab(NULL) +
      ggplot2::labs(x=expression(rho[Y*M[U]]))+
      ggplot2::theme_bw() +
      ggplot2::theme(plot.title = element_text(size = 10, hjust = 0.5, face = "bold"),
                     axis.line = element_line(colour = "black"),
                     axis.ticks.x=element_blank(),
                     legend.text = element_text(colour="black", size = 10),
                     legend.title = element_blank())
  }
  return(figure)
}
