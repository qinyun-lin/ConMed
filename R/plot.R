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
rxmu_plot <- function(rxmo=-2,rxy=-2,rymo=-2,nobs=nobs,labelest = "indirect1", conflevel=0.95, specifyunob = 0, rmomu=-2,rymu=-2){
  if (specifyunob == 0) {
    S1 <- rxmu_data(rxmo=rxmo,rxy=rxy,rymo=rymo,rmomu=0.1,rymu=0.1, nobs=nobs, conflevel=conflevel)
    S2 <- rxmu_data(rxmo=rxmo,rxy=rxy,rymo=rymo,rmomu=0.3,rymu=0.3, nobs=nobs, conflevel=conflevel)
    S3 <- rxmu_data(rxmo=rxmo,rxy=rxy,rymo=rymo,rmomu=0.5,rymu=0.5, nobs=nobs, conflevel=conflevel)
    S4 <- rxmu_data(rxmo=rxmo,rxy=rxy,rymo=rymo,rmomu=0.1,rymu=0.5, nobs=nobs, conflevel=conflevel)
    S5 <- rxmu_data(rxmo=rxmo,rxy=rxy,rymo=rymo,rmomu=0.5,rymu=0.1, nobs=nobs, conflevel=conflevel)
    S1$cat <- "S1"
    S2$cat <- "S2"
    S3$cat <- "S3"
    S4$cat <- "S4"
    S5$cat <- "S5"
    result <- rbind(S1, S2, S3, S4, S5)
    newlabel <- c("S1" = expression("Low"~ rho[M[O]*M[U]] ~ "Low" ~ rho[Y*M[U]]),
                  "S2" = expression("Medium"~ rho[M[O]*M[U]] ~ "Medium" ~ rho[Y*M[U]]),
                  "S3" = expression("High"~ rho[M[O]*M[U]] ~ "High" ~ rho[Y*M[U]]),
                  "S4" = expression("Low"~ rho[M[O]*M[U]] ~ "High" ~ rho[Y*M[U]]),
                  "S5" = expression("High"~ rho[M[O]*M[U]] ~ "Low" ~ rho[Y*M[U]]))
    result$cat <- factor(result$cat, labels = newlabel)
    result <- result[result$label == labelest,]
    if (labelest == "a1") {ytitle <- expression(a[1])}
    if (labelest == "b1") {ytitle <- expression(b[1])}
    if (labelest == "c") {ytitle <- expression(c)}
    if (labelest == "indirect1") {ytitle <- expression(a[1]*b[1])}
    figure <- ggplot2::ggplot(result, ggplot2::aes(x=rxmu, y=est)) +
      ggplot2::geom_ribbon(data=result, ggplot2::aes_string(ymin='ci.lower',ymax='ci.upper'),alpha=0.3) +
      ggplot2::geom_line() +
      ggplot2::geom_point() +
      ggplot2::labs(x = expression(rho[X*M[U]])) +
      ggplot2::labs(y = ytitle)+
      ggplot2::geom_hline(yintercept=result$est.omit,linetype="dashed") +
      ggplot2::facet_wrap(.~ cat, labeller = label_parsed, nrow = 5, scale = "free") +
      ggplot2::theme(plot.title = element_text(hjust = 0.5)) +
      ggplot2::theme_bw() +
      ggplot2::theme(plot.title = element_text(size = 14, hjust = 0.5, face = "bold"),
            axis.line = element_line(colour = "black"),
            axis.ticks.x=element_blank(),
            legend.text = element_text(colour="black", size = 12),
            legend.title = element_text(colour="black", size = 12))
}
  if (specifyunob == 1) {
    figure <- rxmu_plot_auxi(rxmo=rxmo,rxy=rxy,rymo=rymo,rmomu=rmomu,rymu=rymu, nobs=nobs, labelest = labelest, conflevel=conflevel)
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
rmomu_plot1 <- function(rxmo=-2,rxy=-2,rymo=-2,nobs=nobs,labelest = "indirect1", conflevel=0.95,specifyunob = 0, rxmu=-2,rymu=-2){
  if (specifyunob == 0) {
    S1 <- rmomu_data(rxmo=rxmo,rxy=rxy,rymo=rymo,rxmu=0.1,rymu=0.1, nobs=nobs, conflevel=conflevel)
    S2 <- rmomu_data(rxmo=rxmo,rxy=rxy,rymo=rymo,rxmu=0.3,rymu=0.3, nobs=nobs, conflevel=conflevel)
    S3 <- rmomu_data(rxmo=rxmo,rxy=rxy,rymo=rymo,rxmu=0.5,rymu=0.5, nobs=nobs, conflevel=conflevel)
    S4 <- rmomu_data(rxmo=rxmo,rxy=rxy,rymo=rymo,rxmu=0.1,rymu=0.5, nobs=nobs, conflevel=conflevel)
    S5 <- rmomu_data(rxmo=rxmo,rxy=rxy,rymo=rymo,rxmu=0.5,rymu=0.1, nobs=nobs, conflevel=conflevel)
    S1$cat <- "S1"
    S2$cat <- "S2"
    S3$cat <- "S3"
    S4$cat <- "S4"
    S5$cat <- "S5"
    result <- rbind(S1, S2, S3, S4, S5)
    if (labelest == "a1") {ytitle <- expression(a[1])}
    if (labelest == "b1") {ytitle <- expression(b[1])}
    if (labelest == "c") {ytitle <- expression(c)}
    if (labelest == "indirect1") {ytitle <- expression(a[1]*b[1])}
    newlabel <- c("S1" = expression("Low"~ rho[X*M[U]] ~ "Low" ~ rho[Y*M[U]]),
                  "S2" = expression("Medium"~ rho[X*M[U]] ~ "Medium" ~ rho[Y*M[U]]),
                  "S3" = expression("High"~ rho[X*M[U]] ~ "High" ~ rho[Y*M[U]]),
                  "S4" = expression("Low"~ rho[X*M[U]] ~ "High" ~ rho[Y*M[U]]),
                  "S5" = expression("High"~ rho[X*M[U]] ~ "Low" ~ rho[Y*M[U]]))
    result$cat <- factor(result$cat, labels = newlabel)
    result <- result[result$label == labelest,]
    figure <- ggplot2::ggplot(result, ggplot2::aes(x=rmomu, y=est)) +
      ggplot2::geom_ribbon(data=result, ggplot2::aes_string(ymin='ci.lower',ymax='ci.upper'),alpha=0.3) +
      ggplot2::geom_line() +
      ggplot2::geom_point() +
      ggplot2::labs(x = expression(rho[M[O]*M[U]])) +
      ggplot2::labs(y = ytitle)+
      ggplot2::geom_hline(yintercept=result$est.omit,linetype="dashed") +
      ggplot2::facet_wrap(.~ cat, labeller = label_parsed, nrow = 5, scale = "free") +
      ggplot2::theme(plot.title = element_text(hjust = 0.5)) +
      ggplot2::theme_bw() +
      ggplot2::theme(plot.title = element_text(size = 14, hjust = 0.5, face = "bold"),
                     axis.line = element_line(colour = "black"),
                     axis.ticks.x=element_blank(),
                     legend.text = element_text(colour="black", size = 12),
                     legend.title = element_text(colour="black", size = 12))
  }
  if (specifyunob == 1) {
    figure <- rmomu_plot_auxi(rxmo=rxmo,rxy=rxy,rymo=rymo,rxmu=rxmu,rymu=rymu, nobs=nobs, labelest = labelest, conflevel=conflevel)
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
rymu_plot1 <- function(rxmo=-2,rxy=-2,rymo=-2,nobs=nobs,labelest = "indirect1", conflevel=0.95,specifyunob = 0, rxmu=-2,rmomu=-2){
  if (specifyunob == 0) {
    S1 <- rymu_data(rxmo=rxmo,rxy=rxy,rymo=rymo,rxmu=0.1,rmomu=0.1, nobs=nobs, conflevel=conflevel)
    S2 <- rymu_data(rxmo=rxmo,rxy=rxy,rymo=rymo,rxmu=0.3,rmomu=0.3, nobs=nobs, conflevel=conflevel)
    S3 <- rymu_data(rxmo=rxmo,rxy=rxy,rymo=rymo,rxmu=0.5,rmomu=0.5, nobs=nobs, conflevel=conflevel)
    S4 <- rymu_data(rxmo=rxmo,rxy=rxy,rymo=rymo,rxmu=0.1,rmomu=0.5, nobs=nobs, conflevel=conflevel)
    S5 <- rymu_data(rxmo=rxmo,rxy=rxy,rymo=rymo,rxmu=0.5,rmomu=0.1, nobs=nobs, conflevel=conflevel)
    S1$cat <- "S1"
    S2$cat <- "S2"
    S3$cat <- "S3"
    S4$cat <- "S4"
    S5$cat <- "S5"
    result <- rbind(S1, S2, S3, S4, S5)
    if (labelest == "a1") {ytitle <- expression(a[1])}
    if (labelest == "b1") {ytitle <- expression(b[1])}
    if (labelest == "c") {ytitle <- expression(c)}
    if (labelest == "indirect1") {ytitle <- expression(a[1]*b[1])}
    newlabel <- c("S1" = expression("Low"~ rho[X*M[U]] ~ "Low" ~ rho[M[O]*M[U]]),
                  "S2" = expression("Medium"~ rho[X*M[U]] ~ "Medium" ~ rho[M[O]*M[U]]),
                  "S3" = expression("High"~ rho[X*M[U]] ~ "High" ~ rho[M[O]*M[U]]),
                  "S4" = expression("Low"~ rho[X*M[U]] ~ "High" ~ rho[M[O]*M[U]]),
                  "S5" = expression("High"~ rho[X*M[U]] ~ "Low" ~ rho[M[O]*M[U]]))
    result$cat <- factor(result$cat, labels = newlabel)
    result <- result[result$label == labelest,]
    figure <- ggplot2::ggplot(result, ggplot2::aes(x=rymu, y=est)) +
      ggplot2::geom_ribbon(data=result, ggplot2::aes_string(ymin='ci.lower',ymax='ci.upper'),alpha=0.3) +
      ggplot2::geom_line() +
      ggplot2::geom_point() +
      ggplot2::labs(x = expression(rho[Y*M[U]])) +
      ggplot2::labs(y = ytitle)+
      ggplot2::geom_hline(yintercept=result$est.omit,linetype="dashed") +
      ggplot2::facet_wrap(.~ cat, labeller = label_parsed, nrow = 5, scale = "free") +
      ggplot2::theme(plot.title = element_text(hjust = 0.5)) +
      ggplot2::theme_bw() +
      ggplot2::theme(plot.title = element_text(size = 14, hjust = 0.5, face = "bold"),
                     axis.line = element_line(colour = "black"),
                     axis.ticks.x=element_blank(),
                     legend.text = element_text(colour="black", size = 12),
                     legend.title = element_text(colour="black", size = 12))
  }
  if (specifyunob == 1) {
    figure <- rymu_plot_auxi(rxmo=rxmo,rxy=rxy,rymo=rymo,rxmu=rxmu,rmomu=rmomu, nobs=nobs, labelest = labelest, conflevel=conflevel)
  }
  return(figure)
}
