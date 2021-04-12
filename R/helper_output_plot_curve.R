# plot impact curve

plot_curve <- function(focus, obs_r, critical_r, r_con) {
  #obs_r = 0.201
  #critical_r = 0.15
  data <- data.frame(matrix(0, nrow = 1000, ncol = 2))
  names(data) = c("rxcv", "rycv")

  for (i in 1 : 500){
    data$rxcv[i] <- rxcv <- (0.999 - r_con) / 500 * i + r_con
    data$rycv[i] <- (- 2 * obs_r * rxcv +
                        sqrt(4 * obs_r^2 * rxcv^2 - 4 * (critical_r^2 - obs_r^2 - critical_r^2 * rxcv^2) *
                               (- critical_r^2 - rxcv^2 + critical_r^2 * rxcv^2))) /
      (2 * (-critical_r^2 - rxcv^2 + critical_r^2 * rxcv^2))
  }

  data$rxcv[501:1000] <- data$rycv[1:500]
  data$rycv[501:1000] <- data$rxcv[1:500]

  data$impact <- data$rxcv * data$rycv
  if (focus =="inval_a"){
    xlabel <- c("|rX,CV| conditional on other covariates")
    ylabel <- c("|rM,CV| conditional on other covariates")
  }

  if (focus == "inval_b"){
    xlabel <- c("|rM,CV| conditional on other covariates")
    ylabel <- c("|rY,CV| conditional on other covariates")
  }

 plot <- ggplot2::ggplot(data, ggplot2::aes(x=rxcv, y=rycv)) +
    ggplot2::geom_point(color = "blue", size = 1.5) +
    ggplot2::xlim(0, 1) +
    ggplot2::ylim(0, 1) +
    ggplot2:: geom_abline(slope = 1, intercept = 0, alpha = 0.7, linetype = "dotdash") +
    ggplot2::theme(plot.title = element_text(hjust = 0.5)) +
    ggplot2::xlab(xlabel)+
    ggplot2::ylab(ylabel)+
    ggplot2::theme_bw() +
    ggplot2::theme(plot.title = element_text(size = 14, hjust = 0.5, face = "bold"),
                   axis.line = element_line(colour = "black"),
                   axis.ticks.x=element_blank(),
                   legend.text = element_text(colour="black", size = 12),
                   legend.title = element_text(colour="black", size = 12),
                   panel.border = element_blank())
 plot
}


