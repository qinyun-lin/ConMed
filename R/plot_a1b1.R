#' sensitivity plot for estimates with regards to correlation between X and Mu
#'
#' give the sensitivity plot for how the correlation between X and Mu affects the estimates and confidence interval
#' regarding a1, b1, and a1*b1
#'
#' @param rxmo correlation between X and Mo
#' @param rxy correlation between X and Y
#' @param rymo correlation between Y and Mo
#' @param nobs number of observations in the sample or sample size
#' @param conflevel confidence interval level, default is 0.95
#' @param specifyunob whether the user wants to specify two unobserved correlations default is 0, meaning no
#' @param rmomu correlation between Mo and Mu
#' @param rymu correlation between Y and Mu
#' @return plot for how rxmu affects the estimates and confidence interval
#' @export
rxmu_plot_a1b1 <- function(rxmo=-2,rxy=-2,rymo=-2,nobs=nobs, conflevel=0.95,specifyunob = 0, rmomu=-2,rymu=-2){
  if (specifyunob == 0) {
    plow <- rxmu_plot_auxi(rxmo=rxmo,rxy=rxy,rymo=rymo,rmomu=0.1,rymu=0.1, nobs=nobs, labelest = "a1b1indirect1", conflevel=conflevel)
    pmedium <- rxmu_plot_auxi(rxmo=rxmo,rxy=rxy,rymo=rymo,rmomu=0.3,rymu=0.3, nobs=nobs, labelest = "a1b1indirect1", conflevel=conflevel)
    phigh <- rxmu_plot_auxi(rxmo=rxmo,rxy=rxy,rymo=rymo,rmomu=0.5,rymu=0.5, nobs=nobs, labelest = "a1b1indirect1", conflevel=conflevel)
    plowhigh <- rxmu_plot_auxi(rxmo=rxmo,rxy=rxy,rymo=rymo,rmomu=0.1,rymu=0.5, nobs=nobs, labelest = "a1b1indirect1", conflevel=conflevel)
    phighlow <- rxmu_plot_auxi(rxmo=rxmo,rxy=rxy,rymo=rymo,rmomu=0.5,rymu=0.1, nobs=nobs, labelest = "a1b1indirect1", conflevel=conflevel)
    plow <- plow + ggplot2::ggtitle(expression("Low"~ rho[M[O]*M[U]] ~ "Low" ~ rho[Y*M[U]]))
    pmedium <- pmedium + ggplot2::ggtitle(expression("Medium"~ rho[M[O]*M[U]] ~ "Medium" ~ rho[Y*M[U]]))
    phigh <- phigh + ggplot2::ggtitle(expression("High" ~ rho[M[O]*M[U]] ~ "High" ~ rho[Y*M[U]]))
    plowhigh <- plowhigh + ggplot2::ggtitle(expression("Low"~ rho[M[O]*M[U]] ~ "High" ~ rho[Y*M[U]]))
    phighlow <- phighlow + ggplot2::ggtitle(expression("High"~ rho[M[O]*M[U]] ~ "Low" ~ rho[Y*M[U]]))
    figure <- ggpubr::ggarrange(plow, pmedium, phigh, plowhigh, phighlow,
                                ncol = 1, nrow = 5)
  }
   if (specifyunob == 1) {
     figure <- rxmu_plot_auxi(rxmo=rxmo,rxy=rxy,rymo=rymo,rmomu=rmomu,rymu=rymu, nobs=nobs, labelest = "a1b1indirect1", conflevel=conflevel)
   }
  return(figure)
}

#' sensitivity plot for estimates with regards to correlation between Mo and Mu
#'
#' give the sensitivity plot for how the correlation between Mo and Mu affects the estimates and confidence interval
#' regarding a1, b1, and a1*b1
#'
#' @param rxmo correlation between X and Mo
#' @param rxy correlation between X and Y
#' @param rymo correlation between Y and Mo
#' @param nobs number of observations in the sample or sample size
#' @param conflevel confidence interval level, default is 0.95
#' @param specifyunob whether the user wants to specify two unobserved correlations default is 0, meaning no
#' @param rxmu correlation between X and Mu
#' @param rymu correlation between Y and Mu
#' @return plot for how rmomu affects the estimates and confidence interval
#' @export
rmomu_plot_a1b1 <- function(rxmo=-2,rxy=-2,rymo=-2,nobs=nobs, conflevel=0.95,specifyunob = 0, rxmu=-2,rymu=-2){
  if (specifyunob == 0) {
    plow <- rmomu_plot_auxi(rxmo=rxmo,rxy=rxy,rymo=rymo,rxmu=0.0995,rymu=0.1, nobs=nobs, labelest = "a1b1indirect1", conflevel=conflevel)
    pmedium <- rmomu_plot_auxi(rxmo=rxmo,rxy=rxy,rymo=rymo,rxmu=0.2425,rymu=0.3, nobs=nobs, labelest = "a1b1indirect1", conflevel=conflevel)
    phigh <- rmomu_plot_auxi(rxmo=rxmo,rxy=rxy,rymo=rymo,rxmu=0.3713,rymu=0.5, nobs=nobs, labelest = "a1b1indirect1", conflevel=conflevel)
    plowhigh <- rmomu_plot_auxi(rxmo=rxmo,rxy=rxy,rymo=rymo,rxmu=0.0995,rymu=0.5, nobs=nobs, labelest = "a1b1indirect1", conflevel=conflevel)
    phighlow <- rmomu_plot_auxi(rxmo=rxmo,rxy=rxy,rymo=rymo,rxmu=0.3713,rymu=0.1, nobs=nobs, labelest = "a1b1indirect1", conflevel=conflevel)
    plow <- plow + ggplot2::ggtitle(expression("Low"~ rho[X*M[U]] ~ "Low" ~ rho[Y*M[U]]))
    pmedium <- pmedium + ggplot2::ggtitle(expression("Medium"~ rho[X*M[U]] ~ "Medium" ~ rho[Y*M[U]]))
    phigh <- phigh + ggplot2::ggtitle(expression("High" ~ rho[X*M[U]] ~ "High" ~ rho[Y*M[U]]))
    plowhigh <- plowhigh + ggplot2::ggtitle(expression("Low"~ rho[X*M[U]] ~ "High" ~ rho[Y*M[U]]))
    phighlow <- phighlow + ggplot2::ggtitle(expression("High"~ rho[X*M[U]] ~ "Low" ~ rho[Y*M[U]]))
    figure <- ggpubr::ggarrange(plow, pmedium, phigh, plowhigh, phighlow,
                                ncol = 1, nrow = 5)
  }
  if (specifyunob == 1) {
    figure <- rmomu_plot_auxi(rxmo=rxmo,rxy=rxy,rymo=rymo,rxmu=rxmu,rymu=rymu, nobs=nobs, labelest = "a1b1indirect1", conflevel=conflevel)
  }
  return(figure)
}

#' sensitivity plot for estimates with regards to correlation between Y and Mu
#'
#' give the sensitivity plot for how the correlation between Y and Mu affects the estimates and confidence interval
#' regarding a1,b1,and a1b1
#'
#' @param rxmo correlation between X and Mo
#' @param rxy correlation between X and Y
#' @param rymo correlation between Y and Mo
#' @param nobs number of observations in the sample or sample size
#' @param conflevel confidence interval level, default is 0.95
#' @param specifyunob whether the user wants to specify two unobserved correlations default is 0, meaning no
#' @param rxmu correlation between X and Mu
#' @param rmomu correlation between Mo and Mu
#' @return plot for how rmomu affects the estimates and confidence interval
#' @export
rymu_plot_a1b1 <- function(rxmo=-2,rxy=-2,rymo=-2,nobs=nobs,labelest = "indirect1", conflevel=0.95,specifyunob = 0, rxmu=-2,rmomu=-2){
  if (specifyunob == 0) {
    plow <- rymu_plot_auxi(rxmo=rxmo,rxy=rxy,rymo=rymo,rxmu=0.0995,rmomu=0.1, nobs=nobs, labelest = "a1b1indirect1", conflevel=conflevel)
    pmedium <- rymu_plot_auxi(rxmo=rxmo,rxy=rxy,rymo=rymo,rxmu=0.2425,rmomu=0.3, nobs=nobs, labelest = "a1b1indirect1", conflevel=conflevel)
    phigh <- rymu_plot_auxi(rxmo=rxmo,rxy=rxy,rymo=rymo,rxmu=0.3713,rmomu=0.5, nobs=nobs, labelest = "a1b1indirect1", conflevel=conflevel)
    plowhigh <- rymu_plot_auxi(rxmo=rxmo,rxy=rxy,rymo=rymo,rxmu=0.0995,rmomu=0.5, nobs=nobs, labelest = "a1b1indirect1", conflevel=conflevel)
    phighlow <- rymu_plot_auxi(rxmo=rxmo,rxy=rxy,rymo=rymo,rxmu=0.3713,rmomu=0.1, nobs=nobs, labelest = "a1b1indirect1", conflevel=conflevel)
    plow <- plow + ggplot2::ggtitle(expression("Low"~ rho[X*M[U]] ~ "Low" ~ rho[M[O]*M[U]]))
    pmedium <- pmedium + ggplot2::ggtitle(expression("Medium"~ rho[X*M[U]] ~ "Medium" ~ rho[M[O]*M[U]]))
    phigh <- phigh + ggplot2::ggtitle(expression("High" ~ rho[X*M[U]] ~ "High" ~ rho[M[O]*M[U]]))
    plowhigh <- plowhigh + ggplot2::ggtitle(expression("Low"~ rho[X*M[U]] ~ "High" ~ rho[M[O]*M[U]]))
    phighlow <- phighlow + ggplot2::ggtitle(expression("High"~ rho[X*M[U]] ~ "Low" ~ rho[M[O]*M[U]]))
    figure <- ggpubr::ggarrange(plow, pmedium, phigh, plowhigh, phighlow,
                                ncol = 1, nrow = 5)
  }
  if (specifyunob == 1) {
    figure <- rymu_plot_auxi(rxmo=rxmo,rxy=rxy,rymo=rymo,rxmu=rxmu,rmomu=rmomu, nobs=nobs, labelest = "a1b1indirect1", conflevel=conflevel)
  }
  return(figure)
}
