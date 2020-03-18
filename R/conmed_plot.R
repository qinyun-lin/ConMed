#' sensitivity plot for estimates with regards to all correlations regarding Mu
#'
#' give the sensitivity plot for how the correlation between Y and Mu affects the estimates and confidence interval
#'
#' @param rxmo correlation between X and Mo
#' @param rxy correlation between X and Y
#' @param rymo correlation between Y and Mo
#' @param nobs number of observations in the sample or sample size
#' @param labelest label for which estimate the figure is for: a1b1, a1, b1 or c, default is a1b1
#' @param conflevel confidence interval level, default is 0.95
#' @return plot for how rxmu rymu rmomu affects the estimates and confidence interval
#' @export
conmed_plot <- function(rxmo=-2,rxy=-2,rymo=-2,nobs=nobs,labelest = "a1b1", conflevel=0.95){
  rxmuplot<- rxmu_plot(rxmo,rxy,rymo,nobs,labelest, conflevel, specifyunob = 0)
  rmomuplot <- rmomu_plot(rxmo,rxy,rymo,nobs,labelest, conflevel,specifyunob = 0)
  rymuplot <- rymu_plot(rxmo,rxy,rymo,nobs,labelest, conflevel,specifyunob = 0)
  figure <- ggpubr::ggarrange(rxmuplot, rmomuplot, rymuplot, ncol = 3, nrow = 1)
  return(figure)
}

#' sensitivity plot for estimates with regards to all correlations regarding Mu
#'
#' give the sensitivity plot for how the correlation between Y and Mu affects the estimates and confidence interval
#'
#' @param rxmo correlation between X and Mo
#' @param rxy correlation between X and Y
#' @param rymo correlation between Y and Mo
#' @param nobs number of observations in the sample or sample size
#' @param conflevel confidence interval level, default is 0.95
#' @return plot for how rxmu rymu rmomu affects the estimates and confidence interval
#' @export
conmed_plot_a1b1 <- function(rxmo=-2,rxy=-2,rymo=-2,nobs=nobs, conflevel=0.95){
  rxmuplot<- rxmu_plot_a1b1(rxmo,rxy,rymo,nobs, conflevel, specifyunob = 0)
  rmomuplot <- rmomu_plot_a1b1(rxmo,rxy,rymo,nobs, conflevel,specifyunob = 0)
  rymuplot <- rymu_plot_a1b1(rxmo,rxy,rymo,nobs, conflevel,specifyunob = 0)
  figure <- ggpubr::ggarrange(rxmuplot, rmomuplot, rymuplot, ncol = 3, nrow = 1)
  return(figure)
}
