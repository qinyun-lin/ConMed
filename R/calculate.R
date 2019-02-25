#' calculate a1 from correlations
#'
#' calculate a1 as a function of correlations
#'
#' @param rxmo correlation between X and Mo
#' @param rmomu correlation between Mo and Mu
#' @param rxmu correlation between X and Mu
#'
#' @return the value of a1
#' @export
cal_a1 <- function(rxmo,rmomu,rxmu){
  a1 <- (rxmo-rmomu*rxmu)/(1-rxmu^2)
  return(a1)
}

#' calculate b1 from correlations
#'
#' calculate b1 as a function of correlations
#'
#' @param rxmo correlation between X and Mo
#' @param rxmu correlation between X and Mu
#' @param rmomu correlation between Mo and Mu
#' @param rxy correlation between X and Y
#' @param rymo correlation between Y and Mo
#' @param rymu correlation between Y and Mu
#'
#' @return the value of b1
#' @export
cal_b1 <- function(rxmo,rxmu,rmomu,rxy,rymo,rymu){
  b1 <- (rymo+rymu*rxmo*rxmu+rxy*rmomu*rxmu-rymo*rxmu^2-rymu*rmomu-rxy*rxmo)/
    (1+2*rmomu*rxmu*rxmo-rmomu^2-rxmu^2-rxmo^2)
  return(b1)
}

#' calculate c from correlations
#'
#' calculate c as a function of correlations
#'
#' @param rxmo correlation between X and Mo
#' @param rxmu correlation between X and Mu
#' @param rmomu correlation between Mo and Mu
#' @param rxy correlation between X and Y
#' @param rymo correlation between Y and Mo
#' @param rymu correlation between Y and Mu
#'
#' @return the value of c
#' @export
cal_c <- function(rxmo,rxmu,rmomu,rxy,rymo,rymu){
  c <- (rxy+rymu*rxmo*rmomu+rymo*rxmu*rmomu-rxy*rmomu^2-rymu*rxmu-rymo*rxmo)/
    (1+2*rxmu*rmomu*rxmo-rxmu^2-rmomu^2-rxmo^2)
  return(c)
}

#' calculate a2 from correlations
#'
#' calculate a2 as a function of correlations
#'
#' @param rxmu correlation between X and Mu
#'
#' @return the value of a2
#' @export
cal_a2 <- function(rxmu){
  a2 <- rxmu
  return(rxmu)
}

#' calculate b2 from correlations
#'
#' calculate b2 as a function of correlations
#'
#' @param rxmo correlation between X and Mo
#' @param rxmu correlation between X and Mu
#' @param rmomu correlation between Mo and Mu
#' @param rxy correlation between X and Y
#' @param rymo correlation between Y and Mo
#' @param rymu correlation between Y and Mu
#'
#' @return the value of b2
#' @export
cal_b2 <- function(rxmo,rxmu,rmomu,rxy,rymo,rymu){
  b2 <- (rymu+rymo*rxmu*rxmo+rxy*rmomu*rxmo-rymu*rxmo^2-rymo*rmomu-rxy*rxmu)/
    (1+2*rmomu*rxmu*rxmo-rmomu^2-rxmu^2-rxmo^2)
  return(b2)
}

#' calculate k from correlations
#'
#' calculate k as a function of correlations
#'
#' @param rxmo correlation between X and Mo
#' @param rxmu correlation between X and Mu
#' @param rmomu correlation between Mo and Mu
#'
#' @return the value of k
#' @export
cal_k <- function(rxmo,rxmu,rmomu){
  k <- (rmomu-rxmo*rxmu)/(1-rxmu^2)
  return(k)
}


#' calculate percentage of bias for a1 from correlations
#'
#' calculate percentage of bias for a1 a function of correlations
#'
#' @param rxmo correlation between X and Mo
#' @param rxmu correlation between X and Mu
#' @param rmomu correlation between Mo and Mu
#'
#' @return the percentage of bias for a1
#' @export
cal_bias_a1_perc <- function(rxmo,rxmu,rmomu){
  bias_a1_perc <- (rxmu*(rmomu-rxmo*rxmu))/(rxmo*(1-rxmu^2))
  return(bias_a1_perc)
}

#' calculate percentage of bias for b1 from correlations
#'
#' calculate percentage of bias for b1 a function of correlations
#'
#' @param rxmo correlation between X and Mo
#' @param rxmu correlation between X and Mu
#' @param rmomu correlation between Mo and Mu
#' @param rxy correlation between X and Y
#' @param rymo correlation between Y and Mo
#' @param rymu correlation between Y and Mu
#'
#' @return the percentage of bias for b1
#' @export
cal_bias_b1_perc <- function(rxmo,rxmu,rmomu,rxy,rymo,rymu){
  bias_b1_perc <- ((rmomu-rxmo*rxmu)*(rymu+rymo*rxmu*rxmo+rxy*rmomu*rxmo-rymu*rxmo^2-rymo*rmomu-rxy*rxmu))/
    ((rymo-rxmo*rxy)*(1+2*rmomu*rxmu*rxmo-rmomu^2-rxmu^2-rxmo^2))
  return(bias_b1_perc)
}

#' calculate percentage of bias for c from correlations
#'
#' calculate percentage of bias for c a function of correlations
#'
#' @param rxmo correlation between X and Mo
#' @param rxmu correlation between X and Mu
#' @param rmomu correlation between Mo and Mu
#' @param rxy correlation between X and Y
#' @param rymo correlation between Y and Mo
#' @param rymu correlation between Y and Mu
#'
#' @return the percentage of bias for c
#' @export
cal_bias_c_perc <- function(rxmo,rxmu,rmomu,rxy,rymo,rymu){
  bias_c_perc <- ((rxmu-rmomu*rxmo)*(rymu+rymo*rxmu*rxmo+rxy*rmomu*rxmo-rymu*rxmo^2-rymo*rmomu-rxy*rxmu))/
    ((rxy-rxmo*rymo)*(1+2*rmomu*rxmu*rxmo-rmomu^2-rxmu^2-rxmo^2))
  return(bias_c_perc)
}
