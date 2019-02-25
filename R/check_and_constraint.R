# check_input is to check whether the input is within a normal range of correlation 
# constrain_standardized is to check whether the standardization requirement is fulfiled

#' check if input is valid
#' 
#' check whether the user gives valid correlations
#' 
#' @param a each element of the input/correlation
#' 
#' @return True if the input is valid otherwise False
#' @export
check_input <- function(a){
  if ( a > 1 || a < -1 ) {
    return(FALSE) 
  } else {
    return(TRUE)
  }
}

#' check the standardization constraint
#' 
#' check if the combination of correlations fulfil the standardization constraint
#' 
#' @param a1 parameter for path X to Mo 
#' @param a2 parameter for path X to Mu
#' @param b1 parameter for path Mo to Y
#' @param b2 parameter for path Mu to Y
#' @param k parameter for path Mu to Mo
#' @param c parameter for path X to Y
#' 
#' @return True if the constraint is fulfilled otherwise False
#' @export
constrain_standardized <- function(a1,a2,b1,b2,k,c){
  c1 = 1-k^2-a1^2-2*k*a1*a2
  c2 = 1-a2^2
  c3 = 1-b1^2-b2^2-c^2-2*b1*b2*(k+a1*a2)-2*b1*c*(k*a2+a1)-2*a2*b2*c
  if ((c1>0) & (c2>0) & (c3>0)) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}