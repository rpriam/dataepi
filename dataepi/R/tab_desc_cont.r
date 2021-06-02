#' A function for describing the data.frame for the continuous variables
#' 
#' This function take as input a data.frame and the names of the continuous  
#' variables in order to print in a table the names, plus the median, mean,
#' standard-deviation, minimum, maximum, number of NA, and the number of not NA.
#' 
#' @param A The data.frame with the variables to test.
#' @param vars_cont  The names of the variables with continuous values.
#' @param nbdigits The number of decimals to keep.
#' 
#' @return A data.frame with each row for a continuous variable and with the 
#' following columns.
#' \describe{
#'   \item{var}{The name of a variable from the vector of names vars_cont.}
#'   \item{median}{The median of the variable.}
#'   \item{mean}{The mean of the variable.}
#'   \item{sd}{The standard-deviation of the variable.}
#'   \item{min}{The minimum of the variable.}
#'   \item{max}{The maximum of the variable.}
#'   \item{nb_na}{The number of missing values of the variable.}
#'   \item{nb}{The number of non missing values of the variable.}
#' }
#' @keywords 
#' @export
#' @examples
#' data(DebTrivedi)
#' A         <- DebTrivedi
#' vars_cont <- c("age","ofp")
#' resu_    <- tab_desc_cont(A,vars_cont)
#' print(resu_) 
#' 
tab_desc_cont <- function(A,vars_cont,nbdigits=2) {
  D=A[,names(A)%in%vars_cont]
  if (length(vars_cont)==1) {D=data.frame(D); names(D)[1]<-vars_cont;}
  p=ncol(D)
  TT = data.frame(nom=vars_cont,matrix(0,nrow=p,7));
  names(TT) <- c("var", "median", "mean", "sd", "min", "max","nb_na", "nb")
  c = 0;
  for (nv_ in vars_cont) {
    c          = c+1;
    TT[c,2] = round( stats::median(D[,nv_],na.rm = TRUE), nbdigits)
    TT[c,3] = round( base::mean(D[,nv_],na.rm = TRUE), nbdigits)
    TT[c,4] = round( stats::sd(D[,nv_],na.rm = TRUE), nbdigits)
    TT[c,5] = round( base::min(D[,nv_],na.rm = TRUE), nbdigits)
    TT[c,6] = round( base::max(D[,nv_],na.rm = TRUE), nbdigits)
    TT[c,7] = sum(is.na(D[,nv_]))
    TT[c,8] = sum(!is.na(D[,nv_]))
  }
  return(TT)
}
