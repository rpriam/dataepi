#' A function for computing the odds ratio from a table for an exposure
#' 
#' This function computes the odds ratio of a binary variable
#' from the crosstable or contingency table between a disease and an exposure.
#' 
#' @param X The object of type table of size 2x2.
#' 
#' @return A list with the following entries.
#' \describe{
#'   \item{stat}{The odds ratio from the table X.}
#'   \item{SE}{The standard-deviation of the odds ratio from the table X.}
#'   \item{I.95left}{The left part of the confidence interval at 0.95\%.}
#'   \item{I.95right}{The right part of the confidence interval at 0.95\%.}
#'   \item{name}{The name of the statistics, "OR".}
#'   \item{warning}{A boolean value for or for not having a=0 or b=0 or c=0 or d=0.}
#'   \item{a}{The value a from the 2x2 input table.}
#'   \item{b}{The value b from the 2x2 input table.}
#'   \item{c}{The value c from the 2x2 input table.}
#'   \item{d}{The value d from the 2x2 input table.}
#'   \item{table2x2}{The table from the input parameter.}
#' }
#' 
#' @keywords 
#' @export
#' @examples
#' X=matrix(c(2534,459,487,142),ncol=2,byrow=TRUE)
#' X=as.table(X)
#' colnames(X)<-c("0","1")
#' rownames(X)<-c("0","1")
#' print(X)
#' resu_=stat_oddsratio(X)
#' cat(resu_$name,"=",round(resu_$stat,2),
#' paste("(",round(resu_$stdstat,2),")",sep=""),
#' "\n")
#' 
stat_oddsratio <- function(X) {
  check_table2x2(X)
  # _____________________________
  # |           | Case  |Control|
  # |           | Ill   |Not Ill|  
  # -----------------------------
  # |Exposed    |   a   |    b  |
  # -----------------------------
  # |Not exposed|   c   |    d  |
  # -----------------------------
  #
  a = X[2,2]
  b = X[2,1]
  c = X[1,2]
  d = X[1,1]
  #
  OR = (a/(c+0)) / (b/(d+0))
  SE = exp(sqrt(1/(a+0)+1/(b+0)+1/(c+0)+1/(d+0)))
  I.95left = OR*exp(-1.96*sqrt(1/(a+0)+1/(b+0)+1/(c+0)+1/(d+0)))
  I.95right = OR*exp(+1.96*sqrt(1/(a+0)+1/(b+0)+1/(c+0)+1/(d+0)))
  list(stat=OR,stdstat=SE,I.95left=I.95left,I.95right=I.95right,
       name="OR",warning=(a==0|b==0|c==0|d==0),a=a,b=b,c=c,d=d,table2x2=X)
}
