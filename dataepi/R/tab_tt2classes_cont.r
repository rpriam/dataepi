#' A function for generating a table with the t-test of diverse variables
#'
#' This function returns the whole table with the t-tests 
#' from a set of continuous variables versus one categorical variable 
#' with two (or eventually more modalities).
#' 
#' @param A The data.frame with the data.
#' @param vars_cont The names of the continuous variables.
#' @param var_y The name of the categorical variable.
#' 
#' @return A data.frame with each row for a continuous variable and with the 
#' following columns.
#' \describe{
#'   \item{var1}{The name of the continuous variable from group 1.}
#'   \item{median1}{The median of the continuous variable from group 1.}
#'   \item{mean1}{The mean of the continuous variable from group 1.}
#'   \item{sd1}{The standard-deviation of the continuous variable from group 1.}
#'   \item{nb1}{The sample size of group 1.}
#'   \item{var2}{The name of the continuous variable from group 2.}
#'   \item{median2}{The median of the continuous variable from group 2.}
#'   \item{mean2}{The mean of the continuous variable from group 2.}
#'   \item{sd2}{The standard-deviation of the continuous variable from group 2.}
#'   \item{nb2}{The sample size of group 2.}
#'   \item{T_t.test (2cl)}{The statistics computed for the t-Student test.}
#'   \item{P_t.test (2cl)}{The p-value computed for the t-Student test.}
#'   \item{P<0.05 Power_t.t}{The power computed for the t-Student test.}
#' }
#' 
#' @keywords 
#' @export
#' @examples
#' data(DebTrivedi)
#' A         <- DebTrivedi
#' vars_cont <- c("age","ofp")
#' resu_    <- tab_tt2classes_cont(A,vars_cont,"gender")
#' print(resu_)
#' 
#tab_tt2classes_cont <- function(A,vars_cont,var_y) {
tab_tt2classes_cont <- function(A,vars_cont,var_y) {
  stopifnot(sum(vars_cont%in%names(A))>0)
  stopifnot(class(A)=="data.frame")
  #D=A[,c(vars_cont,var_y)]
  #D=A[,names(A)%in%]
  #cat("vars_cont=",vars_cont,"\n")
  D=A[,names(A)%in%c(vars_cont,var_y)]
  D=D[,c(vars_cont,var_y)]
  p = ncol(D)
  v  = as.character(D[,p])
  lv = unique(v[!is.na(v)])
  nv = length(unique(v[!is.na(v)]))
  nom_vars_continues = vars_cont
  p_vars_continues   = length(nom_vars_continues);
  names_TT           = c("var", "median", "mean", "sd", "min", "max","nb_na", "nb")
  TTs = NULL;
  for (l in 1:nv) {
    Dl_ = D[v==lv[l],-p]
    if (length(vars_cont)==1) {Dl_=data.frame(Dl_); names(Dl_)[1]<-vars_cont;}
    TT = tab_desc_cont(Dl_,vars_cont)
    names(TT) <- paste(names_TT,l,sep="")
    if (l==1) {TTs= TT[,-c(5,6,7)];} else {TTs=cbind(TTs,TT[,-c(5,6,7)]);}
  }
  TTs = cbind(TTs,numeric(p-1));
  TTs = cbind(TTs,numeric(p-1));
  TTs = cbind(TTs,numeric(p-1));
  TTs = cbind(TTs,numeric(p-1));
  names(TTs)[ncol(TTs)-3] = "T_t.test (2cl)" # pour deux facteurs uniquemenet ici
  names(TTs)[ncol(TTs)-2] = "P_t.test (2cl)" # pour deux facteurs uniquemenet ici
  names(TTs)[ncol(TTs)-1] = "P<0.05" # pour deux facteurs uniquemenet ici
  names(TTs)[ncol(TTs)-0] = "Power_t.t" # pour deux facteurs uniquemenet ici
  #cat(names(TTs),"\n")
  for (c in 1:p_vars_continues) {
    if ( TTs[c,5]>1 & TTs[c,10]>1 ) {
      TTs[c,ncol(TTs)-3] = gettextf ("%.2f", round( stats::t.test(D[v==lv[1],c],D[v==lv[2],c])$statistic,2))
      TTs[c,ncol(TTs)-2] = gettextf ("%.4f", round( stats::t.test(D[v==lv[1],c],D[v==lv[2],c])$p.value,4))
      TTs[c,ncol(TTs)-1] = as.numeric( stats::t.test(D[v==lv[1],c],D[v==lv[2],c])$p.value < 0.05 )
      #cat("min(TTs[c,5],TTs[c,10])=",min(TTs[c,5],TTs[c,10]),"\n")
      #cat("abs(TTs[c,3]-TTs[c,8])=",abs(TTs[c,3]-TTs[c,8]),"\n")
      #cat("min(TTs[c,4],TTs[c,9])=",min(TTs[c,4],TTs[c,9]),"\n")
      TTs[c,ncol(TTs)-0] = stats::power.t.test(min(TTs[c,5],TTs[c,10]),delta=abs(TTs[c,3]-TTs[c,8]),
                                        sd=min(TTs[c,4],TTs[c,9]), sig.level = 0.05)$power
      ##################### power_2t_unequal(n = 100, d, sigsq1, sigsq2, alpha = 0.05)
    } else {
      TTs[c,ncol(TTs)-3] = -1
      TTs[c,ncol(TTs)-2] = -1
      TTs[c,ncol(TTs)-1] = -1
      TTs[c,ncol(TTs)-0] = -1
    }
  }
  return(TTs)
}
