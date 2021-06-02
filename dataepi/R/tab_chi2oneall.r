#' A function for computing the chi2 test from one against several variables
#'
#' This function take as input a data.frame and the names of the categorical 
#' variables plus one additional variable in order to compute the tests 
#' and aggregates them in a data.frame, without filtering.
#' The tests are computed with a loop on the whole set in vars_disc.
#' 
#' @param A The data.frame with the variables to test.
#' @param vars_disc The vector with the string names of the variables to test.
#' @param var_y The string name of one variable.
#' 
#' @return A data.frame with with the chi2 tests by pairs of variables 
#'   with the following columns
#'   \describe{
#'   \item{row}{The variables from the input vector of variable names vars_disc.}
#'   \item{col}{The variable with its name in the input variable var_y.}
#'   \item{nbr}{The number of modalities of the first variable.}
#'   \item{nbc}{The number of modalities of the second variable.}
#'   \item{chi2}{The statistics as computed from the chi2 test.}
#'   \item{df}{The number of free parameters in the chi2 test.}
#'   \item{p.val}{The p-value from the chi2 test.}
#'   \item{mnij}{The minimum coun in the cells of the table.}
#'   \item{p.val.e}{The p-value from the exact Fisher test.}
#'   \item{pow}{The power (if available) from the chi2 test.}
#'   \item{nb}{The total number of counts in the table.}
#'  }
#' 
#' @keywords 
#' @export
#' @examples
#' data(DebTrivedi)
#' A         <- DebTrivedi
#' vars_disc <- c("health","gender","region")
#' var_y     <- "hosp"
#' resu_    <- tab_chi2oneall(A,vars_disc,var_y)
#' print(head(resu_))
#' print(head(resu_[resu_$p.val<=0.05,]))
#' 
tab_chi2oneall <- function(A,vars_disc,var_y) {
  D=A[,c(vars_disc,var_y)]
  p=base::ncol(D)
  CHI2 = base::matrix(0,ncol=11,nrow=p)
  CHI2 = base::data.frame(CHI2)
  #names(CHI2) <- c("row","col","nbrow","nbcol","chi2","df","p.value","min_nij","p.value_EXACT", "power_chi2", "nb")
  names(CHI2) <- c("row","col","nbr","nbc","chi2","df","p.val","mnij","p.val.e", "pow", "nb")
  ##
  cc=0;
  j2=p
  for (j1 in 1:(p-1)) {
    TT <- base::table(D[,j1],D[,j2]);
    #length(unique(D[,j1]))>1 && length(unique(D[,j2]))>1 && 
    if (base::min(base::colSums(TT>0))>=1 & base::min(base::rowSums(TT>0))>=1 & 
        base::nrow(TT)>1 & base::ncol(TT)>1) {
      #print(TT);
      #print(dim(as.matrix(TT)))
      ##
      # UU <<- TT;
      # jj1 <<- j1
      # jj2 <<- j2
      # D1 <<- D[,j1]
      # D2 <<- D[,j2]
      ##
      test_apprx <- stats::chisq.test(D[,j1], D[,j2]);
      #pvalue_apprx = test_apprx$p.value;
      #test_exact <- fisher.test(TT,simulate.p.value=TRUE);
      #pvalue_exact = test_exact$p.value;
      TT = TT[base::rowSums(TT>0)>0,base::colSums(TT>0)>0];
      # VV <<- TT
      probs = numeric(35)
      for (llpp in 1:35) probs[llpp] = stats::fisher.test(TT,simulate.p.value=TRUE)$p.value;
      pvalue_exact = base::mean(probs);
      #test_exact2<- coin::chisq_test(TT, distribution = "exact");
      effect_size = pwr::ES.w2(TT/base::sum(TT))
      if (!is.nan(effect_size)) {
        power_chi2  = pwr::pwr.chisq.test(w = effect_size, N = base::nrow(A), df = test_apprx$parameter, sig.level = 0.015)$power
      } else {
        power_chi2  = -1;
      }   
      ######if (pvalue_exact<=0.015 ) {
      #if (test_exact$p.value<=0.05 ) {
      cc=cc+1;
      CHI2[cc,1] = base::names(D)[j1];
      CHI2[cc,2] = base::names(D)[j2];
      CHI2[cc,3] = base::length(unique(D[!base::is.na(D[,j1]),j1]));
      CHI2[cc,4] = base::length(unique(D[!base::is.na(D[,j2]),j2]));
      CHI2[cc,5] = base::gettextf ("%.2f", base::round(test_apprx$statistic,2));
      CHI2[cc,6] = test_apprx$parameter;
      CHI2[cc,7] = base::gettextf ("%.4f", base::round(test_apprx$p.value, 4));
      CHI2[cc,8] = base::min(TT);
      ##CHI2[cc,9] = gettextf ("%.4f", round(test_exact$p.value, 4));
      CHI2[cc,9] = base::gettextf ("%.4f", base::round(pvalue_exact, 4));
      CHI2[cc,10]= base::gettextf ("%.4f", base::round(power_chi2,4));
      CHI2[cc,11]= base::sum(TT);
      ######}
    }
  }
  ##
  CHI2=CHI2[1:cc,]
  return(CHI2);
}
