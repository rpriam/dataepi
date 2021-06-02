#' A function for computing the chi2 test from several variables of a data.frame  
#'
#' This function take as input a data.frame and the names of the categorical 
#' variables in order to compute the tests and aggregates them in a data.frame.
#' The resulting data.frame contains the chi-square tests for each variable in 
#' vars_disc minus the last which appears in the second column. There are two 
#' loops: k in (1;p-1) while l in (k+1;p) in order to compute the upper part 
#' to the diagonal.
#' 
#' @param A The data.frame with the variables to test.
#' @param vars_disc The vector with the names of the variables to test.
#' @param pvalue_seuil_ The maximal p-value for keeping the pairs of variables.
#' 
#' @return A list with three entries
#' \describe{
#'   \item{tabchi2}{The data.frame with the chi2 tests by pairs of variables 
#'   with the following columns}
#'      \describe{
#'      \item{row}{The variable from the rows of the contingency table.}
#'      \item{col}{The variable from the cols of the contingency table.}
#'      \item{nbr}{The number of modalities of the first variable.}
#'      \item{nbc}{The number of modalities of the second variable.}
#'      \item{chi2}{The statistics as computed from the chi2 test.}
#'      \item{df}{The number of free parameters in the chi2 test.}
#'      \item{p.val}{The p-value from the chi2 test.}
#'      \item{mnij}{The minimum coun in the cells of the table.}
#'      \item{p.val.e}{The p-value from the exact Fisher test.}
#'      \item{pow}{The power (if available) from the chi2 test.}
#'      \item{nb}{The total number of counts in the table.}
#'   }
#'   \item{pairs_no_pchi2}{A data.frame with by rows the pairs of variables with 
#'                         no chi2 test available because of their corresponding 
#'                         contingency table.}
#'   \item{pairs_large_pchi2}{A data.frame with by rows the pairs of variables 
#'                           with no chi2 test available because their p-value is 
#'                           larger than the threshold.}
#' }
#' 
#' @keywords 
#' @export
#' @examples
#' data(DebTrivedi)
#' A         <- DebTrivedi
#' vars_disc <- c("health","gender","region")
#' resu_    <- tab_chi2all(A,vars_disc,0.05)
#' print(head(resu_$tabchi2))
#' 
tab_chi2all <- function(A,vars_disc,pvalue_seuil_=0.015) {
  D=A[,vars_disc]
  p=base::ncol(D)
  n=base::nrow(D)
  CHI2 = base::matrix(0,ncol=11,nrow=p*(p-1)/2)
  CHI2 = base::data.frame(CHI2)
  #names(CHI2) <- c("row","col","nbrow","nbcol","chi2","df","p.value","min_nij","p.value_EXACT", "power_chi2", "nb")
  names(CHI2) <- c("row","col","nbr","nbc","chi2","df","p.val","mnij","p.val.e", "pow", "nb")
  ##
  pbchi2=base::list()
  ccpb=0
  largechi2=base::list()
  ccsma=0
  cc=0;
  for (j1 in 1:(p-1)) {
    for (j2 in (j1+1):p) {
      TT = base::table(D[,j1],D[,j2]);
      #length(unique(D[,j1]))>1 && length(unique(D[,j2]))>1 && 
      if (base::min(colSums(TT>0))>=1 & base::min(rowSums(TT>0))>=1 & 
          base::nrow(TT)>1 & base::ncol(TT)>1) {
        test_apprx <- stats::chisq.test(D[,j1], D[,j2]);
        probs = base::numeric(35)
        for (llpp in 1:35) probs[llpp] = stats::fisher.test(TT,simulate.p.value=TRUE)$p.value;
        pvalue_exact = base::mean(probs);
        effect_size = pwr::ES.w2(TT/sum(TT))
        if (!base::is.nan(effect_size)) {
          power_chi2  = pwr::pwr.chisq.test(w = effect_size, N = n, df = test_apprx$parameter, sig.level = 0.05)$power
        } else {
          power_chi2  = -1;
        }   
        #
        if (pvalue_exact<=pvalue_seuil_) {
          #if (test_exact$p.value<=0.05 ) {
          cc=cc+1;
          CHI2[cc,1] = base::names(D)[j1];
          CHI2[cc,2] = base::names(D)[j2];
          CHI2[cc,3] = base::length(unique(D[!is.na(D[,j1]),j1]));
          CHI2[cc,4] = base::length(unique(D[!is.na(D[,j2]),j2]));
          CHI2[cc,5] = base::gettextf ("%.2f", base::round(test_apprx$statistic,2));
          CHI2[cc,6] = test_apprx$parameter;
          CHI2[cc,7] = base::gettextf ("%.4f", base::round(test_apprx$p.value, 4));
          CHI2[cc,8] = base::min(TT);
          ##CHI2[cc,9] = gettextf ("%.4f", round(test_exact$p.value, 4));
          CHI2[cc,9] = base::gettextf ("%.4f", base::round(pvalue_exact, 4));
          CHI2[cc,10]= base::gettextf ("%.4f", base::round(power_chi2,4));
          CHI2[cc,11]= base::sum(TT);
        } else {
          ccsma=ccsma+1
          largechi2[[base::as.character(ccsma)]] = c(base::names(D)[j1],base::names(D)[j2])
        }
      } else {
        ccpb=ccpb+1
        pbchi2[[base::as.character(ccpb)]] = c(base::names(D)[j1],base::names(D)[j2])
        #cat("row=",names(D)[j1]," col=",names(D)[j2],"\n")
      }
    }
  }
  CHI2=CHI2[1:cc,]
  return(list(tabchi2=CHI2,pairs_no_pchi2=base::do.call(base::rbind,pbchi2),
              pairs_large_pchi2=do.call(base::rbind, largechi2)))
}
