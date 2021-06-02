#' A function for generating a table with statistics such as odds ratios 
#' 
#' This function returns the whole table with the statistics 
#' odd ratio, risk ratio of any other defined by the user 
#' from a set of binary variables (exposures) vs one binary variable (disease).
#' 
#' @param A The data.frame with the data.
#' @param vars_disc  The name of the variables with categorical values.
#' @param var_y The name of the categorical variable.
#' @param stat_f A function such as stat_oddsratio,stat_relativerisk,...
#' 
#' @return A list with following entries.
#' \describe{
#'   \item{tabstat}{A data.frame with eight columns: the variable name, the 
#'                  modality or level name, the contents of the 2x2 contingency 
#'                  table with A for a, B for b, C for c and D for D, and the 
#'                  corresponding statistics from the function stat_... in the 
#'                  list of parameters, for instance OR and SE_OR for the odds 
#'                  ratios and their standard-deviations.}
#'   \item{binarized_1_s}{The binarized variable for or not the variable equal to 
#'                       modality, in a list of lists.}
#'   \item{binarized_0_s}{The binarized variable for or not the variable not equal 
#'                       to modality, in a list of lists.}
#'   \item{tables2x2}{The contingency tables for all the variables and all the 
#'                       modalities in the format of a list a lists, from the 
#'                       function stat_f in the parameters.}
#' }
#' 
#' @keywords 
#' @export
#' @examples
#' library(dataepi)
#' data(DebTrivedi)
#' A         <- DebTrivedi
#' A$hospbin <- as.integer(A$hosp>0)
#' vars_disc <- c("health","gender","region")
#' var_y     <- "hospbin"
#' resu_or   <- dataepi::tab_all2x2(A,vars_disc,var_y,dataepi::stat_oddsratio)
#' print(resu_or$tabstat)
#' 
tab_all2x2 <- function(A,vars_disc,var_y,stat_f=NULL) {
  stopifnot(length(unique(A[!is.na(A[,var_y]),var_y]))==2)
  binarized_1_s = list()
  binarized_0_s = list()
  for (nv in vars_disc) {
    v_ = as.character(A[,nv])
    lvs = unique(v_)
    lvs = sort(lvs)
    binarized_1_s[[nv]] = list()
    binarized_0_s[[nv]] = list()
    for (l in lvs) {
      binarized_1_s[[nv]][[l]] = as.character(as.numeric(v_==l))
      binarized_0_s[[nv]][[paste(l,"_",sep="")]] = as.character(as.numeric(v_!=l))
    }
  }
  tts <- list()
  out <- list()
  cc=0
  for (nv in vars_disc) {
    V_with_modalities = as.character(A[,nv])
    lvs <- unique(V_with_modalities)
    for (l in lvs[!is.na(lvs)]) {
      V_onemodality_01 = binarized_1_s[[nv]][[l]]
      TT <- table(A[,var_y],V_onemodality_01)
      stopifnot(rownames(TT)[1] == "0")
      stopifnot(rownames(TT)[2] == "1")
      stopifnot(colnames(TT)[1] == "0")
      stopifnot(colnames(TT)[2] == "1")
      stats =  stat_f(TT)
      cc=cc+1
      out[[as.character(cc)]] = c(nv,l,stats$a,stats$b,stats$c,stats$d,
                                  base::gettextf ("%.2f", round(stats$stat,2)),
                                  base::gettextf ("%.2f", round(stats$I.95left,2)),
                                  base::gettextf ("%.2f", round(stats$I.95right,2))
                                  )
      tts[[nv]][[l]] <- TT
    }
  }
  out <- data.frame(do.call(rbind,out))
  names(out)<- c("variable","modality","A","B","C","D",stats$name,
                 paste("I.95l_",base::trimws(stats$name),sep=""),paste("I.95r_",base::trimws(stats$name),sep=""))
  return(list(tabstat=out,binarized_1_s=binarized_1_s,binarized_0_s=binarized_0_s,tables2x2=tts))
}
