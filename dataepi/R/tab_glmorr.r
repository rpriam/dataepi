#' A function for computing the OR from the logistic regression
#'
#' This function take the data.frame with all the variable and 
#' construct a new matrix with the odds ratio from a logistic regression,
#' from the full model and from the reduced model after a selection by AIC.
#' 
#' @param A The data.frame for the analysis.
#' @param vars_x  The variables for the table with glm for or and rr.
#' @param var_y The variable for the disease yes/not.
#' 
#' @return A list with following entries.
#' \describe{
#'   \item{frm}{The formula for the logistic regression for computing the 
#'              odds ratio on a full model before selecting the variables.}
#'   \item{tabl_coeff_full}{The resulting table of coefficients regression 
#'                          with all the variables.}
#'   \item{tabl_coeff_small}{The resulting table of coefficients regression 
#'                          with the variable kept after a selection by AIC.}
#'   \item{allcoeffs}{The two tables of coefficients side to side for 
#'                    comparison purpose.}
#'   \item{allors}{The odds ratios from the two tables of coefficients side to 
#'                  side in allcoeffs.}
#'   \item{fit_full}{The r object from the glm regression with all variables.}
#'   \item{fit_small}{The r object from the glm regression with selected variables.}
#'   \item{yX}{The dataframe restricted to the variable in vars_x and var_y.}
#' }
#' 
#' @keywords 
#' @export
#' @examples
#' \dontrun{
#' data(DebTrivedi)
#' A=DebTrivedi
#' A$hospbin = as.integer(A$hosp>0)
#' vars_x = c("health","region","gender","married","employed")
#' var_y  = "hospbin"
#' gg     = dataepi::tab_glmorr(A,vars_x,var_y) 
#' print(gg$allors)
#' }
#' 
tab_glmorr <- function(A,vars_x,var_y) {
  A=na.omit(A)
  yX           <- data.frame(y=as.numeric(A[,var_y]),A[,c(vars_x)])
  names(yX)[1] <- var_y
  frm          <- stats::as.formula(paste(var_y, "~", paste(vars_x, collapse="+"), sep = ""))
  fit_full     <- stats::glm(frm, data=yX, family=stats::binomial(logit))
  #print(summary(fit_full))
  tabl_coeff_full = stats::coef(summary(fit_full))
  tabl_coeff_full = round_all(tabl_coeff_full)
  tabl_coeff_full = data.frame(tabl_coeff_full,round_all(stats::confint(fit_full)))
  tabl_coeff_full$vars = rownames(tabl_coeff_full)
  ###coefficient_full = coefficients(fit_full)
  ###std_coeff___full = sqrt(diag(vcov(fit_full)))
  ###int_coeff_full = confint(fit_full)
  #print(head(tabl_coeff_full))
  fit_small <- MASS::stepAIC(fit_full, trace=FALSE) 
  # a ajouter: selection modalite et conservation de variables 
  ###print(summary(fit_small))
  ###Anova(fit_small) 
  ###anova(fit_full,fit_small)
  tabl_coeff_small = stats::coef(summary(fit_small))
  tabl_coeff_small = round_all(tabl_coeff_small)
  tabl_coeff_small = data.frame(tabl_coeff_small)
  tabl_coeff_small = data.frame(tabl_coeff_small,round_all(stats::confint(fit_small)))
  tabl_coeff_small$vars = rownames(tabl_coeff_small)
  ###coefficient_small = coefficients(fit_small)
  ###std_coeff___small = sqrt(diag(vcov(fit_small)))
  ###int_coeff_small = confint(fit_small)
  #print(head(tabl_coeff_small))
  tabl_coeff_full_  = tabl_coeff_full[,c(7,1,5,6,4)]
  tabl_coeff_full_$idorder = 1:nrow(tabl_coeff_full_)
  tabl_coeff_small_ = tabl_coeff_small[,c(7,1,5,6,4)]
  allcoeffs=( merge(tabl_coeff_full_,tabl_coeff_small_,by = "vars",all = TRUE) )
  allcoeffs=allcoeffs[order(allcoeffs$idorder),]
  allcoeffs=allcoeffs[,-which(names(allcoeffs)=="idorder")]
  names(allcoeffs)<-c("vars","coef_f","coef02.5_f","coef97.5_f", "pval_f","coef_r","coef02.5_r","coef97.5_r", "pval_r")
  allors = allcoeffs
  allors[,2:4] = round_all(exp(allors[,2:4]))
  allors[,6:8] = round_all(exp(allors[,6:8]))
  names(allors)<-c("vars","or_f","or02.5_f","or97.5_f", "pval_f","or_r","or02.5_r","or97.5_r", "pval_r")
  
  for (j in 1:ncol(allcoeffs)) allcoeffs[,j] <- as.character(allcoeffs[,j])
  for (j in 2:ncol(allcoeffs)) for (i in 1:nrow(allcoeffs)) {
    if (!is.na(as.numeric(as.character(allcoeffs[i,j])))) 
      allcoeffs[i,j] <- base::gettextf ("%.2f", round(as.numeric(as.character(allcoeffs[i,j])),2))
  }  
  allcoeffs[is.na(allcoeffs)] = " "
  allcoeffs[allcoeffs=="0"] = zero_all()
  
  for (j in 1:ncol(allors)) allors[,j] <- as.character(allors[,j])
  for (j in 2:ncol(allors)) for (i in 1:nrow(allors)) {
    if (!is.na(as.numeric(as.character(allors[i,j])))) 
      allors[i,j] <- base::gettextf ("%.2f", round(as.numeric(as.character(allors[i,j])),2))
  }
  allors[is.na(allors)] = " "
  allors[allors=="0"] = zero_all()
  #print(head(allcoeffs))
  #print(head(allors))
  return(list(frm=frm, 
              tabl_coeff_full=tabl_coeff_full,
              tabl_coeff_small=tabl_coeff_small,
              allcoeffs=allcoeffs,allors=allors,              
              fit_full=fit_full, 
              fit_small=fit_small,
              yX=yX))
}
