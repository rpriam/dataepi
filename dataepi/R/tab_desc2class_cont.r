#' A function for generating a table with statistics of continuous variables
#'
#' This function returns the whole table with the statistics 
#' number of observations, mean, standard-error, median, min, max
#' from a set of continuous variables versus one categorical variable 
#' with two (or eventually more modalities).
#' 
#' @param A The data.frame with the data.
#' @param vars_cont The names of the continuous variables.
#' @param var_y The name of the categorical variable.
#' @param nbdigits The number of decimals to keep.
#' 
#' @return A data.frame with each row for a continuous variable and with the 
#' following columns, where <modality> is one of the modalities of the variable 
#' whose name in written in var_y.
#' \describe{
#'   \item{MEAN_<modality>}{The mean of the continuous variable for the modality.}
#'   \item{STD_<modality>}{The standard-deviation of the variable for the modality.}
#'   \item{MD_<modality>}{The median of the continuous variable for the modality.}
#'   \item{MIN_<modality>}{The minimum of the continuous variable for the modality.}
#'   \item{MAX_<modality>}{The maximum of the continuous variable for the modality.}
#'   \item{Nnotna__<modality>}{The number of non missing observation for the modality.}
#' }
#' 
#' @keywords 
#' @export
#' @examples
#' data(DebTrivedi)
#' A         <- DebTrivedi
#' vars_cont <- c("age","ofp")
#' resu_    <- tab_desc2class_cont(A,vars_cont,"gender")
#' print(resu_)
#' 
tab_desc2class_cont <- function(A,vars_cont,var_y,nbdigits=2) {
  ncount = function(v) base::sum(!is.na(v))
  #stopifnot(class(A[,var_y])%inc("character","factor"))
  #for (nv in vars_cont) stopifnot(class(A[,nv])=="numerical")
  Ns     = stats::aggregate(A[,vars_cont], by=list(Category=A[,var_y]), FUN=ncount)
  MOYs   = stats::aggregate(A[,vars_cont], by=list(Category=A[,var_y]), FUN=base::mean, na.rm=TRUE)
  SDs    = stats::aggregate(A[,vars_cont], by=list(Category=A[,var_y]), FUN=stats::sd, na.rm=TRUE)
  MEDs   = stats::aggregate(A[,vars_cont], by=list(Category=A[,var_y]), FUN=stats::median, na.rm=TRUE)
  MINs   = stats::aggregate(A[,vars_cont], by=list(Category=A[,var_y]), FUN=base::min, na.rm=TRUE)
  MAXs   = stats::aggregate(A[,vars_cont], by=list(Category=A[,var_y]), FUN=base::max, na.rm=TRUE)
  
  MINs[,2:ncol(MOYs)] = round(MINs[,2:ncol(MINs)],nbdigits)
  MAXs[,2:ncol(MOYs)] = round(MAXs[,2:ncol(MAXs)],nbdigits)
  MEDs[,2:ncol(MOYs)] = round(MEDs[,2:ncol(MEDs)],nbdigits)
  MOYs[,2:ncol(MOYs)] = round(MOYs[,2:ncol(MOYs)],nbdigits)
  SDs[,2:ncol(SDs)]   = round(SDs[,2:ncol(SDs)],nbdigits)
  
  TT = base::data.frame(t(MOYs[,-1]),t(SDs[,-1]),t(MEDs[,-1]),t(MINs[,-1]),t(MAXs[,-1]),t(Ns[,-1]))
  names(TT) <- c(base::paste("MEAN",MOYs[,1],sep="_"),base::paste("STD",SDs[,1],sep="_"),base::paste("MD",MEDs[,1],sep="_"),
                 base::paste("MIN",MOYs[,1],sep="_"),base::paste("MAX",MOYs[,1],sep="_"),base::paste("Nnotna",Ns[,1],sep="_"))
  
  return(TT)
}
