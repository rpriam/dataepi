#' A function for describing the data.frame for the categorical variables
#'
#' This function take as input a data.frame and the names of the categorical 
#' variables in order to print in a table the names, frequencies per modalities,
#' percentable per modalities, and the number of levels and number of NA values.
#' 
#' @param A The data.frame with the variables to test.
#' @param vars_disc  The names of the variables with categorical values.
#' @param nbdigits The number of decimals to keep.
#' 
#' @return A data.frame with each row for a categorical variable and with the 
#' following columns.
#' \describe{
#'   \item{var}{The name of a variable from the vector of names vars_disc.}
#'   \item{nb_na}{The number of missing values.}
#'   \item{nblevel}{The total number of unique observations.}
#'   \item{nbperlevel}{The numbers of observations by modality.}
#'   \item{propperlevel}{The proportions of observations by modality.}
#'   \item{namelevel}{The corresponding modality names.}
#' }
#' 
#' @keywords 
#' @export
#' @examples
#' data(DebTrivedi)
#' A         <- DebTrivedi
#' vars_disc <- c("health","gender","region")
#' resu_    <- tab_desc_disc(A,vars_disc)
#' print(resu_) 
#' 
tab_desc_disc <- function(A,vars_disc,nbdigits=2) {
  D=A[,names(A)%in%c(vars_disc)]
  p=ncol(D)
  TT  = data.frame(vars_disc,numeric(p),numeric(p),
                  character(p),character(p),character(p))
  TT[,4] = as.character(TT[,4])
  TT[,5] = as.character(TT[,5])
  TT[,6] = as.character(TT[,6])
  names(TT) <- c("var","nb_na", "nblevel","nbperlevel","propperlevel","namelevel")
  c = 0;
  for (nv_ in vars_disc) {
    c          = c+1;
    #cat("var=",nv_,"\n");
    TT[c,2] = sum(is.na(D[,nv_]))
    TT[c,3] = length(unique(D[!is.na(D[,nv_]),nv_]))
    ttnv    = table( sort(D[,nv_]) )
    TT[c,4] = paste(ttnv[ttnv>0],collapse=';')
    TT[c,5] = paste(round(ttnv[ttnv>0]/sum(ttnv[ttnv>0]),nbdigits),collapse=';')
    TT[c,6] = paste(as.character(unique( sort(D[!is.na(D[,nv_]),nv_]) )),collapse=';')
  }
  return(TT)
}
