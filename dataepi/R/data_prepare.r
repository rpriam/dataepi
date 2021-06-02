#' A function for preparing the data.frame before the analysis
#'
#' This function takes as input a data.frame and the names of the 
#' variables in order check the variables and their r types.
#' 
#' @param A The data.frame with the variables to test.
#' @param var_y  The variable for the disease yes/not.
#' @param vars_cont  The names of the variables with continuous values.
#' @param vars_disc  The names of the variables with categorical values.
#' @param vars_int  The names of the variables with integer (ordered) values.
#' @param var_id  The name of the variable for the unique identifier per row.
#' 
#' @return A list with the following entries.
#' \describe{
#'   \item{A}{The data.frame from the dataset after checking and updating.}
#'   \item{var_disc_from_cont}{The names of the discretized variables from continuous ones (not implemented).}
#'   \item{vars_disc}{The vectors of names received from the input parameters.}
#'   \item{vars_cont}{The vectors of names received from the input parameters.}
#'   \item{var_y}{The same name received from the input parameters.}
#' }
#' 
#' @keywords 
#' @export
#' @examples
#' 
#' data(DebTrivedi)
#' A         <- DebTrivedi[,c("age","ofp","gender","region","health")]
#' A$id      <- 1:nrow(A)
#' A$hospbin <- DebTrivedi$hosp>0
#' vars_cont <- c("age","ofp")
#' vars_disc <- c("gender","region","health")
#' var_id    <- "id"
#' var_y     <- "hospbin"
#' fp = data_prepare(A,var_y,vars_cont,vars_disc,var_id)
#' print(head(fp$A))
#' 
data_prepare <- function(A,var_y=NULL,vars_cont=NULL,vars_disc=NULL,vars_int=NULL,var_id=NULL) {#,discretize_=FALSE) {
  n0=nrow(A)
  p0=ncol(A)
  
  #string name with integer value at last character not allowed
  for (nv in names(A)) {stopifnot(!(substr(nv,nchar(nv),nchar(nv))%in%paste(0:9)));}
  stopifnot(sum(!vars_cont%in%names(A))<1)
  stopifnot(sum(!vars_disc%in%names(A))<1)
  stopifnot(var_y%in%names(A))
  
  if (!is.null(var_id)) {
    stopifnot(var_y%in%names(A))
    stopifnot(length(unique(A[,var_id]))==nrow(A))
  } else {
    var_id="id"; A$id=1:nrow(A);
    # if (!"id"%in%names(A)) { var_id="id"; A$id=1:nrow(A); }
    # else { A=data.frame(A,1:nrow(A));
    #                         j=0; ko=TRUE; 
    #                         while(ko==TRUE) { j=j+1; ko=paste("id",j,sep="")%in%names(A); }
    #                         var_id = paste("id",j,sep="");
    #                         names(A)[ncol(A)] <- var_id;
    #                       }
  }
  
  if (!is.null(vars_int)) {
    stopifnot(sum(vars_int%in%names(A))>0)
    for (nv in vars_int) {
      var_     <- A[,nv]
      var_     <- as.integer(as.character(var_))
      A[,nv]   <- var_
    }
  }
  
  if (!is.null(vars_cont)) {
    stopifnot(sum(vars_cont%in%names(A))>0)
    for (nv in vars_cont) {
      var_     <- A[,nv]
      var_     <- as.numeric(as.character(var_))
      A[,nv]   <- var_
    }
  }
  
  # if (discretize_) {
  #   stopifnot(sum(vars_cont%in%names(A))>0)
  #   for (nv in vars_cont) {
  #     var_     <- A[,nv]
  #     var_     <- as.numeric(as.character(var_))
  #     D        <- data.frame(
  #       v2=as.character(Hmisc::cut2(var_,g = 2)), 
  #       v3=as.character(Hmisc::cut2(var_,g = 3)),
  #       v4=as.character(Hmisc::cut2(var_,g = 4)),
  #       v4=as.character(Hmisc::cut2(var_,g = 5)))
  #     names(D) <- paste(nv,"_",c(2,3,4,5),"cl",sep='')
  #     A        <- data.frame(A,data.frame(lapply(D, as.character), stringsAsFactors=FALSE))
  #   }
  # }
  #vars_disc = c(vars_disc,names(A)[(p0+1):ncol(A)])
  
  if (!is.null(vars_disc)) {
    stopifnot(sum(vars_cont%in%names(A))>0)
    for (nv in vars_disc) {
      var_     <- A[,nv]
      var_     <- as.character(as.character(var_))
      A[,nv]   <- var_
    }
  }
  
  #A[,var_y]=as.character(A[,var_y])
  A[,var_y] = as.numeric(as.character(A[,var_y]))  ##stopifnot(class(A[,var_y])=="numeric")
  A = A[is.finite(A[,var_y]) & !is.na(A[,var_y]),] ##stopifnot(sum(is.finite(A[,var_y]) & !is.na(A[,var_y]))<1)
  
  return(list(A=A,var_disc_from_cont=names(A)[(p0+1):ncol(A)],
         vars_disc=vars_disc,vars_cont=vars_cont,var_y=var_y))
}

