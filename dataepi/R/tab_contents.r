#' A function for generating a very simple description of the variables 
#' in a data.frame
#' 
#' This function show the list of variables 
#' with the name of their classes of variable (numerical,integer,...) in r
#' and the number of unique values for each variable.
#' 
#' @param D The data.frame with the variables to describe.
#' 
#' @return A data.frame with each row for a variable from the input data.frame and 
#'         with the following columns.
#' \describe{
#'   \item{variable}{The variable name from the column names of D.}
#'   \item{r_class}{The type of the variable from the R langage.}
#'   \item{nblevels}{The total number of unique observations.}
#'   \item{nbobs}{The total number of non missing observations.}
#' }
#' Warning: with class factor, it may exist empty levels not counted. The 
#' function may be considered only after the function data_prepare().
#' 
#' @keywords 
#' @export
#' @examples
#' data(DebTrivedi)
#' A    <- DebTrivedi
#' A$id <- 1:nrow(A)
#' resu_<-tab_contents(A)
#' print(head(resu_))
#' 
tab_contents <- function(D) {
  cc_<-NULL;nl_<-NULL;nb_<-NULL;
  for (nv in names(D)) cc_<-c(cc_,class(D[,nv]))
  for (nv in names(D)) nl_<-c(nl_,length(unique(D[!is.na(D[,nv]),nv])))
  for (nv in names(D)) nb_<-c(nb_,length(D[!is.na(D[,nv]),nv]))
  return (data.frame(variable=names(D),r_class=cc_,nblevels=nl_,nbobs=nb_))
}
