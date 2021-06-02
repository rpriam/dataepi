#' A function for writing the full report from a data.frame
#' 
#' This function allows to generate a report from a binary variable for a 
#' disease, and a set of categorical variables for the exposures.
#' 
#' @param fullpathfile A character string with the full path for report saving.
#' @param formatfile A character string for the format of the file 
#'     with "doc","docx" or "rtf" for a word document and "tex" for a latex one.
#' @param A The data.frame for the analysis.
#' @param var_y  The variable for the disease yes/not.
#' @param vars_x  The variables for the table with glm for or and rr.
#' @param vars_cont  The name of the variables with continuous values.
#' @param vars_disc  The name of the variables with categorical values.
#' @param vars_int  The name of the variables with integer (ordered) values.
#' @param var_id  The name of the variable for the unique identifier per row.
#' @param list_supp A list with supplementary information for adding to 
#'                  the report, with optional entries.
#'  \describe{
#'   \item{where}{A brief descriptive of the place(s) where the study took place.}
#'   \item{who}{A brief descriptive of the population targeted.}
#'   \item{objective}{A brief descriptive of the objectives and purposes.}
#'   \item{disease}{The name of the disease.}
#'   \item{descriptive}{A brief descriptive of the disease.}
#'   \item{project}{For the type of project, for instance "descriptive".}
#'   \item{keywords}{A list of key words corresponding to the study or analysis.}
#'   \item{inex}{A descriptive for the criteria for the inclusion and exclusion.}
#'   \item{topics}{A list with terms to classify the variables and each subset
#'                 of variable names corresponding, for instance, "biological",
#'                 with blood test results, "socio-demographic" with age, gender,
#'                 etc.}
#' }
#' @param add_ORpca_ A boolean variable for including or not including ORpca 
#'                   in the table with odds ratio and relative risk.
#' 
# # @param groupvar an optional list for grouping the variables vars_x per topic
#' 
#' @return A list with following entries.
#' \describe{
#'   \item{au}{The resulting output from rep_compute().}
#'   \item{fullpathfile}{The copy of the variable from the parameters with 
#'                       the same name for the full path for report saving.}
#'   \item{note}{Message to user not null if the file exists already in order 
#'               to avoid file loss.}
#' }
#' 
#' @keywords 
#' @export
#' @examples
#' \dontrun{
#' data(DebTrivedi)
#' A=DebTrivedi
#' A$id=1:nrow(A)
#' A$hospbin  = as.integer(A$hosp>0)
#' var_id    = "id"
#' vars_cont = c("age","ofp","ofnp","opp","opnp","emer","numchron","hosp","school")
#' vars_disc = c("health","adldiff","region","black","gender","married","employed",
#'                "privins","medicaid")
#' vars_int  = NULL 
#' var_y     = "hospbin" #binary 0/1
#' label_y   = "hospibin"
#' A0 = A
#' fp = data_prepare(A,var_y,vars_cont,vars_disc,var_id)#,discretize_=TRUE)
#' A  = fp$A
#' A$age_3cl = as.character(1*(A$age<7.1)+2*(A$age>=7.1&A$age<7.7)+3*(A$age>=7.7))
#' A$age_3cl[A$age_3cl=="1"]="[6.6, 7.1)"
#' A$age_3cl[A$age_3cl=="2"]="[7.1, 7.7)"
#' A$age_3cl[A$age_3cl=="3"]="[7.7,10.9]"
#' vars_disc = c(vars_disc,"age_3cl")
#' vars_x =c(vars_disc,fp$var_disc_from_cont)[c(1:9,10)] #only discrete variables relevant
#' #au = dataepi::rep_compute(A, var_y, vars_x, vars_cont, vars_disc, vars_int, var_id)
#' wr = dataepi::rep_write("./report_dataepi.docx","docx",
#'                           A, var_y, vars_x, vars_cont, vars_disc, vars_int, var_id)
#' }
#' 
rep_write <- function(fullpathfile=NULL,formatfile="docx",
                      A,var_y,vars_x,vars_cont,vars_disc,vars_int,var_id,
                      list_supp=NULL,add_ORpca_=FALSE) { #,groupvar=NULL) {
  #if (!is.null(groupvar))  stopifnot(class(groupvar)=="list")
  #if (!is.null(groupvar))  stopifnot(groupvar)
  note=NULL
  if (file.access(fullpathfile, mode=0)!=0) { #not deletion of an existing file
    au = rep_compute(A,var_y,vars_x,vars_cont,vars_disc,vars_int,var_id);
    if (formatfile%in%c("doc","rtf","docx")) write_docx(fullpathfile,au,list_supp,add_ORpca_) 
    else if (formatfile%in%c("tex")) write_tex(fullpathfile,au,list_supp,add_ORpca_) 
    else note="file extension not recognized"
  } else {
    note=paste("name file already existing")
  }
  return(list(au=au,fullpathfile=fullpathfile,note))
}
