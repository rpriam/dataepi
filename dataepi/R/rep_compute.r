#' A function for generating the tables for the report from a data.frame
#' 
#' This function returns tables from a variable for disease 
#' which is binary and a set of variables which are categorical for exposures.
#' The descriptive statistics and statistical tests are computed and aggregated 
#' in several data.frames.
#' 
#' @param A The data.frame for the analysis.
#' @param var_y  The variable for the disease yes/not.
#' @param vars_x  The variables for the table with glm for or and rr.
#' @param vars_cont  The name of the variables with continuous values.
#' @param vars_disc  The name of the variables with categorical values.
#' @param vars_int  The name of the variables with integer (ordered) values.
#' @param var_id  The name of the variable for the unique identifier per row.
#' 
#' @return A list with the following entries.
#' \describe{
#'   \item{Anew}{The new matrix A after pre-traitment with data_prepare().}
#'   \item{desc_all}{The result from tab_contents().}
#'   \item{desc_cont}{The result from tab_desc_cont().}
#'   \item{desc_disc}{The result from tab_desc_disc().}
#'   \item{desc_biv}{The result from tab_desc2class_cont().}
#'   \item{test_tt}{The result from tab_tt2classes_cont().}
#'   \item{test_anova}{The result from tab_ttanova_cont().}
#'   \item{test_chi2}{The result from tab_chi2all().}
#'   \item{or}{The result from tab_all2x2() and stat_oddsratio().}
#'   \item{rr}{The result from tab_all2x2() and stat_relativerisk().}
#'   \item{gg}{The result from tab_glmorr().}
#'   \item{fv}{The result from viz_all2x2().}
#'   \item{args}{The variables at the call of the function.}
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
#'               "privins","medicaid")
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
#' au = dataepi::rep_compute(A, var_y, vars_x, vars_cont, vars_disc, vars_int, var_id)
#' }
#' 
rep_compute <- function(A,var_y,vars_x,vars_cont,vars_disc,vars_int,var_id) {
  stopifnot(length(unique(A[,var_y]))==2)
  #
  # cat("var_y=",var_y,"\n")
  # cat("var_x=",vars_x,"\n")
  # cat("vars_cont=",vars_cont,"\n")
  # cat("vars_disc=",vars_disc,"\n")
  # cat("vars_int=",vars_int,"\n")
  # cat("var_id=",var_id,"\n")
  #
  # cat("data_prepare\n")
  A                 <- data_prepare(A,var_y,vars_cont,vars_disc,var_id)$A
  # cat("tab_contents\n")
  desc_all          <- tab_contents(A)
  # cat("tab_desc_cont\n")
  desc_cont         <- tab_desc_cont(A,vars_cont)
  # cat("tab_desc_disc\n")
  desc_disc         <- tab_desc_disc(A,vars_disc)
  # cat("tab_desc2class_cont\n")
  desc_biv          <- tab_desc2class_cont(A,vars_cont,var_y)
  # cat("tab_tt2classes_cont\n")
  test_tt           <- tab_tt2classes_cont(A,vars_cont,var_y)
  # cat("tab_ttanova_cont\n")
  test_anova        <- tab_ttanova_cont(A,vars_cont,vars_disc)
  # cat("tab_chi2all\n")
  test_chi2         <- tab_chi2all(A,c(var_y,vars_disc),pvalue_seuil_ = 0.05)
  # cat("tab_all2x2\n")
  or                <- tab_all2x2(A,vars_disc,var_y,stat_oddsratio)
  # cat("tab_all2x2\n")
  rr                <- tab_all2x2(A,vars_disc,var_y,stat_relativerisk)
  # cat("tab_glmorr\n")
  gg                <- tab_glmorr(A,vars_x,var_y)
  # cat("viz_all2x2\n")
  fv                <- viz_all2x2(or$tabstat,g_=5,graph_=FALSE)
  #cat("keys\n")
  #keys              <- paste(or$TABSTAT[,1],or$TABSTAT[,2],sep = "")
  # cat("\n")
  #
  
  allargs           = list()
  allargs$A         = A
  allargs$var_y     = var_y
  allargs$vars_x    = vars_x
  allargs$vars_cont = vars_cont
  allargs$vars_disc = vars_disc
  allargs$vars_int  = vars_int
  allargs$var_id    = var_id
  
  return(list(Anew=A,desc_all=desc_all,desc_cont=desc_cont,desc_disc=desc_disc,desc_biv=desc_biv,
              test_tt=test_tt,test_anova=test_anova,test_chi2=test_chi2,or=or,rr=rr,gg=gg,fv=fv,
              args=allargs))
}
