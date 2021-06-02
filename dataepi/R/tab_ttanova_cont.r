#' A function for generating a table with the anova of diverse variables
#'
#' This function returns the whole table with the anova tests 
#' from a set of continuous variables and categorical variables 
#' with different number of modalities, plus other related tests.
#' 
#' @param A The data.frame with the data.
#' @param vars_cont The names of the continuous variables.
#' @param vars_disc The names of the categorical variables.
#' 
#' @return A list with the following entries.
#' \describe{
#'   \item{tabstat}{
#' A data.frame with each row for a continuous variable and a 
#' categorical variable (binary or poly) with the following columns.
#' \describe{
#'   \item{var_cont}{The name of the continuous variable.}
#'   \item{var_disc}{The name of the discrete variable.}
#'   \item{n1}{The size of groupe 1.}
#'   \item{n2}{The size of groupe 2.}
#'   \item{...}{...}
#'   \item{ng}{The size of groupe g (if it exists for the variable in var_disc).}
#'   \item{norm1}{The p-value of the normality test from group 1.}
#'   \item{norm2}{The p-value of the normality test from group 2.}
#'   \item{...}{...}
#'   \item{normg}{The p-value of the normality test from group g (if exists).}
#'   \item{test12_vr}{The p-value of the equality test of the variances for two groups.}
#'   \item{test12_eq}{The p-value of the equality t-test of the means for two groups.}
#'   \item{test12_gt}{The p-value of the equality t-test of the means for two groups with option "greater".}
#'   \item{test12_ls}{The p-value of the equality t-test of the means for two groups with option "less".}
#'   \item{test12_wx_eq}{The p-value of the wilkox rank sum (not paired) equality test of the means for two groups.}
#'   \item{test12_wx_gq}{The p-value of the wilkox rank sum (not paired) equality test of the means for two groups  with option "greater".}
#'   \item{test12_wx_ls}{The p-value of the wilkox rank sum (not paired) equality test of the means for two groups  with option "less".}
#'   \item{test_vr}{The p-value of the equality test of the variances for g>2 groups.}
#'   \item{test_aov}{The p-value of the equality test of the means for g>2 groups.}
#'   \item{test_aov_check}{The p-value from the normality test of the residual from the anova test for g>2 groups.}
#'   \item{test_welch}{The p-value from the welch test for g>=2 groups and variances not equal, under normality and variances supposed not equal.}
#'   \item{test_krusk}{The p-value from the kruskal-wallis rank sum test for g>=2 groups to check the equality in distribution of the means.}
#'  }
#' }
#' \item{pairs_no_panova}{A list of pairs of variables with non available anova test.}
#' }
#' @keywords 
#' @export
#' @examples
#' data(DebTrivedi)
#' A         <- DebTrivedi
#' vars_cont <- c("age","ofp")
#' vars_disc <- c("gender","region","health")
#' resu_    <- tab_ttanova_cont(A,vars_cont,vars_disc)
#' print(resu_)
#' 
tab_ttanova_cont <- function(A,vars_cont,vars_disc) {
  stopifnot(sum(vars_cont%in%names(A))>0)
  stopifnot(sum(vars_disc%in%names(A))>0)
  nlevelmax=0;
  for (nv_ in vars_disc) {
    nlevel_ = length(unique(as.character(A[!is.na(A[,nv_]),nv_])))
    datacl_ = as.character( A[,nv_] )
    datacl_ = datacl_[!is.na(datacl_)]
    levell_ = unique( datacl_[!is.na(datacl_)] )
    nlevel_ = length( levell_ )
    stop_ = 0;
    for (nu_ in vars_cont){
      datanv_ = A[!is.na(as.character( A[,nv_] )),nu_]
      for (l in 1:nlevel_) if (sum(is.finite(datanv_[datacl_==levell_[l]]))<3) stop_=1;
    }
    if (nlevel_>nlevelmax & stop_==0) nlevelmax=nlevel_;
  }
  
  names_resu_  <- c( paste("n",1:nlevelmax,sep='') , paste("norm",1:nlevelmax,sep='') )
  names_resu_  <- c(names_resu_, 
                    "test12_vr", "test12_eq", "test12_gt", "test12_ls", "test12_wx_eq", "test12_wx_gq", "test12_wx_ls",
                    "test_vr", "test_aov", "test_aov_check", "test_welch", "test_krusk"
  )
  #resus_       <- data.frame(var_cont=character(),var_disc=character(),character(0,nrow=0,ncol=length(names_resu_)))
  resus_       <- data.frame(var_cont=character(),var_disc=character(),as.data.frame(t(character(length(names_resu_))))[-1,])
  names(resus_)<- c("var_cont", "var_disc", names_resu_)
  resus_[,"var_cont"]=as.character(resus_[,"var_cont"])
  resus_[,"var_disc"]=as.character(resus_[,"var_disc"])
  
  ccpb      = 0
  pbttanova = list()
  
  names_resus_0 = names(resus_)
  
  for (nu_ in vars_disc) {
    
    datacl_ = as.character( A[,nu_] )
    datacl_ = datacl_[!is.na(datacl_)]
    
    levell_ = unique( datacl_[!is.na(datacl_)] )
    levell_ = sort(levell_)
    nlevel_ = length( levell_ )
    
    for (nv_ in vars_cont) {
      
      datanv_ = A[!is.na(as.character( A[,nu_] )),nv_]
      
      Ayg = data.frame(y=datanv_,g=datacl_)
      Ayg = stats::na.omit(Ayg)
      
      stop_ = 0;
      for (l in 1:nlevel_) if (sum(is.finite(datanv_[datacl_==levell_[l]]))<3) stop_=1;
      
      resu_ = NULL;
      
      if (stop_==0) {
        if (nlevel_==2) {
          
          if (length(unique(datanv_[datacl_==levell_[1]]))>1) {
            norm1_  = stats::shapiro.test(datanv_[datacl_==levell_[1]])$p.value;
          } else {
            norm1_  = -1;
          }
          if (length(unique(datanv_[datacl_==levell_[2]]))>1) {
            norm2_  = stats::shapiro.test(datanv_[datacl_==levell_[2]])$p.value;
          } else {
            norm2_  = -1  
          }
          resu_ = data.frame(var_cont   = nv_,
                             var_disc   = nu_,
                             n1         = length(datanv_[datacl_==levell_[1]]),
                             n2         = length(datanv_[datacl_==levell_[2]]),
                             norm1      = norm1_, #shapiro.test(datanv_[datacl_==levell_[1]])$p.value,
                             norm2      = norm2_, #shapiro.test(datanv_[datacl_==levell_[2]])$p.value,
                             test12_vr  = car::leveneTest(y~g,data=Ayg)$`Pr(>F)`[1],
                             test12_eq  = stats::t.test(datanv_[datacl_==levell_[1]],datanv_[datacl_==levell_[2]],
                                                 alternative = "two.sided", var.equal = FALSE)$p.value,
                             test12_gt  = stats::t.test(datanv_[datacl_==levell_[1]],datanv_[datacl_==levell_[2]],
                                                 alternative = "greater",var.equal = FALSE)$p.value,
                             test12_ls  = stats::t.test(datanv_[datacl_==levell_[1]],datanv_[datacl_==levell_[2]],
                                                 alternative = "less",var.equal = FALSE)$p.value,
                             test12_wx_eq  = stats::wilcox.test(datanv_[datacl_==levell_[1]],datanv_[datacl_==levell_[2]],
                                                         alternative = "two.sided",var.equal = FALSE)$p.value,
                             test12_wx_gq  = stats::wilcox.test(datanv_[datacl_==levell_[1]],datanv_[datacl_==levell_[2]],
                                                         alternative = "greater",var.equal = FALSE)$p.value,
                             test12_wx_ls  = stats::wilcox.test(datanv_[datacl_==levell_[1]],datanv_[datacl_==levell_[2]],
                                                         alternative = "less",var.equal = FALSE)$p.value
          )
        } else if (nlevel_>2) {
          cc           <- as.vector(c(table(datacl_)))
          resu_        <- as.data.frame(rbind((cc)))
          names(resu_) <- paste("n",1:nlevel_,sep='')
          
          norms        <- numeric(nlevel_)
          for (l in 1:nlevel_) {
            if (length(unique(datanv_[datacl_==levell_[l]]))>1) {
              norms[l] = stats::shapiro.test(datanv_[datacl_==levell_[l]])$p.value;
            } else {
              norms[l] = -1;
            }
          }
          norms        <- as.data.frame(rbind(norms))
          names(norms) <- paste("norm",1:nlevel_,sep='')
          
          resu_ = data.frame(var_cont   = nv_,
                             var_disc   = nu_,
                             resu_,
                             norms)
          
          res.av= stats::aov(y~g,data=Ayg)
          resu_ = data.frame(resu_,
                             test_vr        = car::leveneTest(y~g,data=Ayg)$`Pr(>F)`[1],        #
                             test_aov       = summary(res.av)[[1]][["Pr(>F)"]][1],   #var equal, normality
                             #test_aov       = summary(aov(y~g,data=Ayg))[[1]][["Pr(>F)"]][1],   #var equal, normality
                             test_aov_check = stats::shapiro.test(x = stats::residuals(object = res.av))$p.value,
                             test_welch     = stats::oneway.test(y~g,data=Ayg)$p.value,                #var not equal, normality
                             test_krusk     = stats::kruskal.test(y~g,data=Ayg)$p.value
          )
        }
        rownames(resu_)<-NULL
        resu_[,"var_cont"]=as.character(resu_[,"var_cont"])
        resu_[,"var_disc"]=as.character(resu_[,"var_disc"])
        resu_1 = resu_
        resu_2 = resu_
        for (l in 3:length(resu_)) resu_2[l] = as.character(resu_2[l])
        for (l in (2+nlevel_+1):length(resu_)) resu_2[l] = base::gettextf (paste("%.",4,"f",sep=""), round(resu_1[l],4));
        resus_=plyr::join(resus_,resu_2,by = names(resu_2), type = "full")
      } else {
        ccpb = ccpb+1
        pbttanova[[base::as.character(ccpb)]] = c(nv_,nu_)
      }
      
    }
    
  }
  #
  for (l in 1:ncol(resus_)) resus_[,l]=as.character(resus_[,l])
  resus_[is.na(resus_)] = " "; ## In `[<-.factor`(`*tmp*`, thisvar, value = " ") : invalid factor level, NA generated
  return(list(tabstat=resus_,pairs_no_panova=pbttanova))
}
