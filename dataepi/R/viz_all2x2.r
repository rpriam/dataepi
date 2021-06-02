#' A function for showing the modalities in a 2d view plus a related indicator
#'
#' This function take the set of 2x2 table for the odds ratio in order to 
#' construct a new matrix of the whole set of modalities and performs pca.
#' The obtained projection is visualized and an indicator is also obtained.
#' 
#' @param TABSTAT The table from the list of 2x2 tables from the data.
#' @param g_ The number of groups for the clustering.
#' @param graph_ The characters string with null value for no projection, with the 
#' value to show the "PCA", the value "COSTAT" to show the percentages for 
#' Sick=1, and the value "CASTAT" to show the percentages for the Sick=0.
#' 
#' @return A list with the following entries.
#' \describe{
#'   \item{tabstat}{A data.frame with in the first left columns the matrix from the tables 
#'                 2x2 aggregated from the function tab_all2x2() after normalization of its pairs 
#'                 of columns, followed by two columns, one for the variable name and one for the 
#'                 modality name.}
#'   \item{pca}{The r object from a pca of the normalized matrix of counts.}
#'   \item{kmeans}{The r object from a kmeans of the normalized matrix of counts.}
#'   \item{ORpca}{An alternative indicator for odds ratios.}
#' }
#' @keywords 
#' @export
#' @examples
#' data(DebTrivedi)
#' A         <- DebTrivedi
#' A$hospbin <- as.integer(A$hosp>0)
#' vars_disc <- c("health","gender","region")
#' var_y     <- "hospbin"
#' resu_or   <- dataepi::tab_all2x2(A,vars_disc,var_y,dataepi::stat_oddsratio)
#' resu_fv   <- viz_all2x2(resu_or$tabstat,g_=7,graph_="PCA")
#' print(resu_fv$tabstat)
#' 
viz_all2x2 <- function(TABSTAT,g_=3,graph_="PCA") {
  for (j in 3:6) { TABSTAT[,j] = as.numeric(as.character(TABSTAT[,j])); }
  rownames_ <- paste(TABSTAT$variable,TABSTAT$modality,sep="")
  MTB=as.matrix(TABSTAT[3:6])
  MTB[,1:2]=diag(x=1/rowSums(MTB[,1:2]))%*%MTB[,1:2]
  MTB[,3:4]=diag(x=1/rowSums(MTB[,3:4]))%*%MTB[,3:4]
  MTB=data.frame(MTB)
  rownames(MTB)<-rownames_
  pr=NULL
  kk=NULL
  if (nrow(MTB>0)) {
    pr=stats::princomp(MTB,cor = TRUE)
    if (g_>0) {kk=stats::kmeans(MTB,centers = g_,nstart = 100);} else 
              {kk=list(); kk$cluster=rep(0,nrow(MTB))}
    if (graph_=="PCA") {
      graphics::plot(pr$scores[,1],pr$scores[,2],col=kk$cluster+1,xlab="PCA.Dim.1",ylab="PCA.Dim.2",
                     xlim=c(min(pr$scores[,1])-0.1*abs(max(pr$scores[,1])-min(pr$scores[,1])),
                            max(pr$scores[,1])+0.1*abs(max(pr$scores[,1])-min(pr$scores[,1]))),
                     ylim=c(min(pr$scores[,2])-0.1*abs(max(pr$scores[,2])-min(pr$scores[,2])),
                            max(pr$scores[,2])+0.1*abs(max(pr$scores[,2])-min(pr$scores[,2]))),
                     cex=0.2,pch=15)
      graphics::text(pr$scores[,1],pr$scores[,2],labels = row.names(MTB),col=kk$cluster+1)
      graphics::abline(h=0)
      graphics::abline(v=0)
    } else if (graph_=="CASTAT") {
      graphics::plot(MTB[,1],MTB[,3],col=kk$cluster+1,xlim=c(-0.1,1.1),ylim=c(-0.1,1.1),
                     xlab=paste(names(TABSTAT)[7],"%SickExposed",sep="_"),
                     ylab=paste(names(TABSTAT)[7],"%SickNotExposed",sep="_"),pch=15,cex=1.0)
      graphics::text(MTB[,1],MTB[,3],labels = row.names(MTB),col=kk$cluster+1)
      graphics::abline(coef = c(0,1))
      graphics::abline(h=0)
      graphics::abline(h=1)
      graphics::abline(v=0)
      graphics::abline(v=1)
    } else if (graph_=="COSTAT") {
      graphics::plot(MTB[,2],MTB[,4],col=kk$cluster+1,xlim=c(-0.1,1.1),ylim=c(-0.1,1.1),
                     xlab=paste(names(TABSTAT)[7],"%UnsickExposed",sep="_"),
                     ylab=paste(names(TABSTAT)[7],"%UnsickNotExposed",sep="_"),pch=15,cex=1.0)
      graphics::text(MTB[,2],MTB[,4],labels = row.names(MTB),col=kk$cluster+1)
      graphics::abline(coef = c(0,1))
      graphics::abline(h=0)
      graphics::abline(h=1)
      graphics::abline(v=0)
      graphics::abline(v=1)
    }
  }
  ORpca = as.data.frame(exp(pr$scores[,2])); names(ORpca)[1]<-"exp_dim1_pca"
  ORpca[,1] = ORpca[,1] * median(as.numeric(as.character(TABSTAT$OR))/ORpca[,1])
  return(list(tabstat=data.frame(MTB,variable=TABSTAT$variable,modality=TABSTAT$modality),
              pca=pr,kmeans=kk,ORpca=ORpca))
}
