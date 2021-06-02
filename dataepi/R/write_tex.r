
write_tex <- function(fullpathfile,au,list_supp=NULL,add_ORpca_=FALSE) { 
  
  str_fix_underscore <- function(s) gsub("_","\\_",s,fixed=TRUE)
  
  write_tab_hunk <- function(tab_stats_,nbrows_,caption_,label_) {
    tab_stats_hunk <- split(tab_stats_, factor(sort(rank(1:nrow(tab_stats_))%/%nbrows_)))
    for (h in 1:length(names(tab_stats_hunk))) {
      tabtex_stats = utils::capture.output ( print(xtable::xtable(tab_stats_hunk[[as.character(h-1)]],
          include.rownames=FALSE,
          caption=paste(caption_, " (part ",h,"/",length(names(tab_stats_hunk)),")",sep=""),
          label=paste(label_,h,sep=".") ) ) )
      write(tabtex_stats,file=fileConn,append = TRUE)
      write(" ", fileConn, append=TRUE)
      write("\\newpage", fileConn, append=TRUE)
    }
  }
  
  fileConn<-file(fullpathfile, "w")
  
  write("\\documentclass[12pt]{article}", fileConn, append=TRUE)
  write("\\usepackage[margin=0.7in]{geometry}", fileConn, append=TRUE)
  write("\\usepackage[utf8]{inputenc}", fileConn, append=TRUE)
  write("\\usepackage{graphics}", fileConn, append=TRUE)
  write("\\usepackage{datetime}", fileConn, append=TRUE)
  write("\\usepackage{pdflscape}", fileConn, append=TRUE)
  
  write(" ", fileConn, append=TRUE)
  write("\\author{ }", fileConn, append=TRUE)
  write("\\date{\\today (\\currenttime)}", fileConn, append=TRUE)
  write("\\title{Report\\footnote{This document is auto-generated from the r package daatepi.} for output from health data}", fileConn, append=TRUE)
  write(" "  , fileConn, append=TRUE)
  write("\\begin{document}", fileConn, append=TRUE)
  write("\\maketitle", fileConn, append=TRUE)
  write("\\begin{abstract}", fileConn, append=TRUE)
  write(paste("\\noindent\\textbf{Main purpose:}  This report has for objective the analysis of the variable ", 
              str_fix_underscore(au$args$var_y), " for the disease ",
              list_supp$disease, " ",
              " with the variables ", str_fix_underscore(paste(au$args$vars_x,collapse = ", ")), " ."), fileConn, append=TRUE)
  write(" ", fileConn, append=TRUE)
  write(paste("\\noindent\\textbf{Main results:} The final main variables are the following, ", 
              str_fix_underscore( paste(rownames(au$fv$tabstat)[!is.na(as.numeric(au$gg$allors$or_r))],collapse = ", ") ), 
              sep=""),fileConn, append=TRUE)
  write("\\end{abstract}", fileConn, append=TRUE)
  
  write(" ", fileConn, append=TRUE)
  write("\\noindent\\textbf{Introduction}", fileConn, append=TRUE)
  write("~\\\\", fileConn, append=TRUE)
  write(" ", fileConn, append=TRUE)
  
  if (!is.null(list_supp)) {
    write("\\noindent\\textit{Place of the study}\\\\", fileConn, append=TRUE)
    write(paste("\\noindent This analysis took place at ", list_supp$where, ".",sep=""), fileConn, append=TRUE)
    write("~\\\\", fileConn, append=TRUE)
    write(" ", fileConn, append=TRUE)
    
    write("\\noindent\\textit{Type of the study}\\\\", fileConn, append=TRUE)
    write(paste("\\noindent This analysis was ", list_supp$project, ".",sep=""), fileConn, append=TRUE)
    write("~\\\\", fileConn, append=TRUE)
    write(" ", fileConn, append=TRUE)
    
    write("\\noindent\\textit{Criteria for inclusion and exclusion}\\\\", fileConn, append=TRUE)
    write(list_supp$inex, fileConn, append=TRUE)
    write("~\\\\", fileConn, append=TRUE)
    write(" ", fileConn, append=TRUE)
  }
  
  write("\\noindent\\textit{Size of the sample}\\\\", fileConn, append=TRUE)
  write(paste("\\noindent The number of rows of the dataset is equal to ", nrow(au$A), " after filtering.",sep=""), fileConn, append=TRUE)
  write("~\\\\", fileConn, append=TRUE)
  write(" ", fileConn, append=TRUE)
  write(" ", fileConn, append=TRUE)
  
  # -----------------------------------------------------
  
  write(" ", fileConn, append=TRUE)
  write("\\newpage", fileConn, append=TRUE)
  write("\\noindent \\textbf{Descriptive results}", fileConn, append=TRUE)
  
  # -----------------------------------------------------
  
  write(" ", fileConn, append=TRUE)
  write("\\newpage", fileConn, append=TRUE)
  write("\\noindent \\textbf{Model results}", fileConn, append=TRUE)
  #write("\\section{Output with relative and odds ratios}", fileConn, append=TRUE)
  
  #tabtex_brut_rr = utils::capture.output ( print(xtable::xtable(au$rr$tabstat[,-c(3,4,5,6)]),include.rownames=FALSE) )
  
  Z2=au$fv$pca$scores[,2]
  CTR2=Z2^2/sum(Z2^2)
  if (add_ORpca_==TRUE) {
    tabstat_=data.frame(au$rr$tabstat[,-(3:6)],
                        au$or$tabstat[,7:9],
                        ORpca=as.character(round_all(unlist(au$fv$ORpca[1]))),
                        #Z2=as.character(round(Z2,2)),
                        CTR2=sprintf("%03d", round(100*CTR2)))
    tabstat_$ORpca = base::gettextf ("%.2f", round(as.numeric(as.character(tabstat_$ORpca)),2))
  } else {
    tabstat_=data.frame(au$rr$tabstat[,-(3:6)],
                        au$or$tabstat[,7:9])
  }
  
  write_tab_hunk(tabstat_,35,
                 "Results for odds ratios and relative risks with their confindence interval at 0.95\\%",
                 "tab:ro.and.rr")
  
  # tabtex_brut_rr = utils::capture.output ( print(xtable::xtable(tabstat_,
  #   include.rownames=FALSE,
  #   caption="Results for odds ratios and relative risks with their confindence interval at 0.95\\%",
  #   label="tab:ro.and.rr") ) )
  # write(tabtex_brut_rr,file=fileConn,append = TRUE)
  
  # -----------------------------------------------------
  
  # write(" ", fileConn, append=TRUE)
  # write("\\newpage", fileConn, append=TRUE)
  #write("\\section{Output with (corrected) odds ratio}", fileConn, append=TRUE)
  # 
  # tabtex_corrected_or = utils::capture.output ( print(xtable::xtable(au$gg$allors,
  #     include.rownames=FALSE,
  #     caption="Results with the multivariate regression and corresponding odds ratios with their confindence interval at 0.95\\%",
  #     label="tab:or.from.reglogistic") ) )
  # write(tabtex_corrected_or,file=fileConn,append = TRUE)
  write_tab_hunk(au$gg$allors,35,
                 "Results with the multivariate regression and corresponding odds ratios with their confindence interval at 0.95\\%",
                 "tab:or.from.reglogistic")
  
  #ggors = au$gg$allors[,c(1,6:9)]
  #au$rr$tabstat[,3:6]
  #write_tab_hunk
  
  # -----------------------------------------------------
  
  write(" ", fileConn, append=TRUE)
  write("\\noindent \\textbf{Discussion}", fileConn, append=TRUE)
  
  # -----------------------------------------------------
  
  write(" ", fileConn, append=TRUE)
  write("\\newpage", fileConn, append=TRUE)
  write("\\section*{Appendix 1 - Variables from data file}", fileConn, append=TRUE)
  
  #write("-------------------------------------------------------------\\\\", fileConn, append=TRUE)
  write(paste("\\noindent nrows=",nrow(au$args$A),"\\\\",sep=""), fileConn, append=TRUE)
  write(paste("\\noindent ncols=",ncol(au$args$A),"\\\\",sep=""), fileConn, append=TRUE)
  write(paste("\\noindent var y=",str_fix_underscore(au$args$var_y),"\\\\",sep=""), fileConn, append=TRUE)
  write(paste("\\noindent vars x","(",length(au$args$vars_x), ") =",str_fix_underscore(paste(au$args$vars_x,collapse = " ")),"\\\\",sep=""), fileConn, append=TRUE)
  write(paste("\\noindent vars cont","(",length(au$args$vars_cont), ") =",str_fix_underscore(paste(au$args$vars_cont,collapse = " ")),"\\\\",sep=""), fileConn, append=TRUE)
  write(paste("\\noindent vars disc","(",length(au$args$vars_disc), ") =",str_fix_underscore(paste(au$args$vars_disc,collapse = " ")),"\\\\",sep=""), fileConn, append=TRUE)
  write(paste("\\noindent vars int","(",length(au$args$vars_int), ") =",str_fix_underscore(paste(au$args$vars_int,collapse = " ")),"\\\\",sep=""), fileConn, append=TRUE)
  write(paste("\\noindent var id=",str_fix_underscore(au$args$var_id),"\\\\",sep=""), fileConn, append=TRUE)
  #write("-------------------------------------------------------------\\\\", fileConn, append=TRUE)
  
  write("\\newpage", fileConn, append=TRUE)
  write("\\pagestyle{empty}", fileConn, append=TRUE)
  write("\\begin{landscape}", fileConn, append=TRUE)
  
  write("\\section*{Appendix 2 - Descriptive statistics}", fileConn, append=TRUE)
  
  tabtex_disc = utils::capture.output ( print(xtable::xtable(au$desc_disc,
                                              include.rownames=FALSE,
                                              caption="Results from description of categorical variables",
                                              label="tab:descdisc") ) )
  write(tabtex_disc,file=fileConn,append = TRUE)
  
  write("\\newpage", fileConn, append=TRUE)
  tabtex_cont = utils::capture.output ( print(xtable::xtable(au$desc_cont,
                                              include.rownames=FALSE,
                                              caption="Results from description of continuous variables",
                                              label="tab:descont") ) )
  write(tabtex_cont,file=fileConn,append = TRUE)
  
  write("\\newpage", fileConn, append=TRUE)
  tabtex_biva = utils::capture.output ( print(xtable::xtable(au$desc_biv,
                                              include.rownames=TRUE,
                                              caption="Results from bivariate descriptions.",
                                              label="tab:descbiv") ) )
  write(tabtex_biva,file=fileConn,append = TRUE)
  
  write("\\newpage", fileConn, append=TRUE)
  write("\\section*{Appendix 3 - Chi-square tests}", fileConn, append=TRUE)
  
  write_tab_hunk(au$test_chi2$tabchi2,20,
                 "Results from chi2 tests for testing the link between two categorical variables. The subsample sizes are not checked.",
                 "tab:chi2")
  
  #write("\\newpage", fileConn, append=TRUE)
  write("\\section*{Appendix 4 - Tests for comparing two means (t.test)}", fileConn, append=TRUE)
  
  tabtex_t.test = utils::capture.output ( print(xtable::xtable(au$test_tt,
                                                               include.rownames=FALSE,
                                                               caption="Results from t-Student tests for comparing two means from continuous variables. The subsample sizes are not checked.",
                                                               label="tab:t.test") ) )
  write(tabtex_t.test,file=fileConn,append = TRUE)
  
  write("\\newpage", fileConn, append=TRUE)
  write("\\section*{Appendix 5 - Tests for comparing several means (t.test,anova)}", fileConn, append=TRUE)
  
  ncoltabanova=ncol(au$test_anova$tabstat)
  
  tabanova=data.frame(au$test_anova$tabstat[,1:2],
                      au$test_anova$tabstat[,(ncoltabanova-11):(ncoltabanova-5)])
  
  write_tab_hunk(tabanova,20,
                 "Results from anova tests for comparing means from continuous variables (part 1). The subsample sizes are not checked.",
                 "tab:anova.test1")
  
  tabanova=data.frame(au$test_anova$tabstat[,1:2],
                      au$test_anova$tabstat[,(ncoltabanova-4):(ncoltabanova)])
  
  write_tab_hunk(tabanova,20,
                 "Results from anova tests for comparing means from continuous variables (part 2). The subsample sizes are not checked.",
                 "tab:anova.test2")
  
  
  write("\\end{landscape}", fileConn, append=TRUE)
  write("\\pagestyle{plain}", fileConn, append=TRUE)
  
  # cc=0
  # while(TRUE) {
  #   
  #   
  # }
  
  write("\\end{document}", fileConn, append=TRUE)
  
  close(fileConn)

}
