
write_docx <- function(fullpathfile,au,list_supp=NULL,add_ORpca_=FALSE) {  
  doc_ <- officer::read_docx()
  
  doc_ <- officer::body_add_par(doc_, "Report for the autogenerated analysis from the r package dataepi", style = "heading 1")
  doc_ <- officer::body_add_par(doc_, "-------------------------------------------------------------\n")
  doc_ <- officer::body_add_par(doc_, paste("nrows=",nrow(au$args$A),"\n",sep=""))
  doc_ <- officer::body_add_par(doc_, paste("ncols=",ncol(au$args$A),"\n",sep=""))
  doc_ <- officer::body_add_par(doc_, paste("var_y=",au$args$var_y,"\n",sep=""))
  doc_ <- officer::body_add_par(doc_, paste("vars_x","(",length(au$args$vars_x), ") =",paste(au$args$vars_x,collapse = " "),"\n",sep=""))
  doc_ <- officer::body_add_par(doc_, paste("vars_cont","(",length(au$args$vars_cont), ") =",paste(au$args$vars_cont,collapse = " "),"\n",sep=""))
  doc_ <- officer::body_add_par(doc_, paste("vars_disc","(",length(au$args$vars_disc), ") =",paste(au$args$vars_disc,collapse = " "),"\n",sep=""))
  doc_ <- officer::body_add_par(doc_, paste("vars_int","(",length(au$args$vars_int), ") =",paste(au$args$vars_int,collapse = " "),"\n",sep=""))
  doc_ <- officer::body_add_par(doc_, paste("var_id=",au$args$var_id,"\n",sep=""))
  doc_ <- officer::body_add_par(doc_, "-------------------------------------------------------------\n")
  
  doc_ <- officer::body_add_break(doc_)
  
  doc_ <- officer::body_add_par(doc_, "This is the table for the relative risks.\n")
  
  TT = au$rr$tabstat
  for (j in 7:8) TT[,j]=as.character(TT[,j])
  for (j in 7:8) TT[,j]=as.numeric(TT[,j])
  TT[,7:8] = round(TT[,7:8],2)
  for (j in 7:8) TT[,j]=as.character(TT[,j])
  TT[is.na(TT)]=" "
  doc_ <- officer::body_add_table(doc_,TT)
  
  doc_ <- officer::body_add_break(doc_)
  
  doc_ <- officer::body_add_par(doc_, "This is the table for the odd ratios from full model and selected.\n")
  
  TT = au$gg$allors
  for (j in c(2:4,6:8)) TT[,j]=as.character(TT[,j])
  for (j in c(2:4,6:8)) TT[,j]=as.numeric(TT[,j])
  TT[,c(2:4,6:8)]=round(TT[,c(2:4,6:8)],2)
  for (j in c(2:4,6:8)) TT[,j]=as.character(TT[,j])
  TT[is.na(TT)]=" "
  doc_ <- officer::body_add_table(doc_,TT)
  
  docx_file_ <- print(doc_, target = fullpathfile)
}
  