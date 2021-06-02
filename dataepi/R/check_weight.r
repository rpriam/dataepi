
check_weight <- function(ws_,size_,normalize_=TRUE) {
  if (!is.null(ws_)) if (length(ws_)!=size_) {
    cat("length of weight vector should be as given (=", size_, ")\n")
    cat("notice:call=",match.call(),"\n")
    ws_=NULL
  }
  if (is.null(ws_)) ws_ <- rep(1, size_)
  if (normalize_) ws_ <- ws_/sum(ws_)
  ws_
}

