
check_labx <- function(labx_,kdim) {
  if (!is.null(labx_)) if (length(labx_)!=kdim) {
    cat("length of label vector should be as given (=", kdim, ")\n")
    cat("notice:call=",match.call(),"\n")
    labx_=NULL
  }
  if (is.null(labx_)) labx_  <- paste("Dim", 1:kdim, sep = ".")
}
