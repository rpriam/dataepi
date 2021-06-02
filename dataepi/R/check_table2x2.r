
check_table2x2 <- function(X) {
  stopifnot(class(X) == "table")
  stopifnot(dim(X)[1] == 2)
  stopifnot(dim(X)[2] == 2)
  stopifnot(rownames(X)[1] == "0")
  stopifnot(rownames(X)[2] == "1")
  stopifnot(colnames(X)[1] == "0")
  stopifnot(colnames(X)[2] == "1")
}
