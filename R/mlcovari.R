#'compute between/within level variance-covariance matrix in interactive way.
#'
#'@param rowmat data matrix for computing variance-covariance matrix. the first column must be grouping variable.
#'
#'@return compute between/within level variance-covariance matrix in interactive way.
#'
#'@export
mlcovari <- function(rowmat){
  submat <- mlsubset(rowmat)
  cat("\n***start to make ML covariance matrix***\n")
  return(invisible(mlcovar(submat)))
}
