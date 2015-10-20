#'make the descriptions of the model (between/within level)
#'
#'@param rowmat data matrix for MLSEM
#'@param omega3 omega for adjusting between-level variance-covariance
#'
#'@return the descriptions of the model (bidirection) in interactive way
#'
#'@export
mlbewith <- function(rowmat, omega3){

  cat("\n***start [models of latent variables of Between / Within level]***\n")
  vec <- c()
  for (i in 1:(ncol(rowmat)-1)){
    vec[i] <- sprintf("%s_b=~c(%f, 0)*%s", colnames(rowmat)[i+1], sqrt(omega3), colnames(rowmat)[i+1])
    vec[(ncol(rowmat)-1)*2+i] <- sprintf("%s_b~~c(NA, 0)*%s_b", colnames(rowmat)[i+1], colnames(rowmat)[i+1])
    vec[(ncol(rowmat)-1)+i] <- sprintf("%s_w=~c(1, 1)*%s", colnames(rowmat)[i+1], colnames(rowmat)[i+1])
    vec[(ncol(rowmat)-1)*3+i] <- sprintf("%s_w~~c(ccw%02d, ccw%02d)*%s_w", colnames(rowmat)[i+1], i, i, colnames(rowmat)[i+1])
    #vec[(ncol(rowmat)-1)*6+i] <- "\n" #sprintf("%s_w~0", colnames(rowmat)[i+1])
    vec[(ncol(rowmat)-1)*4+i] <- sprintf("%s_b~~c(0, 0)*%s_w", colnames(rowmat)[i+1], colnames(rowmat)[i+1])
    #vec[(ncol(rowmat)-1)*7+i] <- "\n" # sprintf("%s_b~0", colnames(rowmat)[i+1])
    #vec[(ncol(rowmat)-1)*5+i] <- sprintf("%s~%f", colnames(rowmat)[i+1], mvec[i])
  }

  ftext <- "\n"
  for (i in 1:length(vec)){
    ftext <- sprintf("%s %s\n", ftext, vec[i])
  }

  cat("\n***completed [models of latent variables of Between / Within level]***\n", ftext, "\n\n")
  return(invisible(ftext))

}
