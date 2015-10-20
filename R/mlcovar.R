#'compute between/within level variance-covariance matrix
#'
#'@param rowmat data matrix for computing variance-covariance matrix. the first column must be grouping variable.
#'
#'@return compute between/within level variance-covariance matrix
#'
#'@export
mlcovar <- function(rowmat){
  # listwise
  matx <- na.omit(rowmat)
  gmmat <- matrix(nrow=nrow(matx), ncol=ncol(matx)-1)
  cmat <- matrix(nrow=nrow(matx), ncol=ncol(matx)-1)
  #
  glist <- unique(matx[,1])

  ngroup <- c()
  for (i in 1:length(glist)){
    ngroup[i] <- sum(matx[,1]==glist[i])
  }

  meanx <- c()

  for (i in 2:ncol(matx)){
    # group mean
    gmmat[,(i-1)] <- tapply(matx[, i], matx[,1], mean, na.rm=T)[as.character(matx[,1])]
    # centering
    cmat[,(i-1)] <- matx[, i]-gmmat[,(i-1)]
    # grand mean
    meanx[i-1] <- mean(matx[,i])
  }

  Sb <- matrix(nrow=ncol(matx)-1,ncol=ncol(matx)-1)
  Sw <- matrix(nrow=ncol(matx)-1,ncol=ncol(matx)-1)
  icc <- c()
  meanvecb <- c()
  meanvecw <- c()


  for (i in 1:ncol(Sb)){
    for (j in 1:nrow(Sb)){
      # S (between)
      Sb[i, j] <- sum((gmmat[,i]-meanx[i])*(gmmat[,j]-meanx[j]))/
        (length(unique(matx[,1]))-1)
      # S (within)
      Sw[i, j] <- sum(cmat[,i]*cmat[,j])/
        (nrow(matx)-length(unique(matx[,1])))
    }
  }

  omega <- (nrow(matx)^2 - sum(ngroup^2))/(nrow(matx)*((length(unique(matx[,1])))-1))

  # Variance/Covariance (between)
  Sigmab <- (Sb - Sw)/omega
  # Variance/Covariance (within)
  Sigmaw <- Sw

  Corrb <- matrix(nrow=nrow(Sb),ncol=ncol(Sb))
  Corrw <- matrix(nrow=nrow(Sw),ncol=ncol(Sw))

  for (i in 1:nrow(Corrb)){
    for (j in 1:ncol(Corrb)){
      Corrb[i, j] <- Sigmab[i, j]/(sqrt(Sigmab[i,i])*sqrt(Sigmab[j,j]))
      Corrw[i, j] <- Sigmaw[i, j]/(sqrt(Sigmaw[i,i])*sqrt(Sigmaw[j,j]))
    }
    icc[i] <- (Sigmab[i,i])/(Sigmab[i,i]+Sigmaw[i,i])
    meanvecb[i] <- mean(gmmat[,i])
    meanvecw[i] <- mean(cmat[,i])

  }

  namae <- colnames(matx)[2:ncol(matx)]
  rownames(Sb) <- namae
  rownames(Sw) <- namae
  rownames(Corrb) <- namae
  rownames(Corrw) <- namae
  colnames(Sb) <- namae
  colnames(Sw) <- namae
  colnames(Corrb) <- namae
  colnames(Corrw) <- namae
  names(icc) <- namae

  res <- list(Sb, Sw, Corrb, Corrw, meanvecb, meanvecw, icc, length(unique(glist)), (nrow(matx)-length(unique(glist))),  omega)
  names(res) <- c("VarCovar_Between","VarCovar_Within","Corr_Between","Corr_Within", "mean_Between", "mean_Within",
                  "icc", "nBetween", "nWithin", "omega")

  return(invisible(res))
}
