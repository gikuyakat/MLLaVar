#'conduct MLSEM by using lavaan function (in interactive way)
#'
#'@param rowmat data matrix for MLSEM. the first column must be grouping variable.
#'@param sub if sub=TRUE, you can choose variables for MLSEM from data matrix in interactive way. If sub=FALSE, all the variables in rowmat were analyzed.
#'@param unimodel desctiptions of model (unidirection). if unimodel=NULL, you can make descriptions of model (Unidirection) in interactive way.
#'@param bimodel desctiptions of model (bidirection). if bimodel=NULL, you can make descriptions of model (bidirection) in interactive way.
#'@param fig if fig=TRUE, draw the figure of results by using semPaths function
#'@param verbose if verbose=TRUE, show the process of iterations.
#'
#'@return conduct MLSEM by using lavaan function (in interactive way)
#'
#'@export
mllavar <- function(rowmat, sub=NULL, unimodel=NULL, bimodel=NULL, fig=FALSE, verbose=FALSE){

  if(is.null(sub)){
    sub <- switch(menu(c("YES","NO"), title="***need to reshape your data?***")+1, FALSE, TRUE, FALSE)
  }

  if(sub == TRUE){
    cat("\n***start to reshape***\n")
    rowmat <- mlsubset(rowmat)
  }

  if (is.null(unimodel)){
    uniq <- switch(menu(c("YES","NO"), title="***enter [models of unidirections]?***")+1, 0, 1, 2)
    if(uniq == 0){
      stop("***cancelled***")
    }else if(uniq == 1){
      unitext <- mluni(rowmat)
    }else if(uniq == 2){
      unitext <- "\n"
      cat("***skip [models of unidirections]***")
    }
  }else{
    unitext <- unimodel
  }

  if (is.null(bimodel)){
    biq <- switch(menu(c("YES","NO"), title="***enter [models of bidirections]?***")+1, 0, 1, 2)
    if(biq == 0){
      stop("***cancelled***")
    }else if(biq == 1){
      bitext <- mlbi(rowmat)
    }else if(biq == 2){
      bitext <- "\n"
      cat("***skip [models of bidirections]***")
    }
  }else{
    bitext <- bimodel
  }

  matx <- na.omit(rowmat)

  matmlcor <- mlcovar(matx)

  Sb <- matmlcor$VarCovar_Between
  Sw <- matmlcor$VarCovar_Within
  omega3 <- matmlcor$omega
  nob <- list(matmlcor$nBetween, matmlcor$nWithin)
  #meanvec <- list(matmlcor$mean_Between, matmlcor$mean_Within)
  icc <- matmlcor$icc

  bwtext <- mlbewith(rowmat, omega3)


  mats <- list(Sb, Sw)
  names(mats) <- c("between", "within")

  modeltext <- sprintf("%s\n %s\n %s", bwtext, bitext, unitext)

  mlsemres <- lavaan::lavaan(modeltext, sample.cov=mats, int.lv.free=F,
                     sample.nobs=nob, mimic="EQS", se="standard", verbose=verbose, likelihood = "wishart")

  if(fig==TRUE){
    par(mfrow=c(1,2))
    #semPlot::semPaths(mlsemres, "par", fade=F)
  }
  cat("\n***CAUTION: PLEASE CHECK AND COMPARE RESULTS BY OTHER SOFTWARES***\n")


  estw <- fitted(mlsemres)$within$cov
  minusw <- c()
  for (i in 1:nrow(estw)){
    if(icc[rownames(estw)[i]]==1){
      minusw <- c(minusw, i)
    }
  }
  covw2 <- Sw
  estw2 <- estw
  if (length(minusw)!=0){
    estw2 <- estw[-minusw, -minusw]
    covw2 <- Sw[-minusw, -minusw]
  }

  estb <- fitted(mlsemres)$between$cov
  minusb <- c()
  for (i in 1:nrow(estb)){
    if(icc[rownames(estb)[i]]<0){
      minusb <- c(minusb, i)
    }
  }
  estb2 <- estb
  covb2 <- Sb
  if(length(minusb)!=0){
    estb2 <- estb[-minusb, -minusb]
    covb2 <- Sb[-minusb, -minusb]
  }

  nvarw <- ncol(estw2)*(ncol(estw2)+1)/2
  nvarb <- ncol(estb2)*(ncol(estb2)+1)/2
  npar2 <- lavaan::fitMeasures(mlsemres, "npar")-(length(minusw)+length(minusb))
  df2 <- (nvarw + nvarb)-npar2

  FMLb <- (log(det(estw + omega3*estb)) + sum(diag(solve(estw + omega3*estb) %*% covb2))-
             log(det(covb2))-ncol(estb2))
  FMLw <- (log(det(estw2)) + sum(diag(solve(estw2) %*% covw2))-log(det(covw2))-ncol(estw2))

  FML <- nob[[1]]*FMLb+nob[[2]]*FMLw

  FMLbaseb <- (log(det(Sw + omega3*covb2)) + sum(diag(solve(Sw + omega3*covb2) %*% covb2))-
                 log(det(covb2))-ncol(covb2))
  FMLbasew <- (log(det(covw2)) + sum(diag(solve(covw2) %*% covw2))-log(det(covw2))-ncol(covw2))

  FMLbase <- nob[[1]]*FMLbaseb+nob[[2]]*FMLbasew

  chisq2 <- FML - FMLbase

  pval2 <- pchisq(chisq2, df2, lower.tail=F)

  AIC2 <- 2*log(FML) + 2*npar2

  Tau <- chisq2 - df2
  Taui<- FMLbase - (ncol(estw2)*(ncol(estw2)-1) + ncol(estb2)*(ncol(estb2)-1))/2

  ifelse (Taui > Tau && Tau >= 0,
          CFI2 <- 1 - Tau/Taui,
          CFI2 <- 0)

  ifelse (df2 != 0,
          RMSEA2 <- sqrt(c(max(Tau/(df2*(nob[[1]]+nob[[2]])),0))),
          RMSEA2 <- 0)

  fitM <- c(nvarw+nvarb, npar2, df2, log(FML), log(FMLbase), AIC2, chisq2, pval2, CFI2, RMSEA2)

  names(fitM) <- c("nvar.mllavar", "npar.mllavar","df.mllavar","loglike.mllavar",
                   "loglike.base.mllavar", "AIC.mllavar", "chisq.mllavar", "p-value.mllavar",
                   "CFI.mllavar", "RMSEA.mllavar")

  reslist <- list(mlsemres, fitM, matmlcor)
  names(reslist) <- c("res.lavaan","fitM.mllavar","mlcovar")

  return(invisible(reslist))
}
