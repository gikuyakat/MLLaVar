#'compute between.within level correlation
#'
#'@param x variable for correlation analysis (1)
#'@param y variable for correlation analysis (2)
#'@param g grouping variable
#'@param fig if fig=TRUE, mlcor draws the scatterplot
#'
#'@return compute between/within level correlation
#'
#'@export
mlcor <- function(x,y,g,fig=F){
  matA <- na.omit(data.frame(x,y,g))
  #
  glist <- unique(matA$g)

  ngroup <- c()
  for (i in 1:length(glist)){
    ngroup[i] <- sum(matA$g==glist[i])
  }

  # ?O???[?v????
  matA$x_m <- tapply(matA$x, matA$g, mean, na.rm=T)[as.character(matA$g)]
  matA$y_m <- tapply(matA$y, matA$g, mean, na.rm=T)[as.character(matA$g)]

  # ???S??
  matA$x_c <- matA$x - matA$x_m
  matA$y_c <- matA$y - matA$y_m

  # ?S?̕???
  mean_x <- mean(na.omit(matA$x))
  mean_y <- mean(na.omit(matA$y))

  Sb_x <- sum((matA$x_m - mean_x)^2)/(length(unique(matA$g))-1)
  Sw_x <- sum(matA$x_c^2)/((nrow(matA))-length(unique(matA$g)))

  Sb_y <- sum((matA$y_m - mean_y)^2)/(length(unique(matA$g))-1)
  Sw_y <- sum(matA$y_c^2)/((nrow(matA))-length(unique(matA$g)))

  SSb <- sum((matA$x_m - mean_x)*(matA$y_m - mean_y))/
    (length(unique(matA$g))-1)
  SSw <- sum(matA$x_c*matA$y_c)/
    ((nrow(matA))-length(unique(matA$g)))

  omega <- (nrow(matA)^2 - sum(ngroup^2))/(nrow(matA)*((length(unique(matA$g)))-1))

  Varb_x <- (Sb_x - Sw_x)/omega
  Varw_x <- Sw_x

  Varb_y <- (Sb_y - Sw_y)/omega
  Varw_y <- Sw_y

  Covb <- (SSb - SSw)/omega
  Covw <- SSw

  Corrb <- Covb/(sqrt(Varb_x)*sqrt(Varb_y))
  Corrw <- Covw/(sqrt(Varw_x)*sqrt(Varw_y))

  Corrb0 <- SSb/(sqrt(Sb_x)*sqrt(Sb_y))

  eCovb0 <- sqrt(1 + Corrb0^2)*sqrt(Sb_x*Sb_y)/sqrt(length(unique(matA$g))-1)
  eCovw <- sqrt(1 + Corrw^2)*sqrt(Varw_x*Varw_y)/sqrt((nrow(matA))-length(unique(matA$g)))
  eCovb <- ifelse(is.na(eCovw), eCovb0/omega, sqrt(eCovw^2 + eCovb0^2)/omega)

  zCorrb <- Covb/eCovb
  zCorrw <- Covw/eCovw

  pCorrb <- pnorm(abs(zCorrb), lower.tail=F)*2
  pCorrw <- pnorm(abs(zCorrw), lower.tail=F)*2

  res <- list(Corrb, zCorrb, pCorrb, (length(unique(matA$g))),
              Corrw, zCorrw, pCorrw, ((nrow(matA))-length(unique(matA$g))))
  names(res) <- c("Corr_Between", "zCorr_Between", "p_Between", "n_Between",
                  "Corr_Within", "zCorr_Within", "p_Within", "n_Within")

  ifelse(is.na(pCorrb)==F&&pCorrb < 0.05,
         sigb <- "*",
         sigb <- "")
  ifelse(is.na(pCorrw)==F&&pCorrw < 0.05,
         sigw <- "*",
         sigw <- "")

  if(fig==T){
    xm <- tapply(matA$x, matA$g, mean)
    ym <- tapply(matA$y, matA$g, mean)
    xmym <- na.omit(data.frame(xm,ym))

    par(mfrow=c(1,2))
    if(var(xmym[,1])!=0 & var(xmym[,2])!=0){
      plot(xmym[,1],xmym[,2],main="between")
      pointLabel(xmym[,1], xmym[,2], labels=rownames(xmym), col="#000080", cex=0.6)
    }else{
      plot(matA$x,matA$y,type="n",main="between")
    }
    if(var(matA$x_c)!=0 & var(matA$y_c)!=0){
      plot(matA$x_c,matA$y_c,main="within")
    }else{
      plot(matA$x_c,matA$x_c,type="n",main="within")
    }
  }

  cat(sprintf("correlation(between) = %.2f%s, correlation(within) = %.2f%s\n\n", Corrb, sigb, Corrw, sigw))
  return(invisible(res))
}
