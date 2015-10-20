#'compute between/within level correlation in interactive way
#'
#'@param rowmat data matrix for correlation analysis
#'
#'@return compute between/within level correlation in interactive way
#'
#'@export
mlcori <- function(rowmat){
  namevec <- colnames(rowmat)

  sh <- switch(menu(c("YES","NO"), title="***show variable names ?***")+1, 2, 1, 2)

  if (sh == 1){
    cat(namevec)
  }

  m <- 0
  vallist <- c()

  while (m == 0){
    cat("\n***enter variable names [group variable]***\n")
    valzantei <- readline("*variable name? >>> ")

    if (length(namevec[namevec==valzantei])==0){
      ag <- switch(menu(c("YES","NO"), title="***not exist! try again? [group variable]***")+1, 2, 1, 2)
      if(ag == 2){
        stop("\n***cancelled [group variable]***\n")
      }else{
        ag <- 0
      }
    }else{
      vallist[1] <- valzantei
      m <- m+1
    }
  }

  for (i in 2:3){

  }

  m <- 0
  ag <- 0
  cat("\n***enter variable names [variables for analysis]***\n")

  while (m == 0){
    ent <- 0
    valzantei1 <- readline("*variable 1 name? >>> ")
    valzantei2 <- readline("*variable 2 name? >>> ")

    if (length(namevec[namevec==valzantei1])==0|length(namevec[namevec==valzantei2])==0){
      ag <- switch(menu(c("YES","NO"), title="***not exist! try again? [variables for analysis]***")+1, 2, 1, 2)
      if(ag == 2){
        stop("\n***cancel [variables for analysis]***\n")
      }else{
        ag <- 0
      }
    }else{
      vallist[2] <- valzantei1
      vallist[3] <- valzantei2

      cat("\n***start ML correlation analysis***\n")
      ent <- switch(menu(c("YES","NO (reset and again)", "NO (cancel)"), title="\n***OK?***")+1, 0, 1, 2, 0)

      if (ent == 1){
        cat("\n***start***\n")
        m <- m+1
      }else if(ent == 2){
        ent <- 0
        vallist[2:3] <- NA
        cat("\n***reset and again***\n")
      }else if(ent == 0){
        stop("\n***cancel***\n")
      }
    }
  }

  redata <- rowmat[, na.omit(vallist)]
  sh2 <- switch(menu(c("YES","NO"), title="***draw plot ?***")+1, F, T, F)

  return(mlcor(redata[,2],redata[,3],redata[,1],sh2))
}
