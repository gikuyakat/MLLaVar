#'make data matrix for mlcovar function in interactive way
#'
#'@param rowmat data matrix for reshaping
#'
#'@return data matrix for mlcovar function
#'
#'@export
mlsubset <- function(rowmat){
  namevec <- colnames(rowmat)

  sh <- switch(menu(c("YES","NO"), title="\n***show variable names?***")+1, 2, 1, 2)

  if (sh == 1){
    cat("\n***list of variables in your data***\n", namevec)
  }

  m <- 0
  vallist <- c()

  while (m == 0){
    cat("\n\n***enter variable names [group variable]***\n")
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

  m <- 0
  neq <- 2
  valzantei <- NA
  ag <- 0
  cat("\n***enter variable names [variables for analysis]***\n")

  while (m == 0){
    ent <- 0
    valzantei <- readline("*variable name? >>> ")

    if (length(namevec[namevec==valzantei])==0){
      ag <- switch(menu(c("YES","NO"), title="***not exist! try again? [variables for analysis]***")+1, 2, 1, 2)
      if(ag == 2){
        stop("\n***cancel [variables for analysis]***\n")
      }else{
        ag <- 0
      }
    }else{
      vallist[neq] <- valzantei
      neq <- neq+1

      more <- switch(menu(c("YES","NO"), title="***continue to enter?  [variables for analysis]***")+1, 2, 1, 2)

      if (more == "1"){
        cat("\n***list of entered variables***\n")
        print(vallist[1:neq-1])
      }else if (more == "2"){
        cat("\n***start to make data frame***\n")
        print(vallist[1:neq-1])
        ent <- switch(menu(c("YES","NO (reset and again)", "NO (cancel)"), title="\n***OK?***")+1, 0, 1, 2, 0)

        if (ent == 1){
          cat("\n***start***\n")
          m <- m+1
        }else if(ent == 2){
          more <- 0
          ent <- 0
          vallist[2:neq] <- NA
          neq <- 2
          cat("\n***reset and again***\n\n***enter variable names [variables for analysis]***\n")
        }else if(ent == 0){
          stop("\n***cancel***\n")
        }
      }
    }
  }

  redata <- rowmat[, na.omit(vallist)]
  resum <- matrix(nrow = length(na.omit(vallist)), ncol = 3)

  for (i in 1:nrow(resum)){
    resum[i, 1] <- vallist[i]
    resum[i, 2] <- as.numeric(length(na.omit(redata[, i])))
  }
  resum[1, 3] <- as.numeric(length(unique(redata[, 1])))
  colnames(resum) <- c("var.name", "N", "C")

  cat("\n")
  print(resum)
  cat("\n")
  return(redata)

}
