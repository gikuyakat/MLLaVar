#'make the descritpions of the model (bidirection) in interactive way
#'
#'@param rowmat data matrix for MLSEM
#'
#'@return the descriptions of the model (bidirection) in interactive way
#'
#'@export
mlbi <- function(rowmat){

  m <- 0
  mat1 <- data.frame(matrix(NA, nrow=1000, ncol=3))
  colnames(mat1) <- c("v1", "v2", "level")
  neq <- 1

  cat("\n***enter [models of bidirections]***\n")
  while (m == 0){
    cat("\n***list of entered variables in loaded data***\n", colnames(rowmat), "\n\n")
    ent <- 0
    mat1[neq, "v1"] <- readline("*variable 1? >>> ")
    mat1[neq, "v2"] <- readline("*variable 2? >>> ")
    mat1[neq, "level"] <- switch(menu(c("Between + Within","Between", "Within"), title="\n***which level(s)?(0--cancel)***")+1, "c", "BeWith","Between", "Within")

    if (mat1[neq, "level"] == "c"){
      mat1[neq, ] <- NA
      cat("\n***cancelled [models of bidirections]***\n\n")
    }else{
      neq <- neq+1
    }
    more <- switch(menu(c("YES","NO"), title="\n***continue to enter? [models of bidirections]***")+1, 2, 1, 2)


    if (more == "1"){
      cat("\n***entered model [models of bidirections]***\n")
      print(mat1[1:neq-1, ])
    }else if (more == "2"){
      cat("\n***start to make descriptions [models of bidirections]***\n")
      print(mat1[1:neq-1, ])
      ent <- switch(menu(c("YES","NO (reset and again)", "NO (cancel)"), title="\n***OK?***")+1, 0, 1, 2, 0)

      if (ent == 1){
        cat("\n***start [models of bidirections]***\n")
        m <- m+1
      }else if(ent == 2){
        more <- 0
        ent <- 0
        mat1[1:neq] <- NA
        neq <- 1
        cat("\n***reset and again [models of bidirections]***\n")
      }else if(ent == 0){
        stop("\n***cancelled [models of bidirections]***\n")
      }
    }
  }


  mat1 <- na.omit(mat1)
  vec1 <- c(unique(mat1$v1))
  nval <- 1
  vec2 <- c()
  k <- 1

  for (i in 1:length(vec1)){
    mat2 <- subset(mat1, mat1$v1==vec1[i])

    for (j in 1:nrow(mat2)){

      if (is.null(vec2[k])||is.na(vec2[k])){
        if (mat2[j, "level"] != "Between"){
          vec2[k] <- sprintf("%s_w~~c(cw%02d, cw%02d)*%s_w", mat2[j, "v1"], nval, nval, mat2[j, "v2"])
        }
      }else{
        if (mat2[j, "level"] != "Between"){
          vec2[k] <- sprintf("%s+c(cw%02d, cw%02d)*%s_w", vec2[k], nval, nval, mat2[j, "v2"])
        }
      }
      if (is.null(vec2[k+1])||is.na(vec2[k+1])){
        if (mat2[j, "level"] != "Within"){
          vec2[k+1] <- sprintf("%s_b~~c(NA, 0)*%s_b+c(cb%02db, cb%02dw)*%s_b", mat2[j, "v1"], mat2[j, "v2"], nval, nval, mat2[j, "v2"])
        }
      }else{
        if (mat2[j, "level"] != "Within"){
          vec2[k+1] <- sprintf("%s+c(NA, 0)*%s_b+c(cb%02d, cb%02dw)*%s_b", vec2[k+1], mat2[j, "v2"], nval, nval, mat2[j, "v2"])
        }
      }

      nval <- nval+1
    }
    k <- k+2
  }

  vec2 <- na.omit(vec2)
  modeltext <- "\n"
  for (l in 1:length(vec2)){
    modeltext <- sprintf("%s %s\n", modeltext, vec2[l])
  }
  cat("\n***completed [models of bidirections]***\n", modeltext, "\n\n")

  return(invisible(modeltext))

}
