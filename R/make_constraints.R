if(FALSE){
  tmp <- readClipboard()
  tmp <- tmp[grep("\\(", tmp)]

  tmp <- gsub("^.+\\((.+)\\);", "\\1", tmp)

  tmp2 <- gsub("(Moth|Fath)", "Prts", tmp)

  constraints <- mapply(function(x,y){
    paste0(gsub("(Moth|Fath)", "Prts", tmp[x]), " = ", tmp[x], " - ", tmp[y], ";")
  }, x = which(duplicated(tmp2)), y = which(duplicated(tmp2, fromLast = TRUE)), SIMPLIFY = TRUE)

  names(constraints) <- paste0("p", 1:length(constraints))

  model_constraints <- c("MODEL CONSTRAINT:", "NEW (", names(constraints), ");", paste0(names(constraints), " = ", paste0(attr(constraints, "labs"), "H"), " - ", paste0(attr(constraints, "labs"), "L"), ";"))

}
