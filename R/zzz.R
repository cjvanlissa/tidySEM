.onLoad <- function(libname, pkgname){
  sem_software <- getOption("sem_software")
  if(is.null(sem_software)){
    options(sem_software = "lavaan")
  } else {
    if(!is.character(sem_software)){
      warning("getOption('sem_software') did not return a character value.")
    }
  }
}
