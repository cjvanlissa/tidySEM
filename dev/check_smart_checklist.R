check_smart_lca <- function(){
  checks <- worcs_checklist
    checks[sapply(checks, inherits, what = "factor")] <- lapply(checks[sapply(checks,
                                                                              inherits, what = "factor")], as.character)
    checks$pass <- FALSE
    f <- list.files(path, recursive = TRUE, full.names = TRUE)
    f_lc <- tolower(f)
    tracked <- tryCatch({
      git_ls(path)
    }, error = function(e) {
      NULL
    })
    checks$pass[checks$name == "git_repo"] <- length(tracked) >
      0
    checks$pass[checks$name == "has_remote"] <- tryCatch({
      dim(git_remote_list(path))[1] > 0
    }, error = function(e) {
      FALSE
    })
    checks$pass[checks$name == "readme"] <- any(endsWith(f_lc,
                                                         "readme.md"))
    checks$pass[checks$name == "license"] <- any(endsWith(f_lc,
                                                          "license") | endsWith(f_lc, "license.md"))
    checks$pass[checks$name == "citation"] <- {
      rmarkdown_files <- f[endsWith(f_lc, ".rmd")]
      any(sapply(rmarkdown_files, function(thisfile) {
        txt <- paste0(readLines(thisfile, encoding = "UTF-8"),
                      collapse = "")
        grepl("@", txt, fixed = TRUE) & grepl("\\.bib",
                                              txt)
      }))
    }
    checks$pass[checks$name == "data"] <- tryCatch({
      if (!is.null(worcsfile[["data"]]) & length(tracked) >
          0) {
        worcs_data <- names(worcsfile$data)
        worcs_data <- c(worcs_data, unlist(sapply(worcsfile$data[sapply(worcsfile$data,
                                                                        function(x) {
                                                                          !is.null(x[["synthetic"]])
                                                                        })], `[[`, "synthetic")))
        any(tolower(worcs_data) %in% tolower(tracked$path))
      }
    }, error = function(e) {
      FALSE
    })
    if (checks$pass[checks$name == "data"]) {
      checks$pass[checks$name == "data_checksums"] <- tryCatch({
        cs_now <- sapply(worcs_data, digest, file = TRUE)
        names(cs_now) <- worcs_data
        cs_stored <- unlist(worcsfile$checksums)
        if (all(names(cs_now) %in% names(cs_stored))) {
          all(sapply(names(cs_now), function(x) {
            cs_now[x] == cs_stored[x]
          }))
        }
        else {
          FALSE
        }
      }, error = function(e) {
        FALSE
      })
    }
    else {
      checks$pass[checks$name == "data_checksums"] <- FALSE
    }
    checks$pass[checks$name == "code"] <- any(endsWith(f_lc,
                                                       ".r"))
    checks$pass[checks$name == "preregistration"] <- any(endsWith(f_lc,
                                                                  "preregistration.rmd"))
  }
  if (verbose) {
    tmp <- apply(checks[worcs_checklist$check, ], 1, function(thisrow) {
      col_message(thisrow["description"], success = thisrow["pass"])
    })
  }
  return(checks)
}
