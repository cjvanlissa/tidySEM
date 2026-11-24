devtools::document()
devtools::spell_check()
Sys.setenv(
  TIDYSEM_RUN_EVERYTHING = "true",
  TIDYSEM_SHOW_RESULTS = "true"
)
f <- list.files("vignettes", pattern = ".Rmd$", full.names = TRUE)
for(thisf in f[1:length(f)]){
  rmarkdown::render(thisf)
}
Sys.setenv(
  TIDYSEM_RUN_EVERYTHING = "false",
  TIDYSEM_SHOW_RESULTS = "true"
)

pkgdown::build_site()


if(FALSE){
  Sys.setenv(
    TIDYSEM_RUN_EVERYTHING = "false",
    TIDYSEM_SHOW_RESULTS = "false"
  )
  f <- list.files("vignettes", pattern = ".Rmd$", full.names = TRUE)
  timez <- lapply(f, function(thisf){
    strt <- Sys.time()
    rmarkdown::render(thisf)
    difftime(strt, Sys.time(), units = "secs")

  })
  library(testthat)

  files <- list.files(
    "tests/testthat",
    pattern = "^test-.*\\.R$",
    full.names = TRUE
  )

  timings <- lapply(files, function(file) {
    gc()

    elapsed <- system.time(
      tryCatch(
        testthat::test_file(
          file,
          reporter = "silent"
        ),
        error = function(e) {
          message("Error in ", basename(file), ": ", conditionMessage(e))
        }
      )
    )[["elapsed"]]

    data.frame(
      file = basename(file),
      seconds = elapsed
    )
  })

  timings <- do.call(rbind, timings)
  timings <- timings[order(-timings$seconds), ]

  print(timings, row.names = FALSE)
}
Sys.setenv(
  TIDYSEM_RUN_EVERYTHING = "false",
  TIDYSEM_SHOW_RESULTS = "false",
  TIDYSEM_TEST_LOCAL = "true"
)
devtools::check()
devtools::run_examples(run_dontrun = TRUE, run_donttest = TRUE)
devtools::check_win_devel()
devtools::check_win_release()
devtools::check_win_oldrelease()
worcs::git_update("run everything in vignettes and update site")
