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

Sys.setenv(
  TIDYSEM_RUN_EVERYTHING = "false",
  TIDYSEM_SHOW_RESULTS = "false"
)
devtools::check()
devtools::run_examples(run_dontrun = TRUE, run_donttest = TRUE)
devtools::check_win_devel()
devtools::check_win_release()
devtools::check_win_oldrelease()
worcs::git_update("run everything in vignettes and update site")
