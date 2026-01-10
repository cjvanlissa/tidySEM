writeLines("TRUE", "vignettes/run_everything.txt")
writeLines("TRUE", "vignettes/pkgdown.txt")
f <- list.files("vignettes", pattern = ".Rmd$", full.names = TRUE)
for(thisf in f){
  rmarkdown::render(thisf, clean = FALSE)
}
writeLines("FALSE", "vignettes/run_everything.txt")
devtools::build_vignettes()
pkgdown::build_site()
writeLines("FALSE", "vignettes/pkgdown.txt")
devtools::check()
worcs::git_update("run everything in vignettes and update site")
