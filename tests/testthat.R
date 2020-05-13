library(testthat)
library(tidySEM)
library(MplusAutomation)
if(mplusAvailable(TRUE) == 0){
  options("test_mplus" = TRUE)
} else {
  options("test_mplus" = FALSE)
}
# To run all mplus tests, run the code below once to create a file that is not
# tracked by git, which toggles the option to run mplus tests:
# writeLines('options("test_mplus" = TRUE)', con = "tests/test_local.R")
test_check("tidySEM")
# Clean up after test_mplus
if(getOption("test_mplus")){
    remove_files <- list.files(pattern = "\\.(inp|out|dat|log)$", full.names = TRUE)
    if(length(remove_files) > 0){
        invisible(file.remove(remove_files))
    }
}
