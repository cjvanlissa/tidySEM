# Version 0.1.2

This resubmission addresses the comment by CRAN-maintainer Swetlana Herbrandt:
* We see code lines such as write.csv(table_descriptives, "scale table.csv", 
  row.names = FALSE)
  + Response:
    We have removed the calls to write.csv(). It is now up to users to
    save the results of analyses (makes more sense, anyway).

## Test environments

* local Windows 10 install, R 4.0.0
* win-builder: release: R version 4.0.0 (2020-04-24)
* win-builder: oldrelease: R version 3.6.3 (2020-02-29)
* win-builder: devel: R Under development (unstable) (2020-05-15 r78473)
* rhub check: Fedora Linux, R-devel, clang, gfortran
* Travis Ubuntu 16.04.6 LTS, R release

## R CMD check results

0 errors | 0 warnings | 1 notes

NOTE: New submission
