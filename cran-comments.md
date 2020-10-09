# Version 0.1.3

* Changed arguments for graph_sem.lavaan and graph_sem.mplus.model; these gain
  arguments "label", "edges", "layout", and "nodes",
  which apply get_edges(), get_layout(), and get_nodes() to the model argument,
  respectively, passing on the "label" argument to get_edges() and get_nodes().
* The default label has been changed to "est_sig" throughout the package. Some
  functions used "est_sig_std" instead.
* Minor bugfixes
* Fix bug where > 26 nodes resulted in wrong plot

## Test environments

* local Windows 10 install, R 4.0.2
* win-builder: release, R 4.0.2 (2020-06-22)
* win-builder: oldrelease: R version 3.6.3 (2020-02-29)
* win-builder: devel: R Under development (unstable) (2020-09-28 r79268)
* rhub check: Windows Server 2008 R2 SP1, R-devel, 32/64 bit
* rhub check: Fedora Linux, R-devel, clang, gfortran
* rhub check: Ubuntu Linux 16.04 LTS, R-release, GCC
  + Preperror: Dependency 'openssl' not available
* Travis linux, release

## R CMD check results

0 errors | 0 warnings | 0 notes
