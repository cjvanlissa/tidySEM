# tidySEM 0.2.10

* Export predict_class() instead of masking OpenMx::predict.mxModel() with
  tidySEM::predict.mxModel()
* Add pmc_srmr() for class enumeration
* Add mx_mixed_lca() for latent class analysis with continuous and categorical
  indicators.
* Add mx_data_quantiles() for data-driven starting values for categorical 
  indicators.
* Internal mixgrads() function is now incorporated in OpenMx version >= 2.21.13,
  so require that version and rely on OpenMx::imxRowGradients()
* Rewrite run_mx()
* Bugfix to plot_density()
* Bugfix to graph_sem()
* Bugfix to table_results.mxModel()
* Fix GitGub issue #116: lavaan is deprecating lavaan:::vnames, replaced with
  lavaan::lavNames()
* Fix GitHub issue #115: Wald tests resulted in empty data.frame when progress
  was not loaded.
* Vignettes should show that OpenMx must be loaded in order to run functions
  that depend on it (i.e., latent class analyses)
* Remove spaces from lavaan operators to be compatible with new parser

## Test environments

* local Windows 10 x64, R 4.4.3
* local Linux 6.14.0-37-generic 24.04.1-Ubuntu, R 4.5.0
* GitHub Actions (r-lib): windows-latest (release)
* GitHub Actions (r-lib): macOS-latest (release)
* GitHub Actions (r-lib): ubuntu-latest (release)
* GitHub Actions (r-lib): ubuntu-latest (devel)
* win-builder: R version 4.4.3 (2025-02-28 ucrt)
* win-builder: R Under development (unstable) (2025-12-24 r89227 ucrt)
* win-builder: R version 4.5.2 (2025-10-31 ucrt)
* Rhub os: linux (R-devel)
* Rhub os: m1-san (R-devel)
* Rhub os: macos-arm64 (R-devel)

## R CMD check results

0 errors | 0 warnings | 0 notes
