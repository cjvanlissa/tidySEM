# SMART-LCA Checklist

This Vignette describes the SMART-LCA Checklist: Standards for More
Accuracy in Reporting of different Types of Latent Class Analysis,
introduced in Van Lissa, C. J., Garnier-Villarreal, M., & Anadria, D.
(2023). *Recommended Practices in Latent Class Analysis using the
Open-Source R-Package tidySEM.* Structural Equation Modeling.
<https://doi.org/10.1080/10705511.2023.2250920>. This version of the
checklist corresponds to the `tidySEM` R-package’s version , 0.2.11.

The paper discusses the best practices on which the checklist is based
and describes specific points to check when writing, reviewing, or
reading a paper in greater detail. However, this vignette may be updated
to keep pace with `tidySEM` package development, whereas the print
publication will remain static.

Note that, although the steps below are numbered for reference purposes,
we acknowledge the process of conducting and reporting research is not
always linear.

1.  Pre-Analysis
    - Define a research question and study design suitable for LCA.
    - Determine whether the application of LCA is confirmatory or
      exploratory.
    - Choose valid and reliable indicators with an appropriate data type
      for LCA.
    - Justify sample size.
    - Optionally: pre-register the analysis plan.
    - Optionally: use Preregistration-As-Code.
2.  Examining Observed Data
    - Assess indicator distributions and scales.
      - [`descriptives()`](https://cjvanlissa.github.io/tidySEM/reference/descriptives.md)
    - Does the observed distribution match the intended measurement
      level? E.g., continuous variables have many unique values,
      categorical variables have a well-balaced response distribution.
    - Are all indicators on similar scales?
    - Are there distributional idiosyncrasies that need to be accounted
      for in model specification or by transforming the data (e.g.,
      extreme skew)?
3.  Data Preprocessing
    - Make sure continuous variables have type `numeric` or `integer`
      and an interval measurement level.
    - Convert ordered and binary variables to `mxFactor`.
      - [`mxFactor()`](https://rdrr.io/pkg/OpenMx/man/mxFactor.html)
    - Convert nominal variables to binary dummy variables.
      - [`mx_dummies()`](https://cjvanlissa.github.io/tidySEM/reference/mx_dummies.md)
    - Rescale indicators that are on very different scales to prevent
      convergence issues.
      - [`poms()`](https://cjvanlissa.github.io/tidySEM/reference/poms.md),
        [`scale()`](https://rdrr.io/r/base/scale.html)
    - Optionally, transform data to account for distributional
      idiosyncracies that could cause the model to fit poorly.
      - [`sqrt()`](https://rdrr.io/r/base/MathFun.html),
        [`log()`](https://rdrr.io/r/base/Log.html)
4.  Missing data
    - Examine and report proportion of missingness per variable.
      - [`descriptives()`](https://cjvanlissa.github.io/tidySEM/reference/descriptives.md)
    - Report MAR test using
      [`mice::mcar()`](https://amices.org/mice/reference/mcar.html).
    - Select and report appropriate method to handle missingness. In
      `tidySEM`, this is full information maximum likelihood (FIML),
      which is valid regardless of the outcome of the MAR test.
5.  Model specification
    - For exploratory LCA, list all sensible alternative specifications
    - In confirmatory LCA, justify alternative model specifications on
      theoretical grounds
    - Clearly report the model specification, so it is clear what all
      parameters are.
    - Optionally, verify that the model can capture distributional
      idiosyncracies (see Data Preprocessing).
6.  Number of classes
    - Justify the maximum number of classes $k$.
    - In exploratory LCA, estimate 1:$k$ classes.
    - In confirmatory LCA, estimate the theoretical number of classes
      and choose other models to benchmark it against. This should
      include a 1-class model, optionally competing theoretical models,
      and optionally models with a different numbers of classes.
7.  Justify the criteria used for class enumeration, which can include:
    - Information criteria.
    - LR tests.
      - The
        [`BLRT()`](https://cjvanlissa.github.io/tidySEM/reference/BLRT.md)
        is best supported by the literature;
        [`lr_lmr()`](https://cjvanlissa.github.io/tidySEM/reference/lr_lmr.md)
        has been criticized but is also available.
    - Theoretically expected class solution.
8.  Justify the criteria used to eliminate models from consideration,
    including:
    - Model convergence.
    - Classification diagnostics.
      - [`table_fit()`](https://cjvanlissa.github.io/tidySEM/reference/table_fit.md)
    - Local identifiability (acceptable number of observations per
      parameter in each class).
    - Theoretical interpretability of the results.
      - Columns `np_global` and `np_local` in output of
        [`table_fit()`](https://cjvanlissa.github.io/tidySEM/reference/table_fit.md)
9.  Transparency and Reproducibility
    - Use free open-source software to enable reproducibility.
      - See the `worcs` package and its Vignettes, [Van Lissa et
        al. (2021)](https://doi.org/10.3233/DS-210031)
    - Comprehensively cite literature, software, and data sources.
    - Share analysis code.
    - If possible, share data. Otherwise, share a synthetic dataset.
      - Synthetic data can be created for an entire dataset in a
        model-agnostic way via `worcs::synthetic()`
      - Synthetic data can be generated from the LCA model by running
        [`mxGenerateData()`](https://rdrr.io/pkg/OpenMx/man/mxGenerateData.html)
        on the model object.
    - Share all digital research output in line with the FAIR
      principles.
    - Optionally, use the Workflow for Open Reproducible Code in Science
      and the `worcs` R-package automate creating a reproducible
      research archive.
10. Estimation and Convergence
    - Report the method of estimation. In `tidySEM`, this is simulated
      annealing with informative start values, determined via K-means
      clustering for complete data and hierarchical clustering when
      there are missing values.
    - Assess convergence of all models before interpreting them.
    - Optionally, if there is non-convergence, use functions like
      [`mxTryHard()`](https://rdrr.io/pkg/OpenMx/man/mxTryHard.html) to
      aid convergence.
      - E.g., if you have concerns about model 4 in output object `res`,
        you could run
        `res[[4]] <- mxTryHard(res[[4]], extraTries = 200)` to run 200
        permutations.
11. Reporting Results
    - Report the fit of all models under consideration in a table.
      - [`table_fit()`](https://cjvanlissa.github.io/tidySEM/reference/table_fit.md)
    - Report classification diagnostics for all models under
      consideration.
      - `class_probs()`
    - Report the minimal class proportions for all models under
      consideration.
      - Column `n_min` in
        [`table_fit()`](https://cjvanlissa.github.io/tidySEM/reference/table_fit.md)
        results.
    - Report class proportions for the final model
      - `class_prob(res, "sum.posterior")`
    - Report point estimates, standard errors and confidence intervals
      for model parameters.
      - [`table_results()`](https://cjvanlissa.github.io/tidySEM/reference/table_results.md)
    - Optionally, convert thresholds to conditional item probabilities.
      - [`table_prob()`](https://cjvanlissa.github.io/tidySEM/reference/table_prob.md)
    - Assign informative class names while clarifying that these names
      are just shorthand.
12. Inference
    - Standard p-values in
      [`table_results()`](https://cjvanlissa.github.io/tidySEM/reference/table_results.md)
      test the hypothesis that parameters are equal to zero. Consider
      whether these tests are meaningful and relevant.
    - Control for, or reflect on, the study-wide Type I error level.
    - Optionally, use the standard errors to test other null hypotheses.
    - Optionally, use
      [`wald_test()`](https://cjvanlissa.github.io/tidySEM/reference/wald_test.md)
      to test informative hypotheses.
    - Optionally, to test a confirmatory LCA model, compare parameter
      estimates to hypothesized values.
    - Optionally, use
      [`lr_test()`](https://cjvanlissa.github.io/tidySEM/reference/lr_test.md)
      to compare parameters across classes.
13. Visualization
    - Visualize raw data to assess class separability and model fit.
      - [`plot_profiles()`](https://cjvanlissa.github.io/tidySEM/reference/plot_profiles.md),
        [`plot_density()`](https://cjvanlissa.github.io/tidySEM/reference/plot_density.md),
        [`plot_prob()`](https://cjvanlissa.github.io/tidySEM/reference/plot_prob.md),
        [`plot_growth()`](https://cjvanlissa.github.io/tidySEM/reference/plot_growth.md)
    - Show point estimates of model parameters.
    - If possible, show confidence bounds.
    - Optionally, show multivariate distributions for multivariate
      models.
14. Follow-up analyses.
    - Account for classification inaccuracy in follow-up analyses.
      - [`BCH()`](https://cjvanlissa.github.io/tidySEM/reference/BCH.md)
