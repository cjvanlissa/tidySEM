# Lo-Mendell-Rubin Likelihood Ratio Test

A likelihood ratio test for class enumeration in latent class analysis,
proposed by Lo, Mendell, & Rubin (2001) based on work by Vuong (1989).
See Details for important clarification.

## Usage

``` r
lr_lmr(x, ...)
```

## Arguments

- x:

  An object for which a method exists.

- ...:

  Additional arguments.

## Value

A `data.frame` containing the Z-value for the likelihood ratio test, its
p-value, df (which indicates the difference in number of parameters, not
true degrees of freedom, which may be zero), w2 (omega squared)
statistic for the test of distinguishability, an its p-value.

## Details

The likelihood ratio test for non-nested models, based on work by Vuong
(1989), is often used for class enumeration in latent class analysis
(see Lo, Mendell, & Rubin, 2001). Following work by Merkle, You, &
Preacher (2016), the models to be compared must first be tested for
distinguishability in the population, using the `w2` test. The null
hypothesis is that the models are indistinguishable. If this null
hypothesis is not rejected, there is no point in statistical model
comparison, either using the LMR LRT or other statistics. If the null
hypothesis is rejected, the LMR LRT can be evaluated using a Z-test.
This function wraps `\link[nonnest2]{vuongtest}` to perform that test.

## References

Lo Y, Mendell NR, Rubin DB. Testing the number of components in a normal
mixture. Biometrika. 2001;88(3):767-778.
[doi:10.1093/biomet/88.3.767](https://doi.org/10.1093/biomet/88.3.767)

Vuong, Q. H. (1989). Likelihood ratio tests for model selection and
non-nested hypotheses. Econometrica, 57, 307-333.
[doi:10.2307/1912557](https://doi.org/10.2307/1912557)

Merkle, E. C., You, D., & Preacher, K. (2016). Testing non-nested
structural equation models. Psychological Methods, 21, 151-163.
[doi:10.1037/met0000038](https://doi.org/10.1037/met0000038)

## Examples

``` r
if(requireNamespace("OpenMx", quietly = TRUE)){
library(OpenMx)
df <- iris[c(1:5, 100:105), 1:3]
names(df) <- letters[1:3]
res <- mx_profiles(df, classes = 1:2)
lr_lmr(res)
}
#> Running mix1 with 6 parameters
#> Running mix2 with 10 parameters
#> Running mix2 with 10 parameters
#> Lo-Mendell-Rubin adjusted Likelihood Ratio Test:
#> 
#>  null  alt   lr df        p   w2  p_w2
#>  mix1 mix2 3.77  4 8.31e-05 2.44 0.258
```
