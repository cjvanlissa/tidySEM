library(OpenMx)
# Construct dataset with ordinal and categorical indicators
set.seed(1)
n = 200
mns <- c(rep(0, floor(.3*n)), rep(2, ceiling(.7*n)))
df <- rnorm(4*n, mean = rep(mns, 4))
df <- matrix(df, nrow = n)
df <- t(t(df) * c(1, 2, .5, 1))
df <- data.frame(df)
df$X4 <- cut(df$X4, 3, labels = FALSE)
df$X4 <- OpenMx::mxFactor(df$X4, levels = c(1:3))
# Estimate the model
res <- mx_mixed_lca(data = df, classes = 2)

res <- mxModel(res,
               mxConstraint(name = "thresholds_2_1", class1.Thresholds[2,1] == class2.Thresholds[2,1]))
run_mx(res) -> tmp

tmp <- res
tmp$class1 <- mxModel(tmp$class1,
                      mxMatrix("Full", nrow(tmp$class1$mat_dev), ncol = ncol(tmp$class1$mat_dev), free=FALSE, values=1, labels=c("class1.Thresholds[1,1]","class1.Thresholds[2,1]"), name="thmat1"))
tmp$class2 <- mxModel(tmp$class2,
                      mxMatrix("Full", nrow(tmp$class2$mat_dev), ncol = ncol(tmp$class2$mat_dev), free=FALSE, values=1, labels=c("class1.Thresholds[1,1]","class1.Thresholds[2,1]"), name="thmat2"))


tmp <- mxRun(tmp)
tmp$class1$thmat1$values
tmp$class2$thmat2$values


library(OpenMx)
# Construct dataset with ordinal and categorical indicators
set.seed(1)
n = 200
mns <- c(rep(0, floor(.3*n)), rep(2, ceiling(.7*n)))
df <- rnorm(4*n, mean = rep(mns, 4))
df <- matrix(df, nrow = n)
df <- t(t(df) * c(1, 2, .5, 1))
df <- data.frame(df)
df$X4 <- cut(df$X4, 3, labels = FALSE)
df$X4 <- OpenMx::mxFactor(df$X4, levels = c(1:3))
df$X3 <- cut(df$X3, 3, labels = FALSE)
df$X3 <- OpenMx::mxFactor(df$X3, levels = c(1:3))
df$X2 <- cut(df$X2, 3, labels = FALSE)
df$X2 <- OpenMx::mxFactor(df$X2, levels = c(1:3))
# Estimate the model
library(tidySEM)
mod <-
"
X2 | t1
X2 | a*t2
X3 | t1
X3 | a*t2
X4 | t1
X4 | a*t2
f =~ X2 + X3 + X4
f ~ X1
"
as_ram(mod, data = df) -> tmp
res <- mxRun(tmp)

# Check that all 2nd thresholds are equal

mod <-
  "
X2 | t1
X2 | a{C}*t2
X3 | t1
X3 | a{C}*t2
X4 | t1
X4 | a{C}*t2
f =~ X2 + X3 + X4
f ~ X1
"
mx_mixture(mod, classes = 2, data = df, run = FALSE) -> tmp

mod <-
  "
X2 | t1
X2 | t2
X3 | t1
X3 | t2
X4 | t1
X4 | t2
f =~ X2 + X3 + X4
f ~ X1
"
mx_mixture(mod, classes = 2, data = df, run = FALSE) -> tmp

res <- mxRun(tmp)
