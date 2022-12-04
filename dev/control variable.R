df <- iris[iris$Species %in% levels(iris$Species)[1:2], ]
df$Species <- as.integer(df$Species %in% levels(df$Species)[1])
names(df)[1:4] <- paste0("y", 1:4)
library(tiySEM)

mod <- as_ram("y1 ~ a*Species
              y1~~b*y1", fixed.x = TRUE)
mods <- list(mod, mod)
res4 <- mx_mixture(model = mods, data = df)
mod$A
mod$S
mod$M
res3 <- mx_mixture("y1 ~ a*Species
              y1~~b*y1", fixed.x = TRUE, classes = 2, data = df)
mod <- as_ram("y1 ~ a*Species")
mod$A
mod$S
mod$M

meanstructure = FALSE, int.ov.free = FALSE,
              int.lv.free = FALSE, orthogonal = FALSE, orthogonal.y = FALSE,
              orthogonal.x = TRUE, orthogonal.efa = FALSE, std.lv = FALSE,
              effect.coding = "", conditional.x = FALSE, fixed.x = FALSE,
              parameterization = "delta", constraints = NULL, ceq.simple = FALSE,
              auto = FALSE, model.type = "sem",
              auto.fix.first = FALSE, auto.fix.single = FALSE, auto.var = FALSE,
              auto.cov.lv.x = FALSE, auto.cov.y = FALSE, auto.th = FALSE,
              auto.delta = FALSE, auto.efa = FALSE,
              varTable = NULL, ngroups = 1L, nthresholds = NULL,
              group.equal = NULL, group.partial = NULL, group.w.free = FALSE,
              debug = FALSE, warn = TRUE, as.data.frame. = TRUE)
res <- mx_mixture(model =
"y1 ~ a*Species
Species~~.25*Species
Species~.5*1
y1 ~~ d*y1", classes = 2, data = df)
summary(res)
res2 <- mxTryHard(res, extraTries = 20)
summary(res2)

res <- mx_mixture(model =
                    "y1 ~ a*Species
Species~~.25*Species
Species~.5*1
y1 ~~ d*y1
y2 ~ b*y1
y2~~c*y2
y2~d*1", classes = 2, data = df)
summary(res)

library(MplusAutomation)
m1 <- mplusObject(VARIABLE = "CLASSES = c(2);",
                  ANALYSIS = "TYPE = MIXTURE;",
                  MODEL =
"%overall%
y1 ON Species;",
rdata = df[, c("y1", "Species")],
modelout = "test.inp",
run = 1L)

res1 <- lm(y1~Species, data =df)
summary(res1)

table_results(res2)
