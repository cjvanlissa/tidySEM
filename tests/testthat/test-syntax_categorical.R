if(FALSE){


set.seed(1)

data("jointdata", package ="OpenMx")

jointdata[,c(2,4,5)] <-
  mxFactor(jointdata[,c(2,4,5)],
           levels=list(c(0,1), c(0, 1, 2, 3), c(0, 1, 2)))

jointdata$extra <- rnorm(nrow(jointdata))

m1 <- mxModel(
    "m1", type="RAM",
    manifestVars = paste0('z',sample.int(5,5)),
    latentVars='G',
    mxData(jointdata[,sample.int(5,5)], "raw", verbose=0L),
    mxPath('one', paste0('z', c(1,3))),
    mxPath(paste0('z', c(1,3)), arrows=2, values=2),
    mxPath(paste0('z', c(2,4,5)), arrows=2, free=FALSE, values=.5),
    mxPath('G', arrows=2, values=1, free=FALSE),
    mxPath('G', paste0('z', 1:5), values=1),
    mxMarginalProbit(paste0('z', c(2,5)), nThresh=c(1,2), free=TRUE),
    mxMarginalPoisson('z4', lambda = .5))

mxMargin
summary(m1)
m1 <- mxRun(m1)
m1$Thresholds

HS9 <- HolzingerSwineford1939[,c("x1","x2","x3","x4","x5", "x6","x7","x8","x9")]
HSbinary <- data.frame(lapply(cbind(data.frame( lapply(HS9[c(1,4)], cut, 2, labels=FALSE)),
                  data.frame( lapply(HS9[c(2,3,5,6,7)], cut, 3, labels=FALSE)),
                  data.frame( lapply(HS9[c(8:9)], cut, 4, labels=FALSE))), as.ordered))

model <- ' visual =~ x1 + x2 + x3
textual =~ x4 + x5 + x6
speed =~ x7 + x8 + x9
x8 | NA*t1
x8 | NA*t2
x8 | a*t3
x9 | NA*t1
x9 | NA*t2
x9 | a*t3'

lavaanify(model)

names(HSbinary) <- paste0(rep(c("vis", "tex", "spd"), each = 3), "_", 1:3)
tmp <- tidy_sem(HSbinary)
tmp$dictionary$type <- "cat"
tmp <- measurement(tmp)
tmp$syntax
HS <- data.frame(lapply(HolzingerSwineford1939[paste0("x", 1:9)], function(i){ordered(cut(i, 2))}))
mod_simple <- ' visual =~ x1 + x2 + x3
textual =~ x4 + x5 + x6
speed =~ x7 + x8 + x9'

res <- run_mx(as_ram(mod_simple, data = HS), data = HS)

mod_thres <- ' visual =~ x1 + x2 + x3
textual =~ x4 + x5 + x6
speed =~ x7 + x8 + x9
x8 | a*t3
x9 | a*t3'

as_ram(mod_thres, data = HSbinary)



fit <- cfa(model, data=HSbinary, ordered=names(HSbinary))
summary(fit, fit.measures=TRUE)
}
