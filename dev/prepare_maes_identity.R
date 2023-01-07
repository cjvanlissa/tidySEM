range(df$geboortejaar, na.rm = T)
table(df$teachervictimizationdichotoom)
res <- readRDS("mx_lca_6.rdata")
maes_identity <- mxGenerateData(res[[6]])

head(maes_identity)

resmaes <- mx_lca(maes_identity, 3)
saveRDS(resmaes, "ressynth.rdata")
maes_identity$class <- class_prob(resmaes)$individual[,4]

maes_identity$belgianborn <- rbinom(nrow(maes_identity), 1, prob = 0.1258237)
maes_identity$belgianborn[maes_identity$class == 2] <- rbinom(sum(maes_identity$class == 2), 1, prob = .24)
maes_identity$belgianborn[maes_identity$class == 3] <- rbinom(sum(maes_identity$class == 3), 1, prob = .17)
maes_identity$belgianborn <- factor(maes_identity$belgianborn, levels = 0:1, labels = c("no", "yes"))

df$teachervictimizationdichotoom[df$teachervictimizationdichotoom == 999] <- NA
df$teachervictimizationdichotoom <- as.integer(df$teachervictimizationdichotoom)-1
model <- glm(teachervictimizationdichotoom ~class,family=binomial(link='logit'),data=df)
model

maes_identity$vict_teacher <- rbinom(nrow(maes_identity), 1, prob = logit2prob(coef(model)[1]))
maes_identity$vict_teacher[maes_identity$class == 2] <- rbinom(sum(maes_identity$class == 2), 1, prob = logit2prob(sum(coef(model)[1:2])))
maes_identity$vict_teacher[maes_identity$class == 3] <- rbinom(sum(maes_identity$class == 3), 1, prob = logit2prob(sum(coef(model)[c(1,3)])))
maes_identity$vict_teacher <- factor(maes_identity$vict_teacher, levels = 0:1, labels = c("no", "yes"))

maes_identity$ses <- rnorm(nrow(maes_identity), mean = 37.297, sd = sd(df$ses))
maes_identity$ses[maes_identity$class == 2] <- rnorm(sum(maes_identity$class == 2), mean = 37.89, sd = sd(df$ses))
maes_identity$ses[maes_identity$class == 3] <- rnorm(sum(maes_identity$class == 3), mean = 37, sd = sd(df$ses))
maes_identity$ses <- scales::rescale(maes_identity$ses, to = range(df$ses))

maes_identity$age_belgium <- rnorm(nrow(maes_identity), mean = 8.23, sd = sd(df$leeftijdbelgië, na.rm = T))
maes_identity$age_belgium[maes_identity$class == 2] <- rnorm(sum(maes_identity$class == 2), mean = 10.77, sd = sd(df$leeftijdbelgië, na.rm = T))
maes_identity$age_belgium[maes_identity$class == 3] <- rnorm(sum(maes_identity$class == 3), mean = 8.58, sd = sd(df$leeftijdbelgië, na.rm = T))
maes_identity$age_belgium <- scales::rescale(maes_identity$age_belgium, to = c(0,8))
maes_identity$age_belgium <- as.integer(round(maes_identity$age_belgium))
summary(lm(age_belgium~I(factor(class)), maes_identity))

model <- glm(gepest ~class,family=binomial(link='logit'),data=df)
model

maes_identity$vict_bully <- rbinom(nrow(maes_identity), 1, prob = logit2prob(coef(model)[1]))
maes_identity$vict_bully[maes_identity$class == 2] <- rbinom(sum(maes_identity$class == 2), 1, prob = logit2prob(sum(coef(model)[1:2])))
maes_identity$vict_bully[maes_identity$class == 3] <- rbinom(sum(maes_identity$class == 3), 1, prob = logit2prob(sum(coef(model)[c(1,3)])))
maes_identity$vict_bully <- factor(maes_identity$vict_bully, levels = 0:1, labels = c("no", "yes"))

model <- glm(vict_bully ~I(factor(class)),family=binomial(link='logit'),data=maes_identity)
model


df$sex <- as.integer(df$geslacht)-1
table(df$sex, df$geslacht)
model <- glm(sex ~I(factor(class))-1,family=binomial(link='logit'),data=df)
model
maes_identity$sex <- rbinom(nrow(maes_identity), 1, prob = logit2prob(coef(model)[1]))
maes_identity$sex[maes_identity$class == 2] <- rbinom(sum(maes_identity$class == 2), 1, prob = logit2prob(sum(coef(model)[2])))
maes_identity$sex[maes_identity$class == 3] <- rbinom(sum(maes_identity$class == 3), 1, prob = logit2prob(sum(coef(model)[3])))
maes_identity$sex <- factor(maes_identity$sex, levels = 0:1, labels = c("boy", "girl"))


df$age <- 2014 - (1900+df$geboortejaar)
model <- lm(age~I(factor(class))-1, df)
maes_identity$age <- rnorm(nrow(maes_identity), mean = coef(model)[1], sd = sd(df$age, na.rm = TRUE))
maes_identity$age[maes_identity$class == 2] <- rnorm(sum(maes_identity$class == 2), mean = coef(model)[2], sd = sd(df$age, na.rm = TRUE))
maes_identity$age[maes_identity$class == 3] <- rnorm(sum(maes_identity$class == 3), mean = coef(model)[3], sd = sd(df$age, na.rm = TRUE))
maes_identity$age <- as.integer(round(maes_identity$age))
maes_identity[1:5] <- lapply(maes_identity[1:5], ordered)

maes_identity$age_belgium[maes_identity$belgianborn == "yes"] <- 0
maes_identity <- maes_identity[c("Ethnic_1", "Ethnic_2", "Ethnic_3", "Belgian", "Flemish", "age", "sex", "ses","belgianborn", "age_belgium", "vict_bully", "vict_teacher")]

saveRDS(maes_identity, "c:/tmp/maes_identity.rdata")
