library(MplusAutomation)
df <- HolzingerSwineford1939

names(df)[grepl("^x", names(df))] <- c("vis_1", "vis_2", "vis_3", "tex_1", "tex_2", "tex_3", "spe_1", "spe_2", "spe_3")
dict <- get_dictionary(df)
measurement(dict) -> model

res_lav <- sem(as.lavaan(model), data = df)

res_mplus <- mplusModeler(mplusObject(MODEL = as.mplus(model), rdata = df), modelout = "test.inp", run = 1L)

sm_lv <- summary(res_lav)
sm_mp <- summary(res_mplus)

table_results(res_lav)
table_results(res_mplus, all = T)


coef(res_mplus)
