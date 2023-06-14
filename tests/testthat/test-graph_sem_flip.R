model <- "
visual =~ x1 + x2 + x3
textual =~ x4 + x5 + x6
speed =~ x7 + x8 + x9
speed ~ ageyr + grade
textual ~ ageyr + grade
x4 ~~ x5 + x6
textual ~~ speed
"

fit <- sem(model, HolzingerSwineford1939)

layout <- data.frame(
  IV = c("x1", "grade", "", "ageyr", ""),
  M = c("x2", "", "visual", "", ""),
  DV = c("x3", "textual", "", "speed", "x9"),
  DV.items = c(paste0("x", 4:8)))

tmp = prepare_graph(fit, layout = layout)
# wrn <- getOption("warn")
# options(warn=0)
test_that("curves flipped correctly when nodes have same x coordinates", {
  expect_no_warning(print(plot(tmp)))
})
