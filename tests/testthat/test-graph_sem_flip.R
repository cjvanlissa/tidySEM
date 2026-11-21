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

layout <- get_layout(
  "x1", "grade", "", "ageyr", "",
  "x2", "", "visual", "", "",
  "x3", "textual", "", "speed", "x9",
  paste0("x", 4:8), rows = 4)

tmp = prepare_graph(fit, layout = layout)
# wrn <- getOption("warn")
# options(warn=0)
test_that("curves flipped correctly when nodes have same x coordinates", {
  expect_no_warning(print(plot(tmp)))
})
