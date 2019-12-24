results <- eval(parse(text = '
structure(list(label = c("AonFcontrol", "AonFnegative", "AonFpositive",
"AonMcontrol", "AonMnegative", "AonMpositive", "FwithMcontrol",
"FwithMnegative", "FwithMpositive", "D_control", "D_Fcontrolnegative",
"D_Fcontrolpositive", "D_Mcontrolnegative", "D_Mcontrolpositive",
"D_negative", "D_positive"), est_sig = c("0.05", "0.10***", "0.11***",
"0.12**", "0.11***", "0.14***", "0.41***", "0.40***", "0.32***",
"0.07", "-0.05", "-0.01", "0.01", "-0.02", "0.01", "0.03"), se = c(0.0280255922646062,
0.0243406397948112, 0.0130441569377656, 0.0410735273929362, 0.019635930039265,
0.0264746318281699, 0.0874976035093974, 0.0903739396005978, 0.0769058069928882,
NA, NA, NA, NA, NA, NA, NA), pvalue = c(0.0923984180376511, 3.7306260433318e-05,
2.67380016410931e-17, 0.00324674009490095, 1.14164830503634e-08,
2.42928787685264e-07, 3.56254332762708e-06, 1.10035266981143e-05,
2.44449548448537e-05, NA, NA, NA, NA, NA, NA, NA), confint = c("[-0.01, 0.10]",
"[0.05, 0.15]", "[0.08, 0.14]", "[0.04, 0.20]", "[0.08, 0.15]",
"[0.08, 0.19]", "[0.23, 0.58]", "[0.22, 0.57]", "[0.17, 0.48]",
"[-0.01, 0.18]", "[-0.13, 0.02]", "[-0.06, 0.04]", "[-0.08, 0.05]",
"[-0.09, 0.04]", "[-0.02, 0.04]", "[-0.02, 0.07]")), class = "data.frame", row.names = c(NA, -16L))
'))

lay <- get_layout("M", "",
                  "", "A",
                  "F", "", rows = 3)
nodes <- data.frame(name = rep(c("A", "M", "F"), 3),
                    label = rep(c("Child", "Mother", "Father"), 3),
                    group = rep(c("control", "negative", "positive"), each = 3))

nodes <- data.frame(name = c("A", "M", "F"),
                    label = c("Child", "Mother", "Father")
                    )

edges <- results[grepl("^[AF]", results$label), c("label", "est_sig")]
edges$from <- gsub("^.(on|with)(\\w).*$", "\\2", edges$label)
edges$to <- gsub("^(.)(on|with)(\\w).*$", "\\1", edges$label)
edges$group <- gsub("^.+(control|negative|positive)$", "\\1", edges$label)
edges$label <- edges$est_sig
edges$est_sig <- NULL


prep <- prepare_graph(edges, lay, nodes, angle = 0, fix_coord = FALSE)
edges(prep)[edges(prep)$from == "M" & edges(prep)$to == "F", c("arrow", "curvature")] <- rep(c("none", -60), each = 3)
p <- plot(prep)

