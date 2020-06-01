# print.layout_matrix <- function(x, ...){
#   tab <- rbind(paste0("[, ", 1:ncol(x), "]"), x)
#   tab[is.na(tab)] <- ""
#   tab <- apply(tab, 2, function(i){
#     sprintf(paste0("%-", max(nchar(i, type = "width")), "s"), i)
#   })
#
#
#   dims <- dim(tab)
#   #tab <- rbind(tab, sapply(tab[1,], function(i){
#   #  paste0(rep("-", nchar(i)), collapse = "")
#   #}))
#   #tab <- tab[c(1, nrow(tab), 2:(nrow(tab)-1)), ]
#
#   tab[, 1:(ncol(tab)-1)] <- paste0(tab[, 1:(ncol(tab)-1)], " | ")
#   header <- tab[1,]
#   tab <- tab[-1, ]
#   lines <- matrix(rep(sapply(tab[1,], function(i){paste0(rep("-", (nchar(i))), collapse = "")}), nrow(tab)), nrow = nrow(tab), byrow = TRUE)
#   out <- matrix(t(cbind(lines, tab)), ncol = ncol(tab), byrow = TRUE)
#
#   #print(out[-nrow(out), ], quote = FALSE)
#   spaces <- paste0(rep(" ", 5+nchar(nrow(out))), collapse = '')
#   cat(spaces, header, "\n", sep = "")
#   for(i in 1:(nrow(out))){
#     cat(ifelse(i %% 2 == 0, paste0("[", i%/%2, ", ] "), spaces), out[i, ], "\n", sep = "")
#     }
# }
