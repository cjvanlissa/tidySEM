df <- iris[c(1:5, 50:55, 100:105), 1:4]
names(df) <- letters[1:4]
res <- mx_profiles(df, 1:2)
