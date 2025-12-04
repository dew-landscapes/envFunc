x <- sample(c(letters, rep(NA, 10)), 100, replace = TRUE)
get_mode(x)
x <- x |> factor(levels = letters)
get_mode(x)
get_mode(x, useNA = "always")

x <- c(10, 10, 20, 20)
get_mode(x)
get_mode(x, ties = "blah")
