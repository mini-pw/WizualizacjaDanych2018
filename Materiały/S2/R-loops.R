x <- c()

for(i in 1L:10) {
  x[i] <- i == 5
}

x

x <- 1L:10
x == 5

sapply(1:10, function(i) i == 5)
