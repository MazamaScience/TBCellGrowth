test <- lapply(xy$green, as.numeric)

foo <- unlist(lapply(test, sample, 100000))

a <- quantile(foo, seq(0,1,0.1))[[9]]

bar <- log(foo - a)

b <- exp(1) ^ quantile(bar, na.rm=TRUE)[[4]]


t1 <- lapply(xy$green, function(x) x[3535:3555, 500:520])
t2 <- 0.6 / unlist(lapply(t1, mean))

t3 <- mapply(function(x,y) return((x*y)), xy$green, t2, SIMPLIFY=FALSE)

t4 <- lapply(t3, function(x) filter_blur((x-0.4) * 3,7)^2)
