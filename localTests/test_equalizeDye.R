test <- lapply(images, as.numeric)

foo <- lapply(test, sample, 10000)
a <- quantile(foo, seq(0,1,0.1))[[9]]
bar <- log(foo - a)
b <- exp(1) ^ quantile(bar, na.rm=TRUE)[[4]]

m1 <- unlist(lapply(test, min))
m2 <- unlist(lapply(test, median))
m3 <- unlist(lapply(test, max))
plot(m1, ylim=range(c(m1,m2*6)), type="n")
lines(m1, col="black")
lines(m2, col=rgb(0.6,0.6,0.6))
lines(m3, col=rgb(0.8,0.8,0.8))
points(m1,col="black",pch=19)
points(m2,col=rgb(0.6,0.6,0.6),pch=19)
points(m3,col=rgb(0.8,0.8,0.8),pch=19)
legend("topright",legend=c("maxValue","medianValue","minValue"),
       fill=c(rgb(0.8,0.8,0.8),rgb(0.6,0.6,0.6),"black"))


images <- lapply(xy$red, function(x) x*5)

test <- lapply(images, as.numeric)
foo <- lapply(test, sample, 10000)

startTime <- as.POSIXct("2015-07-06 17:00:00")
endTime <- startTime + lubridate::dhours((length(foo)-1)*3)
timeAxis <- seq(startTime,endTime,length.out=length(foo))
names(foo) <- lubridate::hour(timeAxis)

boxplot(foo, pch=19, cex=0.3, ylim=range(0,1), outcol=rgb(0,0,0,0.05),
        main=paste0(xyName, " red * 5"))


((1:length(foo))-1)

plot()
for (f in foo) {
  
}

edtest <- function(images) {
  
  images <- lapply(images, filter_blur, 13)
  
  t1 <- unlist(lapply(images, median))
  t2 <- 0.6 / t1
  t3 <- mapply(function(x,y) return((x*y)), images, t2, SIMPLIFY=FALSE)
  t4 <- lapply(t3, function(x) ((x-0.4)*3)^3)
  
  return(t4)
  
}


t1 <- edtest(xy$red)
t2 <- edtest(xy$green)
t3 <- edtest(xy$red)
t4 <- edtest(xy$green)
t5 <- edtest(xy$red)
t6 <- edtest(xy$green)

createGifFromList(t1, "solidxy3red.gif", delay=30, rescale=30)
createGifFromList(t2, "solidxy3green.gif", delay=30, rescale=30)
createGifFromList(t3, "solidxy2red.gif", delay=30, rescale=30)
createGifFromList(t4, "microfluidxy06green.gif", delay=30, rescale=30)

createGifFromList(t5, "microfluid2xy06red.gif", delay=30, rescale=30)
createGifFromList(t6, "microfluid2xy06green.gif", delay=30, rescale=30)
