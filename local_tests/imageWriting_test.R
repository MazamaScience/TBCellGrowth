
tryCatch({
  png("test1.png", 100, 100, res=1000)
  print("can write small png")
}, warning = function(w) {
  print("can write small png")
}, error = function(e) {
  print("can NOT write small png")
})

tryCatch({
  png("test2.png", 50000, 50000, res=1000)
  print("can write large png")
}, warning = function(w) {
  print("can write large png")
}, error = function(e) {
  print("can NOT write large png")
})

tryCatch({
  EBImage::writeImage(matrix(1,nrow=500,ncol=500), "test1.jpg")
  print("can write small jpg")
}, warning = function(w) {
  print("can write small jpg")
}, error = function(e) {
  print("can NOT write small jpg")
})

tryCatch({
  EBImage::writeImage(matrix(1,nrow=5000,ncol=5000), "test2.jpg")
  print("can write large jpg")
}, warning = function(w) {
  print("can write large jpg")
}, error = function(e) {
  print("can NOT write large jpg")
})
