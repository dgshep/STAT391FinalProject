getLabels = function(r){
	to.read = file(file.choose(), "rb")
	magicNumber = readBin(to.read, integer(), endian = "big")
	records = readBin(to.read, integer(), endian = "big")
	labels = c()
	for(i in seq(1,min(r, records))){
		lab = readBin(to.read, raw(), 1)
		labels = c(labels, lab)
	}
	close(to.read)
	labels
}
componentsToImage = function(mat, i){
    for(i in seq(1, i)){
        toImage(mat[,i], paste("pc", i, ".jpg", sep=""))
    }
}
getImageData = function(r){
    print("Select Image File")
	to.read = file(file.choose(), "rb")
	f = scan(to.read)
	out = matrix(f[1:(28*28*r)], nrow = r, byrow = T)
	#labels = as.character(getLabels(r))
	
	out
	# magicNumber = readBin(to.read, integer(), endian = "big")
	#  records = readBin(to.read, integer(), endian = "big")
	#  rows = readBin(to.read, integer(), endian = "big")
	#  columns = readBin(to.read, integer(), endian = "big")
	#  data = c()
	#  for(i in seq(1, min(r, records))){
	#      for(r in seq(1, rows)) {
	#          for(c in seq(1, columns)){
	#              data = c(data, as.integer(readBin(to.read, raw(), 1)))
	#          }
	#      }
	#  }
	#  close(to.read)
	#prcomp(out, scale. = T)
}
toImage = function(vec, name){
    library("jpeg")
    imageMat = matrix(vec, nrow = 28, byrow = T)
    imageMat = imageMat - min(imageMat) #Shift to zero
    imageMat = 1 - imageMat/(max(imageMat)) #Invert and normailze
    writeJPEG(imageMat, target = name)
}
plotComponents = function(mat, x, y){
    xvec = mat[,x]
    yvec = mat[,y]
    plot(xvec, yvec, pch = "", xlab = c("Principal Component ", x), ylab = c("Principal Component ", y))
    text(xvec, yvec, label = rownames(mat), col = strtoi(rownames(mat), base = 10) + 1)
}
train = function(x, comp){
    model = naiveBayes(x[,-(comp + 1):-ncol(x)], x$labels)
    model
}
addLabels = function(dat, lab){x
    x = data.frame(data = dat, labels = as.character(lab))
}
classify = function(model, newDat){
    pred = predict(model, newDat)
    cat(sum(newDat$labels==pred)/nrow(newDat))
    pred
}
analyze = function(dat, lab){
   pc = prcomp(dat)
   newdat = pc$x
      par(mfrow=c(1,2))
         plotComponents(newdat, 1, 2)
         plotComponents(dat, 736, 737)
   pc
}
data = getImageData(10000)
labels = as.character(getLabels(10000))
pca = prcomp(data)
train.labels = labels[1:9900]
test.labels = labels[9001:10000]
train.pca = addLabels(pca$x[1:9000, ], train.labels)
test.pca = addLabels(pca$x[9001:10000, ], test.labels)
pca.model = train(train.pca, 100)
pred = classify(pca.model, test.pca)

train.data = addLabels(data[1:9900, ], train.labels)
test.data = addLabels(data[9001:10000, ], test.labels)
std.model = train(train.data, 784)
pred = classify(std.model, test.data)

