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
    library("e1071")
    model = naiveBayes(x[,1:comp], x$labels)
    model
}
addLabels = function(dat, lab){
    x = data.frame(data = dat, labels = as.character(lab))
}
classify = function(model, newDat){
    pred = predict(model, newDat[,1:(ncol(newDat)-1)])
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
imageReduce = function(image, pca, comp){
    for(i in 2:comp) {
        reduce = pca$rot[,1:i]%*%image[1:i]
        toImage(reduce, paste(formatC(i, width = 3, format = "d", flag = "0") ,".jpg", sep=""))
    }
}
images = 10000
data = data.frame(getImageData(images))
labels = as.character(getLabels(images))
pca = prcomp(data)
train.labels = labels[1:(images-1000)]
test.labels = labels[(images-999):images]
train.pca = addLabels(pca$x[1:(images-1000), ], train.labels)
test.pca = addLabels(pca$x[(images-999):images, ], test.labels)
pca.model = train(train.pca, 100)
pred = classify(pca.model, train.pca)
pred = classify(pca.model, test.pca)


train.data = addLabels(data[1:(images-1000), ], train.labels)
test.data = addLabels(data[(images-999):images, ], test.labels)
std.model = train(train.data, 784)
pred = classify(std.model, train.data)
pred = classify(std.model, test.data)
performance = c()
for(i in seq(2,784)){
    pca.model = train(train.pca, i);
    pred = classify(pca.model, test.pca);
    performance = c(performance, sum(test.pca$labels==pred)/nrow(test.pca));
}
