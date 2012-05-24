getLabels = function(){
	to.read = file(file.choose(), "rb")
	magicNumber = readBin(to.read, integer(), endian = "big")
	records = readBin(to.read, integer(), endian = "big")
	labels = c()
	for(i in seq(1,records)){
		lab = readBin(to.read, raw(), 1)
		labels = c(labels, lab)
	}
	labels
}
getImageData = function(r){
	to.read = file(file.choose(), "rb")
	magicNumber = readBin(to.read, integer(), endian = "big")
	records = readBin(to.read, integer(), endian = "big")
	rows = readBin(to.read, integer(), endian = "big")
	columns = readBin(to.read, integer(), endian = "big")
	data = c()
	for(i in seq(1, min(r, records))){
		for(r in seq(1, rows)) {
			for(c in seq(1, columns)){
				data = c(data, as.integer(readBin(to.read, raw(), 1)))
			}
		}
	}
	out = matrix(data, ncol = rows*columns)
	#prcomp(out, scale. = T)
}
toImage = function(vec, name){
    imageMat = matrix(vec, nrow = 28, byrow = T)
    imageMat = imageMat - min(imageMat)
    imageMat = 1 - imageMat/(max(imageMat))
    writeJPEG(imageMat, target = name)
}
