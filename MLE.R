# Import PCA.R to use the functions that extract data
source("PCA.R")

numComponents = 10	# Number of components used to calculate MLE

# Function that sorts out given data by numbers (0-9) and puts them into the
# respective image collection. Also prints out the number of images within each
# digit set.
#
# TODO: Determine better way to perform this that doesn't give you the warning
# 	(number of columns of result is not a multiple of vector length) or something
# 	of that kind.

sortDigits = function(data, labels, quantity){
	sorted = 0

	num0 = data.frame()
	num1 = data.frame()
	num2 = data.frame()
	num3 = data.frame()
	num4 = data.frame()
	num5 = data.frame()
	num6 = data.frame()
	num7 = data.frame()
	num8 = data.frame()
	num9 = data.frame()

	numDigits = array(0, dim=c(10,1))

	# TODO: Figure out how to properly use the list(), data.frame(), c(), or structure()
	#	that won't give the warning and give empty results (no values entered)
	for(i in seq(1, quantity)){
		num = as.integer(labels[i])
		if(num == 0){
			num0 = rbind(num0, data[i,])
		} else if (num == 1){
			num1 = rbind(num1, data[i,])
		} else if (num == 2){
			num2 = rbind(num2, data[i,])
		} else if (num == 3){
			num3 = rbind(num3, data[i,])
		} else if (num == 4){
			num4 = rbind(num4, data[i,])
		} else if (num == 5){
			num5 = rbind(num5, data[i,])
		} else if (num == 6){
			num6 = rbind(num6, data[i,])
		} else if (num == 7){
			num7 = rbind(num7, data[i,])
		} else if (num == 8){
			num8 = rbind(num8, data[i,])
		} else {
			num9 = rbind(num9, data[i,])
		}

		if(num == 0){
			numDigits[10] = numDigits[10] + 1
		} else {
			numDigits[num] = numDigits[num] + 1
		}
	}

	print(numDigits)

	sorted = list(num1, num2, num3, num4, num5, num6, num7, num8, num9, num0)

	sorted
}

# Calculates the distribution model (using standard normal distribution model 
# for simplicity) for one particular component and data set for one particular number
computeNumberDist = function(numData){
	parameters = matrix(, ncol=ncol(numData))
	numVal = nrow(numData)
	components = prcomp(numData, scale.=T)

	for(i in numComponents){
		values = c()
		for(j in seq(1, numVal)){
			values = crossprod(values, numData[j,])
		}
		mean = mean(values)
		variance = sd(values)
		parameters = matrix(c(parameters, mean, variance), ncol=2, colnames=c("Mean","Variance"))
	}

	parameters
}

# Function that calculates MLE of image using given parameters of selected pixels
# (using normal distribution model and parameters calculated from earlier function)
calcMLE = function(params, pixelNums, image){
	# TODO: Might need to change depending on how the other two functions are
	#	changed (which might happen to get the functions to better function)

	sum = 0
	for(i in seq(1, pixelNums)){
		prob = pnorm(image[pixelNums[i]], mean=params[i,1], sd=params[i,2])
		sum = sum + log(prob)
	}
	sum
}

