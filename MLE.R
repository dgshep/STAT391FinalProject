# Import PCA.R to use the functions that extract data
source("PCA.R")

numComponents = 10	# Number of components used to calculate MLE

# Splits the digits into its respective digit set
splitDigits = function(data){
	sets = list()
	labels = row.names(data)

	for(i in seq(1,10)){
		images = c()
		count = 0
		for(j in seq(1, nrow(data))){
			if( (as.integer(labels[j]) == i) || (i == 10 && as.integer(labels[j]) == 0)){
				images = c(images, data[j,])
				count = count + 1
			}
		}

		if(length(images) != 0){
			sets[[i]] = matrix(images, nrow=count, byrow=T)
		}
	}

	sets
}


# Calculates the distribution model (using standard normal distribution model 
# for simplicity) for one particular component and data set for one particular number
computeNumberDist = function(x, comps){
	params = c()

	sets = splitDigits(x)

	for(i in seq(1, 10)){
		for(j in seq(1, comps)){
			params = c(params, mean(sets[[i]][,j]), sd(sets[[i]][,j]))
		}
	}

	results = matrix(params, nrow=comps*10, ncol=2, byrow=T)

	results
}

# Function that calculates MLE of image using given parameters of selected pixels
# (using normal distribution model and parameters calculated from earlier function)
calcMLE = function(params, rot, r){
	comps = nrow(params) / 10
	sums = c()	
	images = getImageData(r)

	pidata = images %*% rot

	for(image in seq(1, nrow(pidata))){
		for(i in seq(0, 9)){
			sum = 0
			for(j in seq(1, comps)){
				prob = dnorm(pidata[image,j], mean=params[(i*10) + j,1], sd=params[(i*10) + j,2])
				sum = sum + log(prob)
			}

			sums = c(sums, sum)
		}
	}

	results = matrix(sums, ncol=10, byrow=T)

	results
}

# Function that calculates MLE of image using given parameters of selected pixels
# (using multivariate distribution model)
calcMLEMulti = function(params, rot, r){
	comps = length(params[[1]]$means)
	sums = c()	
	images = getImageData(r)

	pidata = images %*% rot

	for(image in seq(1, nrow(pidata))){
		for(i in seq(1, 10)){
			# MULTIVARIATE USAGE HERE (utilizes dmvnorm from imported library package)
			sum = dmvnorm(pidata[image,1:comps], params[[i]]$means, params[[i]]$Sigma, log=T)

			sums = c(sums, sum)
		}
	}

	results = matrix(sums, ncol=10, byrow=T)

	results
}

