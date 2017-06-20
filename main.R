# libriaries 
require(foreign)
require(e1071)
require(fastICA)
require(caret)


data(iris)

setwd("/home/jesus/develop/repos/proyecto-nicandro/input-data/")
loadArff <- function(arffName){
	return(read.arff(arffName))
}

testNaive <- function(mymodel, classIndex,  testSet){
	realClass      <- testSet[, classIndex]
	predictedClass <- predict(m, testSet)
	s <- sum( realClass  == predictedClass  )
	return(s / length(realClass))
}

transformData <- function(myData, classIndex, nComp = 2){
	a <- fastICA(myData[ -classIndex ], n.comp = nComp, alg.typ = "parallel")$S
	dat <- data.frame(a)
	dat[, nComp + 1] <- myData[, classIndex]
	
	names(dat)[nComp + 1] <- names(myData)[classIndex]
	
	return(dat)

}


myArffs <- c(
				"iris.arff",
				"glass.arff",
				"unbalanced.arff",
				"ionosphere.arff",
				"diabetes.arff",
				"segment-challenge.arff",
				"segment-test.arff",
				"pima-indians-diabetes.arff",
				"sonar.arff",
				"ecoli.arff",
				"data_banknote_authentication.arff"
			)

# Precisión de cada base de datos
accuracy <- c()

# varianza de la precisión
accuracy.var <- c()

for(i in 1:length(myArffs)){
	print (myArffs[i])
	myData      <- loadArff(myArffs[i])
	classIndex  <- length(myData[1,])
	
	if (0){
		if (classIndex < 10)
			nc <- classIndex - 1
		else
			nc <- as.integer(classIndex * 0.6)

		try(
			myData <- transformData(myData, classIndex, nc)
		)  

	} else {
		#print("real")
	}
	
	classIndex  <- length(myData[1,])
	
	# Genera folds para validación cruzada
	myFolds <- createFolds(myData[, classIndex], k = 10, list = TRUE, returnTrain = FALSE)

	data.accuracy <- c()
	for (i in 1:length(myFolds)){
		# Conjunto de entrenamiento
		trainSet <-  myData[ -myFolds[[i]], ]
		
		# Conjunto de prueba
		testSet  <-  myData[  myFolds[[i]], ]
		
		# Genera modelo
		m <- naiveBayes(trainSet[, -classIndex], trainSet[, classIndex])
		# precisión de modelo
		a <- testNaive(m, classIndex, testSet)
		
		data.accuracy <- c(data.accuracy, a)
	}
	
	# Promedio de la precisión con 10 folds
	accuracy     <- c(accuracy,    mean(data.accuracy))
	accuracy.var <- c(accuracy.var, var(data.accuracy))
	
}

names(accuracy)     <- myArffs
names(accuracy.var) <- myArffs

print(accuracy)
print(accuracy.var)

print(mean(accuracy))






