library(FNN)
library(caret)
library(caTools)#splitting data

############################################
#Loading the Training and Testing Data Sets
############################################

data.dir <- "/Users/rowanvasquez/Documents/Personal\ Research\ Projects/Datasets/" 
d.test.file <- paste0(data.dir, "digits-test.csv") 
d.train.file <- paste0(data.dir, "digits-train.csv")

d.test <- read.csv(d.test.file, header = TRUE)
d.train <- read.csv(d.train.file, header = TRUE)

save(d.train, file = "digits-train.Rda")
save(d.test, file = "digits-test.Rda")


load("/Users/rowanvasquez/Downloads/digits-train.Rda")
load("/Users/rowanvasquez/Downloads/digits-test.Rda")

#Descriptive Statistics

str(d.train$label)
summary(d.train$label)
str(d.train)


#########
#Split training set into sub-training and sub-testing
set.seed(1000)

#First, worked with a tenth of the data for speed
#d.train.split <- sample.split(d.train$label, SplitRatio = 0.1)
#d.train <- subset(d.train, d.train.split == TRUE)

#Then, worked with complete data set
str(d.train)

d.train.split = sample.split(d.train$label, SplitRatio = 0.7)
d.train.tr <- subset(d.train, d.train.split == TRUE)
d.train.te <- subset(d.train, d.train.split == FALSE)


#Checking that split was done correctly
summary(d.train.tr)
summary(d.train.te)
#split looks good

#sub-training is d.train.tr
#sub testing is d.train.te


#########################
#choosing a k value#####
########################
k.vals <- c(1:30)
k.accuracy <- data.frame(k.vals, k.vals)
colnames(k.accuracy) <- c("K Values", "Accuracy")
k.accuracy

k.accuracy[which(k.accuracy[,2]==11),2]

kval.test <- function(k.accuracy, train.data){
	
	for (i in k.accuracy[,1]){
		print(i)
		cl <- train.data[,1]
		knn.res <- knn.cv(train.data[,c(2:783)], cl, k = i, algorithm = "cover_tree")
		confus.knn.res <- confusionMatrix(knn.res, train.data[,1])
		print(confus.knn.res)
		k.accuracy[which(k.accuracy[,2]==i),2] <- confus.knn.res$overall[1]
	}
}

kval.test.kdtree <- function(k.accuracy, train.data){
	
	for (i in k.accuracy[,1]){
		print(i)
		cl <- train.data[,1]
		knn.res <- knn.cv(train.data[,c(2:783)], cl, k = i, algorithm = "kd_tree")
		confus.knn.res <- confusionMatrix(knn.res, train.data[,1])
		print(confus.knn.res)
		#k.accuracy[which(k.accuracy[,2]==i),2] <- confus.knn.res$Accuracy
	}
}

kval.test(k.accuracy, d.train.tr)
#kval.test.kdtree(k.accuracy, d.train.tr)

plot(k.accuracy[,1], k.accuracy[,2])
