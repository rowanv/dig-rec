# makes the KNN submission


#Based on our past cross-validation we found that k=3 is optimal


results.complete <- cbind(c(2,3,4,4), c(5,5,5,5))
colnames(results.complete) <- c("ImageId", "Label")
write(results.complete, file="digits_knn_3.csv", ncolumns=1) 

?write.csv
library(FNN)

#train <- read.csv("../data/train.csv", header=TRUE)
#test <- read.csv("../data/test.csv", header=TRUE)

load("digits-train.Rda")
load("digits-test.Rda")

summary(d.test)

d.labels <- d.train[,1]
d.train <- d.train[,-1]

system.time(results <- (0:9)[knn(d.train, d.test, d.labels, k = 3, algorithm="kd_tree")])

print(results)
row.numbers<- c(1:nrow(d.test))

results.complete <- cbind(row.numbers, results)
#took 505.018 seconds with first 10,000 rows

colnames(results.complete) <- c("ImageId", "Label")

print(results.complete)

write.csv(results, file="/Users//Documents/Personal Research Projects/Datasets/digits_knn_3.csv")

#Submission - 95% accuracy
