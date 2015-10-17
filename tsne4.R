library(readr)
train <- read_csv("input/ptrain.csv")
test  <- read_csv("input/ptest.csv")
i <- 4
interval <- 200
anfang <- i*interval+1
ende <- min(ncol(train),(i+1)*interval)

gc()
removing_column <- c("ID","target")
train=train[,!(names(train) %in% removing_column)]
test=test[,!(names(test) %in% removing_column)]
train <- train[,anfang:ende]
test <- test[,anfang:ende]
gc()
d = rbind(train,test)
train <- 0
test <- 0

library(Rtsne)
tsne <- Rtsne(d, dims = 3, perplexity=30, max_iter = 2000,initial_dims = 70, check_duplicates=FALSE)
write.table(tsne$Y, file = paste("tsne.out.csv.",i,sep=""),sep=",",row.names=FALSE)

