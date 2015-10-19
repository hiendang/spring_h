#test3R
library(readr)
library(xgboost)
train <- read_csv("input/train.csv")
test  <- read_csv("input/test.csv")
y = train$target
test_id = test$ID

train <- read_csv("input/ptrain.csv")
test <-  read_csv("input/ptest.csv")

#train = subset(train, select=-c(target))
#test = subset(test,select=-c(ID))
gc()
removing_column <- c("ID","target","VAR_0008","VAR_0009","VAR_0010","VAR_0011","VAR_0012","VAR_0018","VAR_0019","VAR_0020","VAR_0021","VAR_0022","VAR_0023","VAR_0024","VAR_0025","VAR_0026","VAR_0027","VAR_0028","VAR_0029","VAR_0030","VAR_0031","VAR_0032","VAR_0038","VAR_0039","VAR_0040","VAR_0041","VAR_0042","VAR_0043","VAR_0044","VAR_0045","VAR_0098","VAR_0099","VAR_0106","VAR_0107","VAR_0114","VAR_0115","VAR_0117","VAR_0130","VAR_0131","VAR_0132","VAR_0138","VAR_0139","VAR_0140","VAR_0180","VAR_0181","VAR_0182","VAR_0183","VAR_0188","VAR_0189","VAR_0190","VAR_0191","VAR_0192","VAR_0193","VAR_0194","VAR_0195","VAR_0196","VAR_0197","VAR_0199","VAR_0202","VAR_0203","VAR_0207","VAR_0213","VAR_0214","VAR_0215","VAR_0216","VAR_0221","VAR_0222","VAR_0223","VAR_0229","VAR_0230","VAR_0236","VAR_0239","VAR_0246","VAR_0362","VAR_0371","VAR_0377","VAR_0378","VAR_0386","VAR_0387","VAR_0388","VAR_0392","VAR_0394","VAR_0395","VAR_0396","VAR_0399","VAR_0411","VAR_0413","VAR_0414","VAR_0428","VAR_0429","VAR_0438","VAR_0445","VAR_0456","VAR_0459","VAR_0460","VAR_0463","VAR_0470","VAR_0471","VAR_0472","VAR_0476","VAR_0478","VAR_0502","VAR_0508","VAR_0509","VAR_0521","VAR_0524","VAR_0526","VAR_0527","VAR_0529","VAR_0530","VAR_0638","VAR_0789","VAR_0840","VAR_0847","VAR_1013","VAR_1158","VAR_1198","VAR_1213","VAR_1427","VAR_1428","VAR_1588","VAR_1594","VAR_1616","VAR_1619","VAR_1656","VAR_1675","VAR_1723","VAR_1845","VAR_1848")
train=train[,!(names(train) %in% removing_column)]
test=test[,!(names(test) %in% removing_column)]
for (ti in 0:7){
	if (ti != 5) {
		tsne_data <- read_csv(paste("tsne.out.csv.",ti,sep=""))
		train = cbind(train,tsne_data[1:nrow(train),])
		test  = cbind(test ,tsne_data[(nrow(train)+1):nrow(tsne_data),])
		cat(paste("new dim is ",dim(train), "\n"))
	}
	
}
tsne_data <- read_csv("tsne.out.csv")
train = cbind(train,tsne_data[1:nrow(train),])
test  = cbind(test,tsne_data[(nrow(train)+1):nrow(tsne_data),])

cat(paste("test dim is ",dim(test),"\n"))

n <- 100000
##
for (i in 1:2){
	set.seed(i*957-5)
	h <- sample(nrow(train), 130000)
	cols <- sample(ncol(train),1000)
	val<-train[-h,cols]
	gc()
	dtrain <-train[h,cols]
	gc()
	dtrain <- xgb.DMatrix(data.matrix(dtrain), label=y[h])

	dval <- xgb.DMatrix(data.matrix(val), label=y[-h])
	gc()

	watchlist <- list(eval = dval)
	param <- list(  objective           = "binary:logistic",
				eta                 = 0.001 +i/1500,
				max_depth           = 12-i%/%2,  # changed from default of 6
				subsample           = 0.7,
				colsample_bytree    = 0.7,
				min_child_weight	= 6 -i%/%2,
				eval_metric         = "auc"
                )
	clf <- xgb.train(   params         	= param, 
                    data                = dtrain, 
                    nrounds             = n, # changed from 300
                    verbose             = 1, 
                    early.stop.round    = 100,
                    watchlist           = watchlist,
                    maximize            = TRUE)
	submission <- data.frame(ID=test_id)
	submission$target <- NA 
	for (rows in split(1:nrow(test), ceiling((1:nrow(test))/10000))) {
		submission[rows, "target"] <- predict(clf, data.matrix(test[rows,]),ntreelimit=clf$bestInd)		
	}
	cat("saving the submission file\n")
	write_csv(submission, paste("xgb5.binary_logistic.csv.",i,sep = "_"))
	set.seed(i*123-7)
	h <- sample(nrow(train), 130000)
	cols <- sample(ncol(train),1000)
	val<-train[-h,cols]
	gc()
	dtrain <-train[h,cols]
	gc()
	dtrain <- xgb.DMatrix(data.matrix(dtrain), label=y[h])

	dval <- xgb.DMatrix(data.matrix(val), label=y[-h])
	gc()
	param <- list(  objective           = "multi:softprob",#"binary:logistic", 
                # booster = "gblinear",
                eta                 = 0.002, #0.06, #0.01,
                max_depth           = 11 -i%%3,  # changed from default of 8
				num_class			= 2,
                subsample           = 0.7,
                colsample_bytree    = 0.76,
                min_child_weight    = 4,
                eval_metric         = "mlogloss"
                )

	clf <- xgb.train(   params              = param, 
						data                = dtrain, 
						nrounds             = n, #280, #125, #250, # changed from 300
						verbose             = 1, 
						early.stop.round    = 100,
						watchlist           = watchlist,
						maximize            = FALSE)


	submission <- data.frame(ID=test_id)
	submission$target <- NA 
	for (rows in split(1:nrow(test), ceiling((1:nrow(test))/10000))) {
		tmp <- predict(clf, data.matrix(test[rows,]),ntreelimit=clf$bestInd)
		tmp <- matrix(tmp, ncol=2, byrow=TRUE)
		submission[rows, "target"] <- tmp[,2]
	}


	cat("saving the submission file\n")
	write_csv(submission, paste("xgb5.multisoftprob.csv.",i,sep="_"))
}

####

