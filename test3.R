#test3R
library(readr)
library(xgboost)
train <- read_csv("input/ptrain.csv")
test  <- read_csv("input/ptest.csv")
y = train$target
test_id = test$ID
train = subset(train, select=-c(ID, target))
test = subset(test,select=-c(ID))
gc()

n <- 10
##
for (i in 1:5){
	set.seed(i*13-7)
	h <- sample(nrow(train), 130000)
	val<-train[-h,]
	gc()
	dtrain <-train[h,]
	gc()
	dtrain <- xgb.DMatrix(data.matrix(dtrain), label=y[h])

	dval <- xgb.DMatrix(data.matrix(val), label=y[-h])
	gc()

	watchlist <- list(eval = dval)
	param <- list(  objective           = "binary:logistic", 
                # booster = "gblinear",
                eta                 = 0.001,
                max_depth           = 12-i%/%2,  # changed from default of 6
                subsample           = 0.7,
                colsample_bytree    = 0.7,
				min_child_weight	= 6
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
	write_csv(submission, paste("xgb2.binary_logistic.csv.",i,sep = "_"))
	
	param <- list(  objective           = "multi:softprob",#"binary:logistic", 
                # booster = "gblinear",
                eta                 = 0.001, #0.06, #0.01,
                max_depth           = 11 - i%/%2,  # changed from default of 8
				num_class			= 2,
                subsample           = 0.8,
                colsample_bytree    = 0.66,
                min_child_weight    = 5,
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
	write_csv(submission, paste("xgb2.multisoftprob.csv.",i,sep="_"))
}

####

