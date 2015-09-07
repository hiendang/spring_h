library(readr)
library(xgboost)
library(Rtsne)

set.seed(1)
train <- read_csv("input/train.csv")
test  <- read_csv("input/test.csv")
y = train$target
test_id = test$ID
train = subset(train, select=-c(ID, target))
test = subset(test,select=-c(ID))
train[train==-1] = NA
train[train==""] = NA
train[train=="[]"] = NA 
test[test==-1]=NA
test[test==""]=NA
test[test=="[]"]=NA

feature.names <- names(train)#[2:ncol(train)-1]

for (f in feature.names) {
  if (class(train[[f]])=="character") {
    levels <- unique(c(train[[f]], test[[f]]))
    train[[f]] <- as.integer(factor(train[[f]], levels=levels))
    test[[f]]  <- as.integer(factor(test[[f]],  levels=levels))
  }
}
train[is.na(train)] <- -1
test[is.na(test)]   <- -1
not_dup_index = !duplicated(lapply(train, summary))
train = train[not_dup_index]
test = test[not_dup_index]

cat("sampling train to get around 8GB memory limitations\n")
#train <- train[sample(nrow(train), 120000),]
gc()

h <- sample(nrow(train), 120000)

val<-train[-h,]
gc()

train <-train[h,]
gc()
dtrain <- xgb.DMatrix(data.matrix(train), label=y[h])

train=train[1:3,]
gc()
dval <- xgb.DMatrix(data.matrix(val), label=y[-h])
val=val[1:3,]
gc()

watchlist <- list(eval = dval)

param <- list(  objective           = "multi:softprob",#"binary:logistic", 
                # booster = "gblinear",
                eta                 = 0.015, #0.06, #0.01,
                max_depth           = 9,  # changed from default of 8
				num_class			= 2,
                subsample           = 0.8,
                colsample_bytree    = 0.7,
                min_child_weight    = 3,
                eval_metric         = "mlogloss"
                # alpha = 0.0001, 
                # lambda = 1
                )

clf <- xgb.train(   params              = param, 
                    data                = dtrain, 
                    nrounds             = 2500, #280, #125, #250, # changed from 300
                    verbose             = 1, 
                    early.stop.round    = 30,
                    watchlist           = watchlist,
                    maximize            = FALSE)


dtrain=0
gc()

dval=0
gc()

submission <- data.frame(ID=test_id)
submission$target <- NA 
for (rows in split(1:nrow(test), ceiling((1:nrow(test))/10000))) {
	tmp <- predict(clf, data.matrix(test[rows,]))
	tmp <- matrix(tmp, ncol=2, byrow=TRUE)
    submission[rows, "target"] <- tmp[,2]
}

cat("saving the submission file\n")
write_csv(submission, "test.R.xgb.multi.02.csv")

