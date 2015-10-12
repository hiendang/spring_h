library(readr)
library(xgboost)

set.seed(1)
## Loading data
train <- read_csv("input/train.csv")
test  <- read_csv("input/test.csv")
y = train$target
test_id = test$ID

#removing id and label from data
train = subset(train, select=-c(ID, target))
test = subset(test,select=-c(ID))
# removing NA columns
removing_column <- c("VAR_0205","VAR_0207","VAR_0213","VAR_0214","VAR_0840","VAR_0847",
"VAR_1428","VAR_0008","VAR_0009","VAR_0010","VAR_0011","VAR_0012",
"VAR_0018","VAR_0019","VAR_0020","VAR_0021","VAR_0022","VAR_0023",
"VAR_0024","VAR_0025","VAR_0026","VAR_0027","VAR_0028","VAR_0029",
"VAR_0030","VAR_0031","VAR_0032","VAR_0038","VAR_0039","VAR_0040",
"VAR_0041","VAR_0042","VAR_0043","VAR_0044","VAR_0188","VAR_0189",
"VAR_0190","VAR_0196","VAR_0197","VAR_0199","VAR_0202","VAR_0203",
"VAR_0215","VAR_0216","VAR_0221","VAR_0222","VAR_0223","VAR_0229",
"VAR_0239","VAR_1427","VAR_0246","VAR_0394","VAR_0438","VAR_0446",
"VAR_0527","VAR_0528","VAR_0530")
train=train[,!(names(train) %in% removing_column)]
test=test[,!(names(test) %in% removing_column)]
# fix city name
library(dplyr)
library(stringdist)
reviewDupes <- mutate(train, City = VAR_0200, State = VAR_0237, Zip=VAR_0241) %>% 
	select(City, State, Zip) %>%
	mutate(stateZip = paste(Zip, State, sep="_"),
         fullGeoID = paste(City, Zip, State, sep="_")) %>%
   distinct()
potentialDupes <- group_by(reviewDupes, stateZip) %>% 
  dplyr::summarise(n = n(), 
                   altName = first(City), # prettier: most common
                   altID = first(fullGeoID)) %>% 
  filter(n > 1)
dupes <- mutate(left_join(potentialDupes, reviewDupes, by="stateZip"), 
                dist=stringdist(altName, City)) %>% 
                filter(dist >= 1 & dist <= 2)
train <- mutate(train, fullGeoID = paste(VAR_0200, VAR_0241, VAR_0237, sep="_"))
train <- left_join(train, select(dupes, altName, fullGeoID), by="fullGeoID") %>%
	mutate(VAR_0200 = ifelse(is.na(altName), VAR_0200, altName)) %>%
	select(-fullGeoID, -altName)
  
test <- mutate(test, fullGeoID = paste(VAR_0200, VAR_0241, VAR_0237, sep="_"))
test <- left_join(test, select(dupes, altName, fullGeoID), by="fullGeoID") %>%
	mutate(VAR_0200 = ifelse(is.na(altName), VAR_0200, altName)) %>%
	select(-fullGeoID, -altName)


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
gc()
### Cross validation
param <- list(  objective           = "binary:logistic", 
                # booster = "gblinear",
                eta                 = 0.002,
                max_depth           = 11,  # changed from default of 6
                subsample           = 0.7,
                colsample_bytree    = 0.7,
                eval_metric         = "auc"
                # alpha = 0.0001, 
                # lambda = 1
                )

res1 <- xgb.cv(param,dtrain,2500,nfold=10,prediction=TRUE,metrics={'auc'},stratified=TRUE,verbose=1,early.stop.round=35,maximize=TRUE)
##
h <- sample(nrow(train), 130000)

val<-train[-h,]
gc()

train <-train[h,]
gc()
dtrain <- xgb.DMatrix(data.matrix(train), label=y[h])

gc()
dval <- xgb.DMatrix(data.matrix(val), label=y[-h])
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


submission <- data.frame(ID=test_id)
submission$target <- NA 
for (rows in split(1:nrow(test), ceiling((1:nrow(test))/10000))) {
	tmp <- predict(clf, data.matrix(test[rows,]),ntreelimit=clf$bestInd)
	tmp <- matrix(tmp, ncol=2, byrow=TRUE)
    submission[rows, "target"] <- tmp[,2]
}

cat("saving the submission file\n")
write_csv(submission, "multi.1.csv")


param <- list(  objective           = "binary:logistic", 
                # booster = "gblinear",
                eta                 = 0.002,
                max_depth           = 11,  # changed from default of 6
                subsample           = 0.7,
                colsample_bytree    = 0.7,
                eval_metric         = "auc"
                # alpha = 0.0001, 
                # lambda = 1
                )

clf <- xgb.train(   params              = param, 
                    data                = dtrain, 
                    nrounds             = 1500, # changed from 300
                    verbose             = 1, 
                    early.stop.round    = 20,
                    watchlist           = watchlist,
                    maximize            = TRUE)

